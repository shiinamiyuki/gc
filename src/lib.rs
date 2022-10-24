use parking_lot::{Mutex, RwLock};
use std::{
    cell::{Cell, RefCell},
    collections::{BTreeMap, HashMap, LinkedList, VecDeque},
    ffi::CString,
    fmt::Formatter,
    hash::Hash,
    ops::Deref,
    ptr::NonNull,
    rc::Rc,
    sync::atomic::{AtomicPtr, Ordering},
};
#[repr(C)]
pub struct Gc<T> {
    pub(crate) inner: *mut GcObject<T>,
}
unsafe impl<T: Send> Send for GcObject<T> {}
unsafe impl<T: Sync> Sync for GcObject<T> {}
unsafe impl<T: Send> Send for Gc<T> {}
unsafe impl<T: Sync> Sync for Gc<T> {}
impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Self { inner: self.inner }
    }
}
impl<T> Copy for Gc<T> {}
impl<T: Trace> Gc<T> {
    pub fn new(value: T) -> Self {
        gc_new(value)
    }
}
impl<T> Gc<T> {
    pub fn into_raw(self) -> *mut GcObject<T> {
        self.inner
    }
    pub unsafe fn from_raw(ptr: *mut GcObject<T>) -> Self {
        Self { inner: ptr }
    }
    pub fn eq(a: Self, b: Self) -> bool {
        a.inner == b.inner
    }
    pub unsafe fn get_mut(gc: &Gc<T>) -> &mut T {
        &mut (*gc.inner).data
    }
    pub fn set_root(gc: Self) {
        unsafe {
            mark_root(&mut (*gc.inner).header, true);
        }
    }
    pub fn unset_root(gc: Self) {
        unsafe {
            mark_root(&mut (*gc.inner).header, false);
        }
    }
}
impl<T> AsRef<T> for Gc<T> {
    fn as_ref(&self) -> &T {
        unsafe {
            let inner = &*self.inner;
            &inner.data
        }
    }
}
impl<T> Deref for Gc<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}
pub type TraceFunc = fn(*mut u8);
pub type DeleteFunc = fn(*mut u8);

#[repr(C)]
pub struct GcHeader {
    pub(crate) data: *mut u8,
    pub(crate) next: AtomicPtr<GcHeader>,
    pub(crate) trace: TraceFunc,
    pub(crate) delete: DeleteFunc,
    pub(crate) mark: bool,
    pub(crate) root: bool,
}
#[repr(C)]
pub struct GcObject<T> {
    pub(crate) header: GcHeader,
    pub(crate) data: T,
}

pub trait Trace {
    fn trace(&self);
}
pub struct GcContext {
    head: AtomicPtr<GcHeader>,
    queue: VecDeque<*mut GcHeader>,
}
pub struct GcContextPtr(NonNull<GcContext>);
unsafe impl Send for GcContextPtr {}
unsafe impl Sync for GcContextPtr {}

static GC: RwLock<Option<GcContextPtr>> = RwLock::new(None);

pub fn gc_alloc<T>(data: T, trace_fn: TraceFunc) -> *mut GcObject<T> {
    let object = Box::into_raw(Box::new(GcObject {
        header: GcHeader {
            next: AtomicPtr::new(std::ptr::null_mut()),
            mark: false,
            trace: trace_fn,
            delete: delete_impl::<T>,
            data: std::ptr::null_mut(),
            root: false,
        },
        data,
    }));
    unsafe {
        (*object).header.data = (&mut (*object).data) as *mut T as *mut u8;
    }

    object
}
pub fn gc_append_object(object: *mut GcHeader) {
    let gc = GC.read();
    let gc = unsafe { gc.as_ref().unwrap().0.as_ref() };
    loop {
        let cur = gc.head.load(Ordering::Relaxed);
        match gc
            .head
            .compare_exchange_weak(cur, object, Ordering::SeqCst, Ordering::Relaxed)
        {
            Ok(_) => {
                unsafe {
                    (*object).next.store(cur, Ordering::SeqCst);
                }
                break;
            }
            Err(_) => {}
        }
    }
}
fn trace_impl<T: Trace>(data: *mut u8) {
    let obj = unsafe { &*(data as *const T) };
    obj.trace();
}
fn delete_impl<T>(data: *mut u8) {
    unsafe {
        let object = data as *mut GcObject<T>;
        drop(Box::from_raw(object));
    }
}
pub unsafe fn mark_root(header: *mut GcHeader, root: bool) {
    (*header).root = root;
}
pub unsafe fn trace_object(header: *mut GcHeader) {
    let header = &mut *header;
    if header.mark {
        return;
    }
    header.mark = true;
    {
        let mut gc = GC.write();
        let gc = gc.as_mut().unwrap().0.as_mut();
        gc.queue.push_back(header);
    }
}
pub fn gc_new<T: Trace>(data: T) -> Gc<T> {
    let object = gc_alloc(data, trace_impl::<T>);
    unsafe {
        gc_append_object(&mut (*object).header);
    }
    Gc { inner: object }
}

unsafe fn _clear_marks() {
    let mut gc = GC.write();
    let gc = gc.as_mut().unwrap().0.as_mut();
    let mut header = gc.head.load(Ordering::Relaxed);
    while !header.is_null() {
        let h = &mut *header;
        h.mark = false;
        if h.root {
            gc.queue.push_back(header);
        }
        header = h.next.load(Ordering::Relaxed);
    }
}

unsafe fn _collect() {
    {
        let mut queue = VecDeque::new();
        loop {
            if queue.is_empty() {
                let mut gc = GC.write();
                let gc = gc.as_mut().unwrap().0.as_mut();
                queue = std::mem::replace(&mut gc.queue, VecDeque::new());
            }
            if queue.is_empty() {
                break;
            }
            let header = queue.pop_front().unwrap();
            ((*header).trace)((*header).data);
        }
    }
    let mut gc = GC.write();
    let gc = gc.as_mut().unwrap().0.as_mut();
    let mut cur = gc.head.load(Ordering::Relaxed);
    let mut prev: *mut GcHeader = std::ptr::null_mut();
    while !cur.is_null() {
        let next = (*cur).next.load(Ordering::Relaxed);
        if !(*cur).mark {
            ((*cur).delete)(cur as *mut u8);
            if let Some(prev) = prev.as_mut() {
                (*prev).next.store(next, Ordering::SeqCst);
            } else {
                gc.head.store(next, Ordering::SeqCst);
            }
        } else {
            prev = cur;
        }
        cur = next;
    }
}
static LOCK: Mutex<()> = Mutex::new(());
pub unsafe fn collect() {
    let _lk = LOCK.lock();
    _clear_marks();
    _collect();
}

macro_rules! impl_trivial {
    ($($t:ty),*) => {
        $(
            impl Trace for $t {
                fn trace(&self) {}
            }
        )*
    };
}
impl_trivial!(
    bool,
    u8,
    u16,
    u32,
    u64,
    i8,
    i16,
    i32,
    i64,
    f32,
    f64,
    char,
    String,
    CString,
    ()
);

impl<T: Trace + Copy> Trace for Cell<T> {
    fn trace(&self) {
        self.get().trace();
    }
}
impl<T: Trace> Trace for RefCell<T> {
    fn trace(&self) {
        self.borrow().trace();
    }
}
impl<T: Trace> Trace for Vec<T> {
    fn trace(&self) {
        for item in self {
            item.trace();
        }
    }
}
impl<T: Trace> Trace for LinkedList<T> {
    fn trace(&self) {
        for item in self {
            item.trace();
        }
    }
}
impl<T: Trace> Trace for Box<T> {
    fn trace(&self) {
        self.as_ref().trace();
    }
}
impl<T: Trace> Trace for Option<T> {
    fn trace(&self) {
        if let Some(item) = self {
            item.trace();
        }
    }
}
impl<T: Trace> Trace for Rc<T> {
    fn trace(&self) {
        self.as_ref().trace();
    }
}
impl Trace for &dyn Trace {
    fn trace(&self) {
        (*self).trace();
    }
}
impl<K: Trace, V: Trace> Trace for HashMap<K, V> {
    fn trace(&self) {
        for (k, v) in self {
            k.trace();
            v.trace();
        }
    }
}
impl<K: Trace, V: Trace> Trace for BTreeMap<K, V> {
    fn trace(&self) {
        for (k, v) in self {
            k.trace();
            v.trace();
        }
    }
}

impl<T: Trace> Trace for Gc<T> {
    fn trace(&self) {
        unsafe {
            trace_object(&mut (*self.inner).header);
        }
    }
}
impl<T: Trace> Trace for &[T] {
    fn trace(&self) {
        self.iter().for_each(|item| item.trace());
    }
}
impl<T: Trace> Trace for &mut [T] {
    fn trace(&self) {
        self.iter().for_each(|item| item.trace());
    }
}
impl<T: Trace> Trace for &T {
    fn trace(&self) {
        (*self).trace();
    }
}
impl<T: PartialEq> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}
impl<T: Eq> Eq for Gc<T> {}
use std::fmt::Debug;

use serde::{Serialize, Serializer};
impl<T: Debug> Debug for Gc<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.as_ref().fmt(f)
    }
}
impl<T: Hash> Hash for Gc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state);
    }
}
impl<T: Serialize> Serialize for Gc<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_ref().serialize(serializer)
    }
}
pub fn create_context() -> *mut GcContext {
    Box::into_raw(Box::new(GcContext {
        head: AtomicPtr::new(std::ptr::null_mut()),
        queue: VecDeque::new(),
    }))
}
pub unsafe fn destroy_context() {
    let context = Box::from_raw(context());
    drop(context);
    *GC.write() = None;
}
pub fn context() -> *mut GcContext {
    let gc = GC.read();
    gc.as_ref()
        .map(|gc| gc.0.as_ptr())
        .unwrap_or(std::ptr::null_mut())
}
pub fn set_context(ctx: *mut GcContext) {
    let mut gc = GC.write();
    assert!(gc.is_none());
    *gc = Some(GcContextPtr(NonNull::new(ctx).unwrap()));
}
#[cfg(test)]
mod test {
    use super::*;
    #[derive(Debug)]
    struct Foo {
        data: Rc<Cell<usize>>,
        next: Cell<Option<Gc<Foo>>>,
    }
    impl Trace for Foo {
        fn trace(&self) {
            self.next.trace();
        }
    }
    impl Drop for Foo {
        fn drop(&mut self) {
            self.data.set(self.data.get() + 1);
        }
    }
    static TEST_LOCK: Mutex<()> = Mutex::new(());
    #[test]
    fn basic() {
        let _lk = TEST_LOCK.lock();
        if context().is_null() {
            set_context(create_context());
        }
        let data = Rc::new(Cell::new(0));
        let foo = gc_new(Foo {
            data: data.clone(),
            next: Cell::new(None),
        });
        foo.next.set(Some(foo));
        std::mem::drop(foo);
        unsafe {
            collect();
        }
        assert_eq!(data.get(), 1, "{:?}", foo);
        unsafe {
            destroy_context();
        }
    }
    #[test]
    fn basic2() {
        let _lk = TEST_LOCK.lock();
        if context().is_null() {
            set_context(create_context());
        }
        let data = Rc::new(Cell::new(0));
        let foo = gc_new(Foo {
            data: data.clone(),
            next: Cell::new(None),
        });
        let bar = gc_new(Foo {
            data: data.clone(),
            next: Cell::new(Some(foo)),
        });
        foo.next.set(Some(bar));
        Gc::set_root(foo);
        assert_eq!(data.get(), 0);
        unsafe {
            destroy_context();
        }
    }
}
