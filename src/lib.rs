use std::{
    cell::{Cell, RefCell},
    collections::VecDeque,
};

#[derive(Clone, Copy)]
pub struct Gc<T: Trace> {
    pub(crate) inner: *mut GcObject<T>,
}
pub type TraceFunc = fn(*mut u8);
pub type DeleteFunc = fn(*mut u8);

pub struct GcHeader {
    pub(crate) next: *mut GcHeader,
    pub(crate) mark: bool,
    pub(crate) trace: TraceFunc,
    pub(crate) delete: DeleteFunc,
    pub(crate) data: *mut u8,
}
pub struct GcObject<T> {
    pub(crate) header: GcHeader,
    pub(crate) data: T,
}
pub trait Trace {
    fn trace(&self);
}
struct GcContext {
    head: Cell<*mut GcHeader>,
    queue: RefCell<VecDeque<*mut GcHeader>>,
}
thread_local! {
    static GC: GcContext = GcContext {
        head: Cell::new(std::ptr::null_mut()),
        queue: RefCell::new(VecDeque::new()),
    };
}
pub fn gc_alloc<T>(data: T, trace_fn: TraceFunc, delete_func: DeleteFunc) -> *mut GcObject<T> {
    let gc_head = GC.with(|gc| gc.head.get());
    let object = Box::into_raw(Box::new(GcObject {
        header: GcHeader {
            next: gc_head,
            mark: false,
            trace: trace_fn,
            delete: delete_func,
            data: std::ptr::null_mut(),
        },
        data,
    }));
    unsafe {
        (*object).header.data = (&mut (*object).data) as *mut T as *mut u8;
        GC.with(|gc| gc.head.set(&mut (*object).header));
    }

    object
}
fn trace_impl<T: Trace>(data: *mut u8) {
    let obj = unsafe { &*(data as *const T) };
    obj.trace();
}
fn delete_impl<T>(data: *mut u8) {
    let obj = unsafe { Box::from_raw(data as *mut T) };
    drop(obj);
}
pub unsafe fn trace_object(header: *mut GcHeader) {
    let header = &mut *header;
    if header.mark {
        return;
    }
    header.mark = true;
    GC.with(|gc| gc.queue.borrow_mut().push_back(header));
}
pub fn gc_new<T: Trace>(data: T) -> Gc<T> {
    Gc {
        inner: gc_alloc(data, trace_impl::<T>, delete_impl::<T>),
    }
}
pub fn mark_root<T: Trace>(gc: Gc<T>) {
    unsafe {
        let header = &mut (*gc.inner).header;
        if !header.mark {
            header.mark = true;
            GC.with(|gc| gc.queue.borrow_mut().push_back(header));
        }
    }
}
pub unsafe fn clear_marks() {
    GC.with(|gc| {
        let mut header = gc.head.get();
        while !header.is_null() {
            (*header).mark = false;
            header = (*header).next;
        }
    });
}
pub unsafe fn collect() {
    GC.with(|gc| loop {
        let mut queue = gc.queue.borrow_mut();
        if queue.is_empty() {
            break;
        }
        let header = queue.pop_front().unwrap();
        drop(queue);
        ((*header).trace)((*header).data);
    })
}
pub unsafe fn trace_and_collect(f: impl FnOnce()) {
    clear_marks();
    f();
    collect();
}
