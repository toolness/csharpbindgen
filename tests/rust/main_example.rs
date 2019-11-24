pub const BOOP: u8 = 1;
pub const IGNORE_THIS_CONST: u8 = 2;

#[repr(C)]
pub struct MyStruct {
    pub foo: f32,
    pub bar: f32,
}

#[repr(C)]
pub struct IgnoreThisStruct {
    pub foo: f32,
}

pub type MyOpaqueRef = *mut MyOpaqueStruct;

pub unsafe extern "C" fn public_func() {}

#[repr(C)]
pub struct PublicStruct {
    pub bop: i32,
}

fn unexported_func() {}

pub unsafe extern "C" fn blarg(a: i32, b: *const MyStruct, c: MyOpaqueRef) -> u8 {
    120
}

pub unsafe extern "C" fn ignore_this_func(a: i32) -> u8 {
    120
}

// test primitive types
pub extern "C" fn primitive_typeck(
    _v1: u8,
    _v2: i8,
    _v3: u16,
    _v4: i16,
    _v5: u32,
    _v6: i32,
    _v7: u64,
    _v8: i64,
    _v9: usize,
    _v10: isize,
) -> u8 {
    0
}

pub type CallbackFn = extern "C" fn(v: u8) -> u8;

pub unsafe extern "C" fn fn_with_callback(v: u8, cb: CallbackFn) -> u8 {
    cb(v)
}

pub unsafe extern "C" fn fn_with_callback_ptr(v: u8, cb: *const CallbackFn) -> u8 {
    (*cb)(v)
}
