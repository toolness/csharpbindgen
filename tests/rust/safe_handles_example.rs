pub type MyOpaqueRef = *mut MyOpaqueStruct;

pub unsafe extern "C" fn blarg(c: MyOpaqueRef) -> u8 { 120 }
