pub type BinaryFn = extern "C" fn(a: u8, b: u8) -> u8;

#[no_mangle]
pub unsafe extern "C" fn apply_bin_fn(a: u8, b: u8, f: BinaryFn) -> u8 {
    f(a, b)
}
