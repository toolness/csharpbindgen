use insta::assert_snapshot_matches;

use csharpbindgen::{
    Builder,
    CSAccess
};

#[test]
fn test_it_works() {
    let rust_code = r#"
        pub const BOOP: u8 = 1;
        pub const IGNORE_THIS_CONST: u8 = 2;

        #[repr(C)]
        pub struct MyStruct {
            pub foo: f32,
            pub bar: f32
        }

        #[repr(C)]
        pub struct IgnoreThisStruct { pub foo: f32 }

        pub type MyOpaqueRef = *mut MyOpaqueStruct;

        pub unsafe extern "C" fn public_func() {}

        #[repr(C)]
        pub struct PublicStruct {
            pub bop: i32,
        }

        fn unexported_func() {}

        pub unsafe extern "C" fn blarg(
            a: i32,
            b: *const MyStruct,
            c: MyOpaqueRef
        ) -> u8 { 120 }

        pub unsafe extern "C" fn ignore_this_func(a: i32) -> u8 { 120 }
    "#;

    let code = Builder::new("MyDll", String::from(rust_code))
        .class_name("MyStuff")
        .ignore(&["ignore_*", "IGNORE_*", "Ignore*"])
        .access("public_func", CSAccess::Public)
        .access("PublicStruct", CSAccess::Public)
        .generate()
        .unwrap();

    assert_snapshot_matches!("main_example", code);
}
