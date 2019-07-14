use std::fs;
use std::path::PathBuf;
use insta::assert_snapshot_matches;

use csharpbindgen::{
    Builder,
    CSAccess
};

fn run_example_test<F: FnOnce(Builder) -> Builder>(name: &'static str, build: F) {
    let builder = build(Builder::new("MyDll", load_example(name)));
    assert_snapshot_matches!(name, builder.generate().unwrap());
}

fn load_example(name: &'static str) -> String {
    let mut path = PathBuf::new();
    path.push("tests");
    path.push("rust");
    path.push(format!("{}.rs", name));
    fs::read_to_string(path).unwrap()
}

#[test]
fn test_main_example() {
    run_example_test("main_example", |b|
        b.class_name("MyStuff")
         .ignore(&["ignore_*", "IGNORE_*", "Ignore*"])
         .access("public_func", CSAccess::Public)
         .access("PublicStruct", CSAccess::Public)
    );
}

#[test]
fn test_safe_handles() {
    run_example_test("safe_handles_example", |b| b.use_safe_handles());
}
