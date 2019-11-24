use insta::assert_snapshot_matches;
use std::fs;
use std::path::PathBuf;

use csharpbindgen::{Builder, CSAccess};

fn load_example(name: &'static str) -> String {
    let mut path = PathBuf::new();
    path.push("tests");
    path.push("rust");
    path.push(format!("{}.rs", name));
    fs::read_to_string(path).unwrap()
}

#[test]
fn test_main_example() {
    let example_name = "main_example";
    let code = Builder::new("MyDll", load_example(example_name))
        .class_name("MyStuff")
        .ignore(&["ignore_*", "IGNORE_*", "Ignore*"])
        .access("public_func", CSAccess::Public)
        .access("PublicStruct", CSAccess::Public)
        .generate()
        .unwrap();

    assert_snapshot_matches!(example_name, code);
}
