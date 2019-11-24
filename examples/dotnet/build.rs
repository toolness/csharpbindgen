use csharpbindgen::CSAccess;
use std::env;
use std::fs;
use std::path::PathBuf;

const API_RS: [&'static str; 2] = ["src", "api.rs"];
type PathParts = [&'static str];

fn read_file(path_parts: &PathParts) -> String {
    let path = path_from_cwd(path_parts);

    if !path.exists() {
        panic!("Expected file to exist: {}", path.to_string_lossy());
    }

    if let Ok(code) = fs::read_to_string(&path) {
        code
    } else {
        panic!("Unable to read {}!", path.to_string_lossy())
    }
}

fn path_from_cwd(parts: &PathParts) -> PathBuf {
    let mut pathbuf = env::current_dir().unwrap();
    for part in parts.iter() {
        pathbuf.push(part);
    }
    pathbuf
}

fn write_if_changed(path_parts: &PathParts, content: &String) {
    let path = path_from_cwd(&path_parts);

    if has_content_changed(&path, &content) {
        println!("Writing {}.", path_parts.join("/"));

        fs::write(path, content).unwrap();
    }
}

fn has_content_changed(path: &PathBuf, new_content: &String) -> bool {
    if path.exists() {
        let curr_content = fs::read_to_string(path.clone()).unwrap();
        if curr_content == *new_content {
            return false;
        }
    }
    true
}

fn build_csharp_code() {
    let srcfile = API_RS;
    let code = read_file(&srcfile);
    let bindings_result = csharpbindgen::Builder::new("testdotnet", code)
        .class_name("TestDotNet")
        .generate();

    match bindings_result {
        Err(err) => {
            println!(
                "Unable to generate C# code from {}.\n{}.",
                srcfile.join("/"),
                err
            );
            std::process::exit(1);
        }
        Ok(bindings_code) => {
            write_if_changed(&["cs", "TestDotNet.cs"], &bindings_code);
        }
    }
}

pub fn main() {
    build_csharp_code();
}
