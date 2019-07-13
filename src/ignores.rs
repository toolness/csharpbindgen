use std::collections::BTreeSet;

pub struct Ignores {
    exact: BTreeSet<String>,
    prefixes: Vec<String>
}

impl Ignores {
    pub fn new() -> Self {
        Ignores {
            exact: BTreeSet::new(),
            prefixes: vec![],
        }
    }

    pub fn add_static_array(&mut self, ignore: &[&str]) {
        for name in ignore.iter() {
            if (*name).ends_with("*") {
                let substr = &(*name)[..(*name).len()-1];
                self.prefixes.push(String::from(substr));
            }
            self.exact.insert(String::from(*name));
        }
    }

    pub fn ignore<T: AsRef<str>>(&self, value: T) -> bool {
        let s = value.as_ref();

        if self.exact.contains(s) {
            return true;
        }

        for prefix in self.prefixes.iter() {
            if s.starts_with(prefix) {
                return true;
            }
        }

        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn from_static_array(ignore: &[&str]) -> Ignores {
        let mut result = Ignores::new();
        result.add_static_array(ignore);
        result
    }

    #[test]
    fn test_it_works_with_exact_matches() {
        let ig = from_static_array(&["boop"]);
        assert_eq!(ig.ignore("boop"), true);
        assert_eq!(ig.ignore("boopy"), false);
    }

    #[test]
    fn test_it_works_with_prefixes() {
        let ig = from_static_array(&["boop*"]);
        assert_eq!(ig.ignore("boop"), true);
        assert_eq!(ig.ignore("boopy"), true);
        assert_eq!(ig.ignore("funkyboop"), false);
    }
}
