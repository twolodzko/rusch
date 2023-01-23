use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Env<T>(Option<Rc<RefCell<EnvContainer<T>>>>);

#[derive(Debug, PartialEq)]
struct EnvContainer<T> {
    local: HashMap<String, T>,
    parent: Env<T>,
}

impl<T> Env<T>
where
    T: Clone,
{
    /// Create root environment
    pub fn new() -> Self {
        Env(Some(Rc::new(RefCell::new(EnvContainer {
            local: HashMap::new(),
            parent: Env(None),
        }))))
    }

    /// Create environment inheriting from the current one
    #[inline]
    pub fn branch(&self) -> Self {
        Env(Some(Rc::new(RefCell::new(EnvContainer {
            local: HashMap::new(),
            parent: self.clone(),
        }))))
    }

    /// Save key-value pair to the environment
    #[inline]
    pub fn insert(&mut self, key: &String, val: T) {
        let mut env = self.unwrap().borrow_mut();
        env.local.insert(key.to_string(), val);
    }

    /// Get (recursively) value associated with the key if available
    #[inline]
    pub fn get(&self, key: &String) -> Option<T> {
        let env = self.unwrap().borrow();
        if let Some(val) = env.local.get(&key.clone()) {
            Some(val.clone())
        } else {
            match env.parent {
                Env(None) => None,
                _ => env.parent.get(key),
            }
        }
    }

    /// Find (recursively) the enviroment that has some value associated with the key
    #[inline]
    pub fn find_env(&self, key: &String) -> Option<Self> {
        let env = self.unwrap().borrow();
        if env.local.get(key).is_some() {
            Some(self.clone())
        } else {
            match env.parent {
                Env(None) => None,
                _ => env.parent.find_env(key),
            }
        }
    }

    /// Extract the internal Env container
    #[inline]
    fn unwrap(&self) -> &Rc<RefCell<EnvContainer<T>>> {
        self.0.as_ref().unwrap()
    }
}

impl<const N: usize, T> From<[(&str, T); N]> for Env<T>
where
    T: Clone,
{
    fn from(records: [(&str, T); N]) -> Self {
        let mut env: Env<T> = Env::new();
        for (key, val) in records.iter() {
            env.insert(&String::from(*key), val.clone());
        }
        env
    }
}

impl<T> Default for Env<T>
where
    T: Clone,
{
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::Env;
    use crate::types::Sexpr;

    #[test]
    fn insert() {
        let mut env = Env::<Sexpr>::new();
        assert_eq!(env.get(&String::from("foo")), None);

        env.insert(&String::from("foo"), Sexpr::Integer(42));
        assert_eq!(env.get(&String::from("foo")), Some(Sexpr::Integer(42)));

        env.insert(&String::from("bar"), Sexpr::True);
        assert_eq!(env.get(&String::from("foo")), Some(Sexpr::Integer(42)));
        assert_eq!(env.get(&String::from("bar")), Some(Sexpr::True));

        env.insert(&String::from("foo"), Sexpr::String(String::from("ok?")));
        assert_eq!(
            env.get(&String::from("foo")),
            Some(Sexpr::String(String::from("ok?")))
        );
    }

    #[test]
    fn branch() {
        let mut root = Env::new();
        root.insert(&String::from("foo"), Sexpr::Integer(42));

        {
            let mut local = root.branch();
            assert_eq!(root.get(&String::from("foo")), Some(Sexpr::Integer(42)));
            assert_eq!(local.get(&String::from("foo")), Some(Sexpr::Integer(42)));

            local.insert(&String::from("foo"), Sexpr::True);
            assert_eq!(root.get(&String::from("foo")), Some(Sexpr::Integer(42)));
            assert_eq!(local.get(&String::from("foo")), Some(Sexpr::True));
        }

        assert_eq!(root.get(&String::from("foo")), Some(Sexpr::Integer(42)));
    }

    #[test]
    fn find_env() {
        let mut root = Env::new();
        root.insert(&String::from("foo"), &Sexpr::Integer(42));

        assert_eq!(root.find_env(&String::from("foo")), Some(root.clone()));

        let local1 = root.branch();
        assert_eq!(local1.find_env(&String::from("foo")), Some(root.clone()));

        let local2 = local1.branch();
        assert_eq!(local2.find_env(&String::from("foo")), Some(root.clone()));
    }

    #[test]
    fn from_records() {
        let env = Env::from([
            ("foo", Sexpr::Integer(1)),
            ("bar", Sexpr::symbol("hello")),
            ("baz", Sexpr::True),
        ]);
        assert_eq!(env.get(&String::from("foo")), Some(Sexpr::Integer(1)));
        assert_eq!(env.get(&String::from("bar")), Some(Sexpr::symbol("hello")));
        assert_eq!(env.get(&String::from("baz")), Some(Sexpr::True));
        assert_eq!(env.get(&String::from("WRONG")), None);
    }
}
