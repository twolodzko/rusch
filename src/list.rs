// The code is adapted from the Chapter 3 of "Learning Rust With Entirely Too Many Linked Lists"
// https://rust-unofficial.github.io/too-many-lists/second-final.html
// https://github.com/rust-unofficial/too-many-lists/blob/master/lists/src/third.rs

use std::convert::From;
use std::rc::Rc;

#[derive(PartialEq, Debug, Clone)]
pub struct List<T> {
    head: MaybePair<T>,
}

type MaybePair<T> = Option<Rc<Pair<T>>>;

#[derive(PartialEq, Debug)]
struct Pair<T> {
    this: T,
    next: MaybePair<T>,
}

impl<T> Pair<T> {
    #[inline]
    fn new(this: T, next: MaybePair<T>) -> MaybePair<T> {
        Some(Rc::new(Pair { this, next }))
    }
}

impl<T> List<T> {
    #[inline]
    pub fn empty() -> Self {
        List { head: None }
    }

    #[inline]
    pub fn push_front(&self, elem: T) -> List<T> {
        List {
            head: Pair::new(elem, self.head.clone()),
        }
    }

    /// Reverse the list
    pub fn rev(&self) -> Self
    where
        T: Clone,
    {
        let mut head: MaybePair<T> = None;
        for elem in self.iter() {
            head = Pair::new(elem.clone(), head);
        }
        List { head }
    }

    /// Extract first element of the list
    #[inline]
    pub fn head(&self) -> Option<&T> {
        self.head.as_ref().map(|pair| &pair.this)
    }

    /// Extract list containing everyghing but the first element
    #[inline]
    pub fn tail(&self) -> Option<List<T>> {
        let next = self.head.as_ref().and_then(|head| head.next.clone())?;
        Some(List { head: Some(next) })
    }

    #[inline]
    pub fn has_next(&self) -> bool {
        match self.head.as_ref() {
            None => false,
            Some(head) => head.next.is_some(),
        }
    }

    #[inline]
    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            next: self.head.as_deref(),
        }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }
}

impl<T> From<Vec<T>> for List<T> {
    fn from(v: Vec<T>) -> Self {
        let mut list = List::empty();
        for elem in v.into_iter().rev() {
            list = list.push_front(elem);
        }
        list
    }
}

impl<T> Drop for List<T> {
    fn drop(&mut self) {
        let mut head = self.head.take();
        while let Some(pair) = head {
            if let Ok(mut pair) = Rc::try_unwrap(pair) {
                head = pair.next.take();
            } else {
                break;
            }
        }
    }
}

impl<T> FromIterator<T> for List<T>
where
    T: Clone,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut list = List::empty();
        for elem in iter {
            list = list.push_front(elem);
        }
        list.rev()
    }
}

#[derive(Clone)]
pub struct Iter<'a, T> {
    next: Option<&'a Pair<T>>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.map(|pair| {
            self.next = pair.next.as_deref();
            &pair.this
        })
    }
}

impl<T> Default for List<T> {
    fn default() -> Self {
        Self::empty()
    }
}

#[cfg(test)]
mod tests {
    use super::List;

    #[test]
    fn empty() {
        assert_eq!(List::<i64>::empty(), List { head: None })
    }

    #[test]
    fn default() {
        assert_eq!(List::<i64>::default(), List::empty())
    }

    #[test]
    fn head_and_tail() {
        let list = List::from(vec![1, 2, 3]);

        assert_eq!(list.head(), Some(&1));
        let list = list.tail().unwrap();

        assert_eq!(list.head(), Some(&2));
        let list = list.tail().unwrap();

        assert_eq!(list.head(), Some(&3));
        assert_eq!(list.tail(), None);
    }

    #[test]
    fn push_front() {
        let list = List::empty();
        let list = list.push_front(1);
        let list = list.push_front(2);
        let list = list.push_front(3);

        assert_eq!(list, List::from(vec![3, 2, 1]));

        let list = List::from(vec![2, 3]);
        assert_eq!(list.push_front(1), List::from(vec![1, 2, 3]));
    }

    #[test]
    fn mutability() {
        let list1 = List::empty();
        let list1 = list1.push_front(1);
        assert_eq!(list1.head(), Some(&1));

        let list2 = list1.push_front(2);
        let list3 = list1.push_front(3);

        assert_eq!(list1.head(), Some(&1));
        assert_eq!(list2.head(), Some(&2));
        assert_eq!(list3.head(), Some(&3));
    }

    #[test]
    fn iter() {
        let list = List::from(vec![1, 2, 3]);

        let mut iter = list.iter();
        assert_eq!(iter.next(), Some(&1));
        assert_eq!(iter.next(), Some(&2));
        assert_eq!(iter.next(), Some(&3));
        assert_eq!(iter.next(), None);

        // Was not mutated
        assert_eq!(list.head(), Some(&1));

        assert_eq!(list.iter().collect::<Vec<&i64>>(), vec![&1, &2, &3]);
    }

    #[test]
    fn rev() {
        assert_eq!(List::<i64>::empty().rev(), List::empty());

        let list = List::from(vec![1, 2, 3]);
        assert_eq!(list.rev(), List::from(vec![3, 2, 1]));

        // Didn't mutate
        assert_eq!(list, List::from(vec![1, 2, 3]));
    }

    #[test]
    fn clone() {
        let list = List::from(vec![1, 2, 3]);
        assert_eq!(list, list.clone());
    }

    #[test]
    fn has_next() {
        assert!(!List::<i64>::empty().has_next());
        assert!(List::from(vec![1, 2, 3]).has_next());
    }
}
