use std::marker::PhantomData;

/// Try to iterate through all the elements, stop on error, otherwise unwrap the element
pub struct TryIter<I, T, E> {
    iter: I,
    err: Result<(), E>,
    _phantom: PhantomData<T>,
}

impl<I, T, E> TryIter<I, T, E>
where
    E: Clone,
{
    #[inline]
    pub fn new(iter: I) -> TryIter<I, T, E>
    where
        I: Iterator<Item = Result<T, E>>,
    {
        TryIter {
            iter,
            err: Ok(()),
            _phantom: PhantomData,
        }
    }

    #[inline]
    pub fn err(&self) -> Result<(), E> {
        self.err.clone()
    }
}

impl<I: Iterator<Item = Result<T, E>>, T, E> Iterator for TryIter<I, T, E> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        debug_assert!(self.err.is_ok()); // not need to check it in runtime
        match self.iter.next()? {
            Ok(result) => Some(result),
            Err(err) => {
                self.err = Err(err);
                None
            }
        }
    }
}
