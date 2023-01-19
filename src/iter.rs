/// Try to iterate through all the elements, stop on error, otherwise unwrap the element
pub struct TryIter<I, T, E> {
    pub iter: I,
    pub err: Option<Result<T, E>>,
}

impl<I, T, E> TryIter<I, T, E>
where
    T: Clone,
    E: Clone,
{
    #[inline]
    pub fn new(iter: I) -> TryIter<I, T, E>
    where
        I: Iterator<Item = Result<T, E>>,
    {
        TryIter { iter, err: None }
    }

    #[inline]
    pub fn err(&self) -> Option<Result<T, E>> {
        self.err.clone()
    }
}

impl<I: Iterator<Item = Result<T, E>>, T, E> Iterator for TryIter<I, T, E> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next()? {
            Ok(result) => Some(result),
            err => {
                self.err = Some(err);
                None
            }
        }
    }
}
