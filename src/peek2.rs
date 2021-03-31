#![allow(dead_code)]
use std::iter::FusedIterator;

/// An iterator with a `peek()` and a `peek_next()` that return optional references to the next
/// or after next elements respectively.
#[derive(Clone, Debug)]
#[must_use = "iterators are lazy and do nothing unless consumed"]
pub struct Peekable2<I: Iterator> {
    iter: I,
    /// Remember a peeked value, even if it was None.
    peeked: [Option<Option<I::Item>>; 2],
    p: bool,
}

impl<I: Iterator> Peekable2<I> {
    pub fn new(iter: I) -> Peekable2<I> {
        Peekable2 {
            iter,
            peeked: [None, None],
            p: false,
        }
    }
}

// Peekable must remember if a None has been seen in the `.peek()` method.
// It ensures that `.peek(); .peek();` or `.peek(); .next();` only advances the
// underlying iterator at most once. This does not by itself make the iterator
// fused.
impl<I: Iterator> Iterator for Peekable2<I> {
    type Item = I::Item;

    #[inline]
    fn next(&mut self) -> Option<I::Item> {
        let item = match self.peeked[self.p as usize].take() {
            Some(v) => v,
            None => self.iter.next(),
        };
        self.p = !self.p;
        item
    }

    #[inline]
    fn count(mut self) -> usize {
        self.iter.count()
            + match (self.peeked[0].take(), self.peeked[1].take()) {
                (Some(Some(_)), Some(Some(_))) => 2,
                (Some(Some(_)), _) | (_, Some(Some(_))) => 1,
                _ => 0,
            }
    }

    // #[inline]
    // fn nth(&mut self, n: usize) -> Option<I::Item> {
    //     match self.peeked.take() {
    //         Some(None) => None,
    //         Some(v @ Some(_)) if n == 0 => v,
    //         Some(Some(_)) => self.iter.nth(n - 1),
    //         None => self.iter.nth(n),
    //     }
    // }

    // #[inline]
    // fn last(mut self) -> Option<I::Item> {
    //     let peek_opt = match self.peeked.take() {
    //         Some(None) => return None,
    //         Some(v) => v,
    //         None => None,
    //     };
    //     self.iter.last().or(peek_opt)
    // }

    // #[inline]
    // fn size_hint(&self) -> (usize, Option<usize>) {
    //     let peek_len = match self.peeked {
    //         Some(None) => return (0, Some(0)),
    //         Some(Some(_)) => 1,
    //         None => 0,
    //     };
    //     let (lo, hi) = self.iter.size_hint();
    //     let lo = lo.saturating_add(peek_len);
    //     let hi = match hi {
    //         Some(x) => x.checked_add(peek_len),
    //         None => None,
    //     };
    //     (lo, hi)
    // }

    // #[inline]
    // fn fold<Acc, Fold>(self, init: Acc, mut fold: Fold) -> Acc
    // where
    //     Fold: FnMut(Acc, Self::Item) -> Acc,
    // {
    //     let acc = match self.peeked {
    //         Some(None) => return init,
    //         Some(Some(v)) => fold(init, v),
    //         None => init,
    //     };
    //     self.iter.fold(acc, fold)
    // }
}

// impl<I> DoubleEndedIterator for Peekable2<I>
// where
//     I: DoubleEndedIterator,
// {
//     #[inline]
//     fn next_back(&mut self) -> Option<Self::Item> {
//         match self.peeked.as_mut() {
//             Some(v @ Some(_)) => self.iter.next_back().or_else(|| v.take()),
//             Some(None) => None,
//             None => self.iter.next_back(),
//         }
//     }

//     #[inline]
//     fn rfold<Acc, Fold>(self, init: Acc, mut fold: Fold) -> Acc
//     where
//         Fold: FnMut(Acc, Self::Item) -> Acc,
//     {
//         match self.peeked {
//             Some(None) => init,
//             Some(Some(v)) => {
//                 let acc = self.iter.rfold(init, &mut fold);
//                 fold(acc, v)
//             }
//             None => self.iter.rfold(init, fold),
//         }
//     }
// }

impl<I: ExactSizeIterator> ExactSizeIterator for Peekable2<I> {}

impl<I: FusedIterator> FusedIterator for Peekable2<I> {}

impl<I: Iterator> Peekable2<I> {
    /// Returns a reference to the next() value without advancing the iterator.
    ///
    /// Like [`next`], if there is a value, it is wrapped in a `Some(T)`.
    /// But if the iteration is over, `None` is returned.
    ///
    /// [`next`]: Iterator::next
    ///
    /// Because `peek()` returns a reference, and many iterators iterate over
    /// references, there can be a possibly confusing situation where the
    /// return value is a double reference. You can see this effect in the
    /// examples below.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use rilox::peek2::Peekable2;
    ///
    /// let xs = [1, 2, 3];
    ///
    /// let mut iter = Peekable2::new(xs.iter());
    ///
    /// // peek_next() lets us see into the future
    /// assert_eq!(iter.peek(), Some(&&1));
    /// assert_eq!(iter.next(), Some(&1));
    ///
    /// assert_eq!(iter.next(), Some(&2));
    ///
    /// // The iterator does not advance even if we `peek` multiple times
    /// assert_eq!(iter.peek(), Some(&&3));
    /// assert_eq!(iter.peek(), Some(&&3));
    ///
    /// assert_eq!(iter.next(), Some(&3));
    ///
    /// // After the iterator is finished, so is `peek()`
    /// assert_eq!(iter.peek(), None);
    /// assert_eq!(iter.next(), None);
    /// ```
    #[inline]
    pub fn peek(&mut self) -> Option<&I::Item> {
        let iter = &mut self.iter;
        self.peeked[self.p as usize]
            .get_or_insert_with(|| iter.next())
            .as_ref()
    }

    /// Returns a reference to the value after the next() value without advancing the iterator.
    ///
    /// Like [`next`], if there is a value, it is wrapped in a `Some(T)`.
    /// But if the iteration is over, `None` is returned.
    ///
    /// [`next`]: Iterator::next
    ///
    /// Because `peek()` returns a reference, and many iterators iterate over
    /// references, there can be a possibly confusing situation where the
    /// return value is a double reference. You can see this effect in the
    /// examples below.
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use rilox::peek2::Peekable2;
    ///
    /// let xs = [1, 2, 3, 4];
    ///
    /// let mut iter = Peekable2::new(xs.iter());
    ///
    /// // peek_next() lets us see into the future
    /// assert_eq!(iter.peek_next(), Some(&&2));
    /// assert_eq!(iter.peek(), Some(&&1));
    /// assert_eq!(iter.next(), Some(&1));
    /// assert_eq!(iter.peek_next(), Some(&&3));
    /// assert_eq!(iter.peek(), Some(&&2));
    ///
    /// assert_eq!(iter.next(), Some(&2));
    ///
    /// // The iterator does not advance even if we `peek` multiple times
    /// assert_eq!(iter.peek_next(), Some(&&4));
    /// assert_eq!(iter.peek_next(), Some(&&4));
    ///
    /// assert_eq!(iter.next(), Some(&3));
    ///
    /// // After the iterator is finished, so is `peek()`
    /// assert_eq!(iter.peek_next(), None);
    /// assert_eq!(iter.next(), Some(&4));
    /// assert_eq!(iter.next(), None);
    /// ```
    #[inline]
    pub fn peek_next(&mut self) -> Option<&I::Item> {
        self.peek();
        let iter = &mut self.iter;
        self.peeked[!self.p as usize]
            .get_or_insert_with(|| iter.next())
            .as_ref()
    }

    /// Consume and return the next value of this iterator if a condition is true.
    ///
    /// If `func` returns `true` for the next value of this iterator, consume and return it.
    /// Otherwise, return `None`.
    ///
    /// # Examples
    /// Consume a number if it's equal to 0.
    /// ```
    /// let mut iter = (0..5).peekable();
    /// // The first item of the iterator is 0; consume it.
    /// assert_eq!(iter.next_if(|&x| x == 0), Some(0));
    /// // The next item returned is now 1, so `consume` will return `false`.
    /// assert_eq!(iter.next_if(|&x| x == 0), None);
    /// // `next_if` saves the value of the next item if it was not equal to `expected`.
    /// assert_eq!(iter.next(), Some(1));
    /// ```
    ///
    /// Consume any number less than 10.
    /// ```
    /// let mut iter = (1..20).peekable();
    /// // Consume all numbers less than 10
    /// while iter.next_if(|&x| x < 10).is_some() {}
    /// // The next value returned will be 10
    /// assert_eq!(iter.next(), Some(10));
    /// ```
    // pub fn next_if<F>(&mut self, func: F) -> Option<I::Item>
    // where
    //     F: FnOnce(&I::Item) -> bool,
    // {
    //     match self.next() {
    //         Some(matched) if func(&matched) => Some(matched),
    //         other => {
    //             // Since we called `self.next()`, we consumed `self.peeked`.
    //             assert!(self.peeked.is_none());
    //             self.peeked = Some(other);
    //             None
    //         }
    //     }
    // }

    /// Consume and return the next item if it is equal to `expected`.
    ///
    /// # Example
    /// Consume a number if it's equal to 0.
    /// ```
    /// let mut iter = (0..5).peekable();
    /// // The first item of the iterator is 0; consume it.
    /// assert_eq!(iter.next_if_eq(&0), Some(0));
    /// // The next item returned is now 1, so `consume` will return `false`.
    /// assert_eq!(iter.next_if_eq(&0), None);
    /// // `next_if_eq` saves the value of the next item if it was not equal to `expected`.
    /// assert_eq!(iter.next(), Some(1));
    /// ```
    fn a() {}
    // pub fn next_if_eq<T>(&mut self, expected: &T) -> Option<I::Item>
    // where
    //     T: ?Sized,
    //     I::Item: PartialEq<T>,
    // {
    //     self.next_if(|next| next == expected)
    // }
}
