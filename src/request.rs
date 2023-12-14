//

use std::any::{TypeId, type_name};
use std::mem::transmute_copy;

#[inline]
fn type_of<T: 'static>(_: &T) -> TypeId {
  TypeId::of::<T>()
}

#[inline]
fn type_name_of<T>(_: &T) -> &'static str {
  type_name::<T>()
}

#[inline]
fn type_eq<T: 'static, U: 'static>() -> bool {
  TypeId::of::<T>() == TypeId::of::<U>()
}

#[inline]
fn is_a<T: 'static>(value: &(impl Sized + 'static)) -> bool {
  type_of(value) == TypeId::of::<T>()
}

#[inline]
fn cast_if_a<T: 'static>(value: impl Sized + 'static) -> Option<T> {
  is_a::<T>(&value).then(|| unsafe { transmute_copy(&value) })
}

/// Requests are a way to generically get fields from objects based on their type. Requests are designed to exploit monomorphization to have at most the same cost of inserting into the base container.
pub trait Request: Sized {
  /// Responds to a request with a value.
  fn supply<T: 'static>(&mut self, value: T);

  /// Returns whether a request can accept a specific type of value. This should only be used for optimization, as false positives are allowed (and the default implementation is to return [true] for all types).
  #[inline]
  fn can_accept<T: 'static>(&self) -> bool {
    true
  }
}

impl<T: 'static> Request for Option<T> {
  #[inline]
  fn supply<U: 'static>(&mut self, value: U) {
    if self.is_none() {
      *self = cast_if_a::<T>(value);
    }
  }

  #[inline]
  fn can_accept<U: 'static>(&self) -> bool {
    type_eq::<T, U>()
  }
}

impl<T: 'static> Request for Vec<T> {
  #[inline]
  fn supply<U: 'static>(&mut self, value: U) {
    if let Some(value) = cast_if_a(value) {
      self.push(value);
    }
  }

  #[inline]
  fn can_accept<U: 'static>(&self) -> bool {
    type_eq::<T, U>()
  }
}

/// Providers are generic bags of properties capable of answering [Request]s.
pub trait Provider {
  /// Answers a request.
  fn answer(&self, request: &mut impl Request);

  /// Returns whether the provider can answer a request. This should only be used for optimization, as false positives are allowed (and the default implementation is to return [true] for all types).
  #[inline]
  fn can_answer(&self, request: &impl Request) -> bool {
    true
  }
}