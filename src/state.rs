#[allow(unused)]
macro_rules! defaulted {
  (($($out:tt)*)) => { $($out)* };
  (($(ignore:tt)*) $(out:tt)+) => { $($out)+ };
}

#[macro_export]
macro_rules! state_machine {
  (
    $(#[$tmeta:meta])* $tvis:vis enum $name:ident$([$($typar:tt),*])? $(where {$($where:tt)+})?;

    $(#[$fmeta:meta])* $fvis:vis fn $stepfn:ident(self $(, $arg:ident: $argty:ty)* $(,)?) $(-> $rty:ty)?;

    $(
      $(#[$imeta:meta])* $item:ident$(($($iarg:ident: $ity:ty),* $(,)?))? => $ivalue:expr;
    )*
  ) => {
    $(#[$tmeta])* $tvis enum $name $(<$($typar),*>)? $(where $($where)+)? {
      $(
        $(#[$imeta])* $item ($($($iarg: $ity, )*)?)
      ),*
    }
    impl $(<$($typar),*>)? $name $(<$($typar),*>)? $(where $($where)+)? {
      $(#[$fmeta])* $fvis fn $stepfn(self $(, $arg: $argty)*) defaulted!((-> $name) $(-> $rty:ty)?) {
        use $name::{$($item),*};
        match self {
          $(
            $(#[$imeta])* $item($($($iarg, )*)?) => $ivalue
          ),*
        }
      }
    }
  }
}
