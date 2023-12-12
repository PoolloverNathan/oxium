#[allow(unused)]
macro_rules! defaulted {
  (($($out:tt)*)) => { $($out)* };
  (($(ignore:tt)*) $(out:tt)+) => { $($out)+ };
}

#[macro_export]
macro_rules! state_machine {
  (
    $($(#[$tmeta:meta])* $tvis:vis enum)? $name:ident$(<$($typar:ident),+>)? $(where {$($where:tt)+})? $(($($garg:ident: $gty:ty),* $(,)?))?;

    $(#[$fmeta:meta])* $fvis:vis fn $stepfn:ident(self $(, $arg:ident: $argty:ty)* $(,)?) $(-> $rty:ty)?;

    $(
      $(#[$imeta:meta])* $item:ident$(($($iarg:ident: $ity:ty),* $(,)?))? => $ivalue:expr;
    )*
  ) => {
    $($(#[$tmeta])* $tvis)? enum $name $(<$($typar:ident),+>)? $(where $($where:tt)+)? {
      $(
        $(#[$imeta])* $item($($($garg: $gty, )*)? $($($iarg: $ity, )*)?)
      ),*
    }
    impl $(<$($typar:ident),+>)? $name $(<$($typar:ident),+>)? $(where $($where:tt)+)? {
      $(#[$fmeta])* $fvis fn $stepfn(self $(, $arg: $argty)*) defaulted!((-> $name) $(-> $rty:ty)?) {
        match self {
          $(
            $(#[$imeta])* $item($($($garg, )*)? $($($iarg, )*)?) => $ivalue
          ),*
        }
      }
    }
  }
}
