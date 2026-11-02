# ChangeLog for unix-time

## v0.6.0

This release changes the results of some functions without changing
their types. Please check the following before upgrading.

* `UnixDiffTime` is normalized so that both fields have the same sign.
  For example, -0.5 seconds is now `UnixDiffTime 0 (-500000)`
  instead of `UnixDiffTime (-1) 500000`.
  Code checking the sign with `udtSeconds d < 0` must use `d < 0`
  (or `signum`) instead.
* `parseUnixTime` takes DST into account. Results during DST were
  one hour off.
* `parseUnixTime` and `parseUnixTimeGMT` honor the offset of `%z`
  on glibc. It was ignored.
* The GMT functions no longer modify the `TZ` environment variable,
  which could be corrupted by concurrent calls. So they no longer
  call `tzset()` as a side effect.
* `addUnixDiffTime` normalizes `utMicroSeconds` into `[0, 1000000)`
  also for times before 1970.
* The `Binary` instance of `UnixTime` always encodes seconds as
  `Int64`. Data encoded on 32-bit platforms by older versions cannot
  be decoded.

Other changes:

* Security: `formatUnixTime` and `formatUnixTimeGMT` read beyond a
  heap buffer when the output exceeded 80 bytes. Now the buffer grows.
* New: `parseUnixTimeMaybe` and `parseUnixTimeGMTMaybe`.
* Fix `*`, `negate`, `abs` and `signum` of `UnixDiffTime`.
* Use `strptime_l()`/`strftime_l()` on Linux instead of changing
  the process-global locale.
* Require `base` >= 4.9.

## v0.5.0

* Remove deprecated `old-time` dependency by dropping `fromClockTime` and `toClockTime`
  [#68](https://github.com/kazu-yamamoto/unix-time/pull/68)

## v0.4.17

* Fix MinGW64 linker error
  [#67](https://github.com/kazu-yamamoto/unix-time/pull/67)

## v0.4.16

* Make `tzset` optional.
  [#65](https://github.com/kazu-yamamoto/unix-time/pull/65)

