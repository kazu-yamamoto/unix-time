# ChangeLog for unix-time

## v0.5.1

* Security: `formatUnixTime` and `formatUnixTimeGMT` read beyond a
  heap buffer when the output exceeded 80 bytes. Now the buffer grows.
* Security: concurrent calls of the GMT functions could leave the
  process-global `TZ` corrupted. Calls are now serialized and the old
  `TZ` value is copied before being restored.
* Windows: fix a heap overflow in `_patch_unsetenv`, which also failed
  to unset `TZ`.

## v0.5.0

* Remove deprecated `old-time` dependency by dropping `fromClockTime` and `toClockTime`
  [#68](https://github.com/kazu-yamamoto/unix-time/pull/68)

## v0.4.17

* Fix MinGW64 linker error
  [#67](https://github.com/kazu-yamamoto/unix-time/pull/67)

## v0.4.16

* Make `tzset` optional.
  [#65](https://github.com/kazu-yamamoto/unix-time/pull/65)

