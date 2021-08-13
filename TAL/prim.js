// Provides: Base_am_testing
function Base_am_testing() {
  return 0;
}
// Provides: Base_int_math_int64_clz
function Base_int_math_int64_clz() {
  return 0;
}
// Provides: Base_int_math_int64_ctz
function Base_int_math_int64_ctz() {
  return 0;
}
// Provides: Base_int_math_int64_pow_stub
function Base_int_math_int64_pow_stub() {
  return 0;
}
// Provides: Base_int_math_int_clz
function Base_int_math_int_clz() {
  return 0;
}
// Provides: Base_int_math_int_ctz
function Base_int_math_int_ctz() {
  return 0;
}
// Provides: Base_int_math_int_popcount
function Base_int_math_int_popcount() {
  return 0;
}
// Provides: Base_int_math_int_pow_stub
function Base_int_math_int_pow_stub() {
  return 0;
}
// Provides: Base_internalhash_fold_float
function Base_internalhash_fold_float() {
  return 0;
}
// Provides: Base_internalhash_fold_int
function Base_internalhash_fold_int() {
  return 0;
}
// Provides: Base_internalhash_fold_int64
function Base_internalhash_fold_int64() {
  return 0;
}
// Provides: Base_internalhash_fold_string
function Base_internalhash_fold_string() {
  return 0;
}
// Provides: Base_internalhash_get_hash_value
function Base_internalhash_get_hash_value() {
  return 0;
}
// Provides: caml_out_channel_pos_fd
function caml_out_channel_pos_fd() {
  return 0;
}
// Provides: expect_test_collector_after_test
function expect_test_collector_after_test() {
  return 0;
}
// Provides: expect_test_collector_before_test
function expect_test_collector_before_test() {
  return 0;
}
// Provides: time_now_nanoseconds_since_unix_epoch_or_zero
function time_now_nanoseconds_since_unix_epoch_or_zero() {
  return 0;
}
// Provides: caml_alloc_dummy_infix
function caml_alloc_dummy_infix() {
  return {};
}
// Provides: ml_z_mul_overflows
// https://github.com/janestreet/zarith_stubs_js/pull/8#issuecomment-830694486
function ml_z_mul_overflows(x, y) {
  var z = x * y;
  return z != (z | 0);
}
