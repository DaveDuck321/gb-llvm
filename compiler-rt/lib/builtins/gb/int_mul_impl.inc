#ifndef __mulxi3
#error "__mulxi3 must be defined to use this generic implementation"
#endif

#ifndef __mul_int_t
#error "__mul_int_t must be defined to use this generic implementation"
#endif

__mul_int_t __mulxi3(__mul_int_t lhs, __mul_int_t rhs) {
  // 15x = x + 2x + 4x + 8x + ...
  // 15x = x + 2 * (x + 2 * (x + 2 * (...)))
  __mul_int_t sum = 0;
  __mul_int_t x = rhs;

  while (lhs != 0) {
    if ((lhs & 1) != 0) {
        sum += x;
    }
    x <<= 1;
    lhs >>= 1;
  }
  return sum;
}
