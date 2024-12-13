#include <stdint.h>

float noopt(float);

template <typename int_t>
int_t multiply(int_t lhs, int_t rhs) {
  int_t sum = 0;
  int_t x = rhs;

  while (lhs != 0) {
    if ((lhs & 1) != 0) {
        sum += x;
    }
    x <<= 1;
    lhs >>= 1;
  }
  return sum;
}

extern "C" {
uint8_t multiply_i8(uint8_t lhs, uint8_t rhs) {
  return multiply(lhs, rhs);
}
uint16_t multiply_i16(uint16_t lhs, uint16_t rhs) {
  return multiply(lhs, rhs);
}
uint8_t builtin_multiply(uint8_t lhs, uint8_t rhs) {
  return lhs * rhs;
}

uint16_t floating_point_add(uint16_t lhs) {
  return lhs + noopt(73.9f);
}

uint16_t floating_point_mul(uint16_t lhs) {
  return lhs * noopt(15.333f);
}
}
