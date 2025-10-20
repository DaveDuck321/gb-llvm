#include <stdint.h>

#ifndef __op_name
#error "__op_name must be defined to use this generic implementation"
#endif

#ifndef __op
#error "__op must be defined to use this generic implementation"
#endif

#include <stdint.h>

#define C_NAME_IMPL(op_name) __impl##op_name
#define C_NAME_FN(op_name) C_NAME_IMPL(op_name)
#define C_NAME C_NAME_FN(__op_name)

#define STR_IMPL(macro) #macro
#define STR(macro) STR_IMPL(macro)

#if __is_signed
#define iN int8_t
#else
#define iN uint8_t
#endif

// Clang complains if we redefine the __sync builtins -- even though they're
// library functions. Trick the type checker with the asm label
iN C_NAME(volatile iN *ptr, iN val) __asm__(STR(__op_name));
iN C_NAME(volatile iN *ptr, iN val) {
  static volatile uint8_t *const g_interrupt_enable_reg = (uint8_t *)(0xFFFFU);

  uint8_t old_val = *g_interrupt_enable_reg;
  // There is a race here... Interrupts are not allowed to touch the interrupt
  // enable reg if sync-based atomics are used.
  *g_interrupt_enable_reg = 0;

  iN res = *ptr;
  *ptr = __op(res, val);

  *g_interrupt_enable_reg = old_val;
  return res;
}

#undef iN
