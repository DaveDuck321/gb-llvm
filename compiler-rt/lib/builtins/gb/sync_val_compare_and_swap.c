#include <stdint.h>

uint8_t __impl__sync_val_compare_and_swap_1(
    volatile uint8_t *ptr, uint8_t expected,
    uint8_t desired) __asm__("__sync_val_compare_and_swap_1");

uint8_t __impl__sync_val_compare_and_swap_1(volatile uint8_t *ptr,
                                            uint8_t expected, uint8_t desired) {
  static volatile uint8_t *const g_interrupt_enable_reg = (uint8_t *)(0xFFFFU);

  uint8_t old_val = *g_interrupt_enable_reg;
  // There is a race here... Interrupts are not allowed to touch the interrupt
  // enable reg if sync-based atomics are used.
  *g_interrupt_enable_reg = 0;

  uint8_t current = *ptr;
  if (current == expected) {
    *ptr = desired;
  }

  *g_interrupt_enable_reg = old_val;
  return current;
}
