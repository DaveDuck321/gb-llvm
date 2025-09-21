// RUN: %clang_cc1 -std=c++20 -triple gb -O3 -emit-llvm %s -o - \
// RUN:         | FileCheck %s -check-prefix=CHECK


extern "C" [[gnu::gb_interrupt_cc]] void fn_int();
extern "C" void fn_normal();

extern "C" void fn_int_c() __attribute__((gb_interrupt_cc));

using InterruptFnType = __attribute__((gb_interrupt_cc)) void(*)(void);

void (*fn_normal_var) (void);
InterruptFnType fn_int_var;
void (*fn_int_c_var) (void) __attribute__((gb_interrupt_cc));

void test() {
    fn_normal();
    // CHECK: call void @fn_normal()

    fn_int();
    // CHECK: call gb_interrupt_cc void @fn_int()

    fn_int_c();
    // CHECK: call gb_interrupt_cc void @fn_int_c()

    fn_normal_var();
    // CHECK: call void %0

    fn_int_var();
    // CHECK: call gb_interrupt_cc void %1

    fn_int_c_var();
    // CHECK: call gb_interrupt_cc void %2
}
