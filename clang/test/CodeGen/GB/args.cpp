// RUN: %clang_cc1 -std=c++20 -triple gb -O3 -emit-llvm %s -o - \
// RUN:         | FileCheck %s -check-prefix=CHECK
struct Coord {
    char a;
    char b;
};

// CHECK: void @takes_coord(i8 %.0, i8 %.1)
extern "C" void takes_coord(Coord) {}


struct ShortCoord {
    short a;
    short b;
};

// CHECK: void @takes_short_coord(i16 %.0, i16 %.1)
extern "C" void takes_short_coord(ShortCoord) {}

struct LongCoord {
    long a;
    long b;
};

// CHECK: void @takes_long_coord(ptr
extern "C" void takes_long_coord(LongCoord) {}
