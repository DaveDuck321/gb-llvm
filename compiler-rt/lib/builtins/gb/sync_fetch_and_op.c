#define __is_signed 0

#define __op_name __sync_lock_test_and_set_1
#define __op(lhs, rhs) rhs;
#include "sync_fetch_and_op.inc"
#undef __op_name
#undef __op

#define __op_name __sync_fetch_and_add_1
#define __op(lhs, rhs) (lhs + rhs);
#include "sync_fetch_and_op.inc"
#undef __op_name
#undef __op

#define __op_name __sync_fetch_and_sub_1
#define __op(lhs, rhs) (lhs - rhs);
#include "sync_fetch_and_op.inc"
#undef __op_name
#undef __op

#define __op_name __sync_fetch_and_and_1
#define __op(lhs, rhs) (lhs & rhs);
#include "sync_fetch_and_op.inc"
#undef __op_name
#undef __op

#define __op_name __sync_fetch_and_or_1
#define __op(lhs, rhs) (lhs | rhs);
#include "sync_fetch_and_op.inc"
#undef __op_name
#undef __op

#define __op_name __sync_fetch_and_xor_1
#define __op(lhs, rhs) (lhs ^ rhs);
#include "sync_fetch_and_op.inc"
#undef __op_name
#undef __op

#define __op_name __sync_fetch_and_nand_1
#define __op(lhs, rhs) ~(lhs & rhs);
#include "sync_fetch_and_op.inc"
#undef __op_name
#undef __op

#define __op_name __sync_fetch_and_umax_1
#define __op(lhs, rhs) (lhs >= rhs ? lhs : rhs);
#include "sync_fetch_and_op.inc"
#undef __op_name
#undef __op

#define __op_name __sync_fetch_and_umin_1
#define __op(lhs, rhs) (lhs >= rhs ? rhs : lhs);
#include "sync_fetch_and_op.inc"
#undef __op_name
#undef __op

#undef __is_signed
#define __is_signed 1

#define __op_name __sync_fetch_and_max_1
#define __op(lhs, rhs) (lhs >= rhs ? lhs : rhs);
#include "sync_fetch_and_op.inc"
#undef __op_name
#undef __op

#define __op_name __sync_fetch_and_min_1
#define __op(lhs, rhs) (lhs >= rhs ? rhs : lhs);
#include "sync_fetch_and_op.inc"
#undef __op_name
#undef __op
