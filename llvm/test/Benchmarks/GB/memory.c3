@require_once "std/array.c3"
@require_once "std/iterators.c3"

function g_memset : (dst: u8[mut&], value: u8, length: isize) -> void = {
    for i in range<isize>(0, length) {
        dst[i] = value;
    }
}
