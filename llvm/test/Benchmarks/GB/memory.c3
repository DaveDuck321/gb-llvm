@require_once "std/array.c3"
@require_once "std/iterators.c3"

function g_memset : (dst: u8[mut&], value: u8, length: isize) -> void = {
    for i in range<isize>(0, length) {
        dst[i] = value;
    }
}

function g_memcpy : (dst: u8[mut&], src: u8[&], length: isize) -> void = {
    for i in range<isize>(0, length) {
        dst[i] = src[i];
    }
}

function g_strlen : (string: u8[&]) -> isize = {
    mut len : isize = 0;
    while string[len] != "\0"[0] {
        len += 1;
    }
    return len;
}
