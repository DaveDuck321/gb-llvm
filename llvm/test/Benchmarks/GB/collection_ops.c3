@require_once "std/arithmetic.c3"

typedef Rect : {
    x: int,
    y: int,
    width: int,
    height: int,
}

typedef Player : {
    rect: Rect,
    health: int,
}

function do_intersect : (rect1: Rect&, rect2: Rect&) -> bool = {
    let x_collision : bool = ((rect1.x + rect1.width) >= rect2.x) & ((rect2.x + rect2.width) >= rect1.x);
    let y_collision : bool = ((rect1.y + rect1.height) >= rect2.y) & ((rect2.y + rect2.height) >= rect1.y);
    return x_collision & y_collision;
}

function do_collide : (player1: Player&, player2: Player&) -> bool = {
    return do_intersect(&player1.rect, &player2.rect);
}

function get_player : (x: int, y: int) -> Player = {
    return {
        .rect = {
            .x = x,
            .y = y,
            .width = 100,
            .height = 100,
        },
        .health = 10,
    };
}
