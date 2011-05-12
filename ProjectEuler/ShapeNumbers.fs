module ProjectEuler.ShapeNumbers

#light

let triangle n =
    (n * (n+1)) / 2

let pentagon n =
    (n * ((3 * n) - 1)) / 2

let isPentagon n =
    ((sqrt(float (24 * n + 1)) + 1.0) % 6.0) = 0.0

let hexagon n =
    n * ((2 * n) - 1)

let longTriangle n =
    (n * (n+1L)) / 2L

let longPentagon n =
    (n * ((3L * n) - 1L)) / 2L

let longIsPentagon n =
    ((sqrt(float (24L * n + 1L)) + 1.0) % 6.0) = 0.0

let longHexagon n =
    n * ((2L * n) - 1L)