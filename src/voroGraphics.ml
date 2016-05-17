open Graphics

let unrgb (color: color): int * int * int =
  ((color lsr 16) land 0xFF,
   (color lsr 8) land 0xFF,
   color land 0xFF)
