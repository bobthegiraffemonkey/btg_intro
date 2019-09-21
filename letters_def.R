
# Setup the list, then add each letter to it. Use the scratch_letters
# file for designing. See letter_functions for what the functions do
# in more detail.

# o; x y, point
# h: x y, point, can only wobble horizontally
# v; x y, point, can only wobble vertically
# n: {}, newline
# f; x y theta_1 theta_2 radius n, n vertices even spaced on circle between angles given
# s: x1 y1 x2 y2 n ccw, semicircle with n vertices

# If there's one poly inside another (like 'o'), do the outside first.

outlines = list()

outlines[["a"]] = "
h 4 0
o 4 1
o 4 2
o 4 3
f 3 1.5 0.25 0.75 1.5 3
n
o 3.1 1.5
f 3 1.5 0.25 0.75 0.5 2
"

outlines[["b"]] = "
h 0 0
s 1 0 1 3 4
o 1 5
o 1 6
o 0 6
o 0 4
o 0 2
n
s 1 1 1 2 3
"

outlines[["B"]] = "
h 0 0
f 1 1.5 0.75 1.1 1.5 2
f 1 4.5 0.8 1.26 1.5 3
o 0 6
o 0 4.113
o 0 1.145
n
s 0.9 1 0.9 2.3 3
n
s 0.7 3.5 0.7 5 2
"

outlines[["e"]] = "
o 1.618 0.785
f 1.5 1.5 -0.04 0.85 1.5 6
n
f 1.5 1.5 0.02 0.52 0.5 2
"

outlines[["f"]] = "
f 3 4 0.25 0.5 2 2
h 0 4
h 0 3
v 1 3
v 1 2
h 1 0
h 2 0
v 2 1
v 2 3
h 3 3
h 3 4
f 3 4 0.5 0.25 1 2
"

outlines[["g"]] = "
o 3 6
s 2 6 2 2.5 4 1
s 2 2 1.2 2 2 -1
s 0.2 2 3 2 3 1
n
s 2 5 2 3.5 2 1
"

outlines[["G"]] = "
f 1.5 1.5 0.17 1 1.5 7
o 1.6 1.5
o 1.6 1.2
f 1.5 1.5 0.93 0.16 0.8 5
"

outlines[["h"]] = "
h 0 0
h 1 0
s 1 2 2 2 2 -1
h 2 0
h 3 0
o 3 1
f 1.5 2 0 0.21 1.5 1
o 1 3
o 1 4.123
o 1 6
o 0 6
h 0 5
h 0 1.966
"

outlines[["i"]] = "
h 0 0
h 1 0
o 1 3
o 0 3
n
f 0.5 4 0 2 0.5 7
"

outlines[["k"]] = "
h 0 0
h 1 0
o 1 1
o 1.5 2
h 2 0
h 3 0
v 2 3
o 3 5
o 2 5
o 1 3
o 1 6
o 0 6
"

outlines[["m"]] = "
h 0 0
h 1 0
s 1 2 2 2 2 -1
o 2 0.1
o 3 0.1
s 3 2 4 2 2 -1
h 4 0
h 5 0
f 3.5 2 0 0.3 1.5 2
f 1.5 2 0.14 0.5 1.5 3
"
outlines[["n"]] = "
h 0 0
h 1 0
s 1 2 2 2 3 -1
h 2 0
h 3 0
s 3 2 0 2 4 1
"

outlines[["o"]] = "
f 2 2 0 1 2 7
n
f 2 2 0 1 1 6
"

outlines[["r"]] = "
h 0 0
h 1 0
f 4 3 0.5 0.25 3 2
f 4 3 0.25 0.5 4 3
"

outlines[["t"]] = "
s 1 1 2.5 1 4 1
o 2 1
o 2 3
o 3 3
o 3 4
o 1.62 4
f 1 3 0.25 0.56 1 3
o 0.5 3
o 1 3
"

outlines[["y"]] = "
h 0 0
h 1 0
o 3 6
o 2 6
o 1.5 4.3
o 1 6
o 0 6
o 1 3"
