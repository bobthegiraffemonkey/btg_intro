
# This should be a fairly high level script that calls into other things and
# makes the magic happen to animate and save it off. If anything here is
# awkward, it's because I haven't done a good enough job defining other stuff.

setwd("code/cubes/btg_intro")

word = "Bob\nthe\nGiraffemonkey"
indents = c(4,7,0)
# filename = "btg_intro.mp4"
filename = "btg_intro.mp4"

# cat(unique(sort(btg)))
# a b e f g h i k m n o r t y


### Config ##f#
settings = list()
settings$win_width = 8
settings$win_height = 4.8

settings$n_rand = 1024
settings$seed = 255

# I studied some epidemic dynamics at uni, and that helped inspire this design.
# DYK: A ;ong rectangular field is more resistant to the spread of disease
# than a square field of the same area?
settings$infection_rate = (exp(1)) / 100

settings$point_border_width = sqrt(42)
settings$view_border_width = 6.28318
settings$text_lwd = settings$view_border_width / 2
settings$bg_lwd = settings$text_lwd / 2

settings$initial_wait = 3
settings$updates_per_frame = 6

settings$f.lux = F


source("main.R")

main(word,
     indents,
     filename,
     settings,
     FALSE)
