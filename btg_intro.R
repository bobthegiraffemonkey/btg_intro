
# This should be a fairly high level script that calls into other things and
# makes the magic happen to animate and save it off. If anything here is
# awkward, it's because I haven't done a good enough job defining other stuff.

setwd("code/cubes/btg_intro")

word = "Bob\nthe\nGiraffemonkey"
indents = c(4,7,0)
# filename = "btg_intro.mp4"
filename = "foo.mp4"

# cat(unique(sort(btg)))
# a b e f g h i k m n o r t y


### Config ###
settings = list()
settings["win_width"] = 8
settings["win_height"] = 4.8

settings["n_rand"] = 1024
settings["seed"] = 175

settings["initial_wait_s"] = 1
settings["fps"] = 50 # ???

settings["point_border_width"] = 6.28
settings["view_border_width"] = 6.28 / 2
settings["text_lwd"] = 6.28 / 2
settings["bg_lwd"] = settings$text_lwd / 2

settings["f.lux"] = T


source("main.R")
