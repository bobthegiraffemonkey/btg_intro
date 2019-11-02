
# This should be a fairly high level script that calls into other things and
# makes the magic happen to animate and save it off. If anything here is
# awkward, it's because I haven't done a good enough job defining other stuff.

setwd("code/cubes/btg_intro")

word = "Bob\nthe\nGiraffemonkey"
indents = c(4,7,0)

# cat(unique(sort(btg)))
# a b e f g h i k m n o r t y

source("main.R")


### Config ###
win_width = 8
win_height = 4.8

n_rand = 1024
set.seed(175)

initial_wait_s = 1
fps = 50 # ???


point_border_width = tau
view_border_width = tau/2
line_width = 1

f.lux=T

animate_and_save(word,
                 indents,
                 "foo.mp4")
