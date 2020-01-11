
# Author:
# Matt Sheerin / bobthegiraffemonkey

# Write out 'bobthegiraffemonkey'
# Have the outline as a graph
# Make colour change spread through the graph
# Animate the whole thing
# Randomise and find a nice seed
# Possibly, decide this wasn't the best use of my spare time

# This will likely not adhere to the best practices for writing R
# code, because I'm not familiar with those, and don't have much
# use for learning that paradigm. This may hinder maintainaility,
# but since this script is intended to be executed only once after
# completion and debugging, this consideration is rendered moot.

# It's still a hell of a lot cleaner than the messy crap I wrote
# during my PhD, they really should teach mathematicians writing
# code how to actually write decent code. Seriously.

require("stringr")
require("spatgraphs")
require("animation")
require("Matrix")


# Setup by sourcing definitions and functions in various files.
source("letters_def.R")
source("letter_functions.R")
source("word_functions.R")
source("plot_functions.R")
source("graph_functions.R")
source("animate.R")
source("init.R")

main = function(w, indents, filename, settings, dev=TRUE){
  # Plan is to call into the plotting function and pass in dev
  # if true, plot to external window, else actually live up to
  # function name.
  
  word_split = str_split(w, "", simplify = TRUE)
  stuff = init(word_split, settings)
  
  # frame = 0
  # while (!stuff$data$done) {
  #   frame = frame + 1
  #   print(frame)
  #   stuff$data = update_state(stuff$data$E, stuff$data$V)
  # }
  
  ani.options(ffmpeg = "C:/Users/USer/Downloads/ffmpeg-4.2-win64-static/ffmpeg-4.2-win64-static/bin/ffmpeg",
              ani.width=720,
              ani.heigth=480)
  frame = 0
  saveVideo({ani.options(interval = 1/29.97, nmax = 50)
    while (!stuff$data$done) {
      frame = frame + 1
      print(frame)
      for (i in settings$updates_per_frame) stuff$data = update_state(stuff$data$E, stuff$data$V)
      draw_graph(stuff$p, stuff$data$E, stuff$vars, settings)
      ani.pause()
    }
  }, video.name = filename)
  print("done")

  # draw_graph(stuff$p, stuff$data$E, stuff$vars, settings)
  # plot_word(word_split, indents)
}
