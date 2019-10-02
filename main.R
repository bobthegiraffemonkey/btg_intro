
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

if (f.lux){
  colours=c(rgb(.9,.9,0), rgb(1,1,1),
            rgb(1,0,0),  rgb(1,.5,0),
            rgb(0,1,0),  rgb(0.2,0.2,1),)
} else {
  colours=c(rgb(1,1,0), rgb(1,1,1),
            rgb(1,0,0), rgb(1,.5,0),
            rgb(0,1,0), rgb(0,0,1))
}
