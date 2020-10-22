# Network sketches
# http://blog.schochastics.net/post/sketchy-hand-drawn-like-networks-in-r/
remotes::install_github("schochastics/roughnet")

library(igraph)
library(roughnet)


g <- make_graph("Frucht")
roughnet(g,width = 600,height = 400)

# customize vertex
V(g)$shape  <- "water" # shapes: “heart”, “air”, “earth”, “fire”, “water”, "rectangle", "circle"
V(g)$fill   <- "skyblue"
V(g)$color  <- "#0f3460"
V(g)$stroke <- 2
V(g)$fillstyle <- "zigzag"
V(g)$size <- 40

# customize edges
E(g)$color <- "darkblue"
E(g)$width <- 2

roughnet(g, width = 800,height = 600)
