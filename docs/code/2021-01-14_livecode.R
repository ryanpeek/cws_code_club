# ggplot2 overview

# all code lives here: https://raw.githubusercontent.com/ryanpeek/cws_code_club/main/code/2021-01-28_livecode.R

# The Data ----------------------------------------------------------------

# get datasets
data("mtcars")
head(mtcars)

# Load Libraries ----------------------------------------------------------


library(ggplot2)
library(dplyr)


# Bones of ggplot: framing your house -------------------------------------

# the bones of the ggplot
#ggplot() # use this as an empty structure
p1 <- ggplot(data=mtcars, aes(x=hp, y=wt))  # specify data to be used in all subsequent geoms
p2 <- p1 + geom_point()

(p1 <- ggplot(data=mtcars, aes(x=hp, y=wt))  +
    geom_point()) # this is starting make a plot pretty, the "rooms"

(p1 <- ggplot()  +
    geom_point(data=mtcars, aes(x=hp, y=wt)))


# Adding Aes  and themes --------------------------------------------------

# make a boxplot with points
(b1 <- ggplot() +
    geom_boxplot(data=mtcars, aes(x=drat, y=cyl, group=cyl), alpha=0.8, fill="orange"))

# flip with coord_flip
(b1 <- ggplot() +
    geom_boxplot(data=mtcars, aes(x=drat, y=cyl, group=cyl), alpha=0.8, fill="orange")+
    coord_flip())

# add some labels
(b1 <- ggplot() +
    geom_boxplot(data=mtcars, aes(x=drat, y=cyl, group=cyl), alpha=0.8, fill="orange")+
    labs(x="X axis", y="Y axis", title = "A LARGE TITLE", subtitle = "Subtitle", caption="Where the data is from"))


# playing with themes and customizing
(b1 <- ggplot() +
    geom_boxplot(data=mtcars, aes(x=drat, y=cyl, group=cyl), alpha=0.8, fill="orange")+
    labs(x="X axis", y="Y axis", title = "A LARGE TITLE", subtitle = "Subtitle", caption="Where the data is from"))

# some nice customization packages
library(ggthemes)
library(ggdark)

b1 + ggdark::dark_theme_bw() +
  theme(axis.ticks.length.x = unit(0.4, "cm"))

# explain what the :: does?

# allows you to call a specific function from a package
# so it looks like: "mypackage::function()"
# readr::read_csv()
# dplyr::select()
