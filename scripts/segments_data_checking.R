library(tidyverse)
library(here)
library(readxl)
library(magrittr)

# data checking - check that segments were correctly grouped
x <- map_dbl(segments_grp$seg_ls, ~length(.x))
sum(x > 2)

table(segments_grp$seg_ls[[10]])[[1]] > length(segments_grp$seg_ls[[10]])

x <- map_lgl(segments_grp$seg_ls, function(x) table(x)[[1]] == length(x))
which(x == 0)

p <- c("black", "white")
q <- c("yellow", "red")
t <- c("pink", "pink")

r <- list(p,q, t)
x <- r
map_lgl(r, function(x) table(x)[[1]] == length(x))