library(tidyverse)

input <- readLines("~/Desktop/AoC/AoCDay9.txt")

test <- c("0 3 6 9 12 15",
          "1 3 6 10 15 21",
          "10 13 16 21 30 45")

# Part 1
get_diff <- function(x){
  diffs <- c()
  for (i in (1:length(x)-1)) {
    diff <- x[i+1]-x[i]
    diffs <- c(diffs, diff)
  }
  return(diffs)
}

get_next <- function(string){
  x <- string %>%
    str_split(pattern=" ") %>%
    unlist() %>%
    as.numeric()
  diffs <- get_diff(x)
  last_x <- diffs[length(diffs)]
  while (length(unique(diffs)) !=1) {
    diffs <- get_diff(diffs)
    last_x <- c(last_x, diffs[length(diffs)])
  }
  new <- x[length(x)]+sum(last_x)
  return(new)
}

test %>% map(get_next) %>%
  reduce(`+`)
# 114  

input %>% map(get_next) %>%
  reduce(`+`)

# Part 2
get_previous <- function(string){
  x <- string %>%
    str_split(pattern=" ") %>%
    unlist() %>%
    as.numeric()
  diffs <- get_diff(x)
  first_x <- diffs[1]
  while (length(unique(diffs)) !=1) {
    diffs <- get_diff(diffs)
    first_x <- c(first_x, diffs[1])
  }
  previous <- c(x[1], first_x) %>%
    reduce(`-`, .dir = "backward")
  return(previous)
}

get_previous(test[1]) == -3
get_previous(test[2]) == 0
get_previous(test[3]) == 5

test %>% map(get_previous) %>%
  reduce(`+`)
# 2

input %>% map(get_previous) %>%
  reduce(`+`)
