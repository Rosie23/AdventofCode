library(tidyverse)

# Part 1
test_times <- c(7, 15, 30)
test_distances <- c(9, 40, 200)
# Time:      7  15   30
# Distance:  9  40  200

times <- c(49, 87, 78, 95)
distances <- c(356, 1378, 1502, 1882)
# Time:        49     87     78     95
# Distance:   356   1378   1502   1882


get_race_outcomes <- function(t, d){
  wins <- c()
  for(i in 0:t){
    race_dist <- i*(t-i)
    win <- race_dist>d
    wins <- c(wins, win)
  }
  return(sum(wins))
}

map2(test_times, test_distances, get_race_outcomes) %>%
  reduce(`*`) == 288

map2(times, distances, get_race_outcomes) %>%
  reduce(`*`)

# Part 2
test_times <- c(71530)
test_distances <- c(940200)

times <- c(49877895)
distances <- c(356137815021882)


get_race_outcome <- function(t_press, t_total, d){
  race_dist <- t_press*(t_total-t_press)
  win <- race_dist>d
  return(win)
}


# Find winning range
find_win_range <- function(t,d,split=100){
  splits <- seq(from=0, to=t, by=split)
  for(i in splits){
    wins <- get_race_outcome(t_press=i, t_total=t, d)
    if(wins){
      break
    }
  }
  splits <- seq(from=i, to=t, by=split)
  for(j in splits){
    wins <- get_race_outcome(t_press=j, t_total=t, d)
    if(!wins){
      break
    }
  }
  return(c(i,j))
}

win_range_test <- find_win_range(test_times, test_distances, split=1)
win_range_test[2]-win_range_test[1]

win_range <- find_win_range(times, distances, split=1)
win_range[2]-win_range[1]

