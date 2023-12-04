library(tidyverse)

cards <- read.delim("~/Downloads/AoCDay4.txt", sep=":", header=F, fill=T)[,2]
test <- read.delim("~/Downloads/AoCDay4Test.txt", sep=":", header=F, fill=T)[,2]

get_points <- function(cards){
  nums <- str_split_1(cards, pattern="\\|")
  my_nums <- str_trim(nums[2], side="both") %>%
    str_split_1(pattern=" ")
  my_nums <- my_nums[my_nums != ""]
  winning_nums <- str_trim(nums[1], side="both") %>%
    str_split_1(pattern=" ")
  winning_nums <- winning_nums[winning_nums != ""]
  number_of_wins <- sum(my_nums %in% winning_nums)
  if(number_of_wins == 0) points <- 0 
  else{
    points <- 2^(number_of_wins-1)
  }
  return(points)
}

# Part 1
sum(unlist(lapply(test, get_points)))==13
sum(unlist(lapply(cards, get_points)))

# Part 2
get_matches <- function(cards){
  nums <- str_split_1(cards, pattern="\\|")
  my_nums <- str_trim(nums[2], side="both") %>%
    str_split_1(pattern=" ")
  my_nums <- my_nums[my_nums != ""]
  winning_nums <- str_trim(nums[1], side="both") %>%
    str_split_1(pattern=" ")
  winning_nums <- winning_nums[winning_nums != ""]
  number_of_wins <- sum(my_nums %in% winning_nums)
  return(number_of_wins)
}


total_cards_won <- function(cards){
  total_cards <- length(cards)
  n_cards_df <- data.frame(index=1:total_cards, n_cards = 1)
  for(i in 1:total_cards){
    card <- cards[i]
    n_matches <- get_matches(card)
    extra_cards <- rep(0, total_cards+n_matches)
    if(n_matches!=0){
      extra_cards[(i+1):(n_matches+i)] <- 1*n_cards_df$n_cards[i]
    }
    extra_cards <- extra_cards[1:total_cards]
    n_cards_df$n_cards <- n_cards_df$n_cards+extra_cards
  }
  return(sum(n_cards_df$n_cards))
}

total_cards_won(test) == 30
total_cards_won(cards)

