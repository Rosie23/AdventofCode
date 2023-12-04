library(tidyverse)

test <- read.delim("~/Downloads/day2test.txt", sep=":", header=F, fill=T)
input <- read.delim("~/Downloads/day2.txt", sep=":", header=F, fill=T)

head(test)
game_results <- input$V2

# Get list of cubes for a game
get_list_cubes <- function(x){
  bag <- str_split(x, "; ")
  all_game_cubes <- str_split(bag[[1]], ", ")
  return(all_game_cubes)
}

# from list of cubes make a df
make_cube_list <- function(cubes){
  cubes <- str_trim(cubes, side="both")
  output <- data.frame(V1="")
  for(col in c("blue","red","green")){
    index <- grep(col, cubes)
    if(length(index)==0){
      n_col <- 0
    } else n_col <- as.numeric(str_split(cubes, " ")[[index]][1])
    tmp <- data.frame(v1=n_col)
    colnames(tmp) <- col
    output <- cbind(output,tmp)
  }
  output <- output[,-1]
  return(output)
}


# Part 1
get_max_cubes_in_game <- function(game_result){
  all_game_cubes <- get_list_cubes(game_result)
  df <- map_df(all_game_cubes, make_cube_list)
  df_max <- data.frame(blue=max(df$blue), red=max(df$red), green=max(df$green))
  return(df_max)
}

max_cubes <- map_df(game_results, get_max_cubes_in_game)
max_cubes$ID <- 1:nrow(max_cubes)

# only 12 red cubes, 13 green cubes, and 14 blue cubes
max_cubes$possible <- max_cubes$red <13 & max_cubes$green <14 & max_cubes$blue < 15
max_cubes$rank_poss <- max_cubes$possible * max_cubes$ID
sum(max_cubes$rank_poss)

# Part 2
head(max_cubes)

get_min_power_cubes_in_game <- function(game_res){
  all_game_cubes <- get_list_cubes(game_res)
  df <- map_df(all_game_cubes, make_cube_list)
  power <- max(df$blue)*max(df$red)*max(df$green)
  return(power)
}

power_cubes <- sum(unlist(lapply(input[,2], get_min_power_cubes_in_game)))
power_cubes
