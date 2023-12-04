library(tidyverse)

# Part 1
input <- read.table("~/Downloads/AoC2023Day1.txt")

test <- c(
  "1abc2",
  "pqr3stu8vwx",
  "a1b2c3d4e5f",
  "treb7uchet"
)

string <- input$V1

# combining the first digit and the last digit
get_calibration_value <- function(string){
  digits <- str_extract_all(string, pattern="[:digit:]")
  digits <- unlist(digits)
  digits <- digits[c(1,length(digits))]
  digits <- as.numeric(paste(digits, collapse = ""))
  return(digits)
}
sum(unlist(lapply(string, get_calibration_value)))
sum(unlist(lapply(test, get_calibration_value)))

# Part 2
test <- c(
  "two1nine",
  "eightwothree",
  "abcone2threexyz",
  "xtwone3four",
  "4nineeightseven2",
  "zoneight234",
  "7pqrstsixteen"
  )

num_words <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
digits <- 1:9
names(digits) <- num_words
patterns <- paste0(num_words, collapse="|")

replace_word_with_digit<- function(word_digit){
  last_letter <- unlist(strsplit(word_digit, split = ""))[nchar(word_digit)]
  num <- digits[word_digit]
  #add last letter back in case it's part of another word
  replacement <- paste0(c(num, last_letter), collapse="")
  return(replacement)
}

replace_word_with_digit("two")

test_words <- lapply(test, word_nums_to_digits)
input_words <- unlist(lapply(input$V1, word_nums_to_digits))

sum(unlist(lapply(test_words, get_calibration_value))) == 281
sum(unlist(lapply(input_words, get_calibration_value)))



# 
# replace_all_words_with_digit <- function(string){
#   word_digit <- str_extract(string, pattern=patterns)
#   while(!(is.na(word_digit))){
#     string <- gsub(pattern=word_digit, replacement=replace_word_with_digit(word_digit), string)
#     word_digit <- str_extract(string, pattern=patterns)
#   }
#   return(string)
# }
