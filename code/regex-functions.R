## Title: Regex Functions
## Description: Write functions that do things with strings
##
## Inputs: none
## Outputs: none
#==============================================
library(stringr)

#' @title split_chars(phrase)
#' @description tests if a numeric number can be considered to be an integer value
#' @param phrase character string
#' @return vector where each element is a singe character
split_chars <- function(phrase){
  l <- str_length(phrase)
  v <- rep(0,l)
  for (i in 1:l){
    v[i] <- str_sub(phrase, start=i,end=i)
  }
  v
}
vec1 <- split_chars('Go Bears!')
vec2 <- split_chars('Expecto Patronum')

#' @title num_vowels(vec)
#' @description returns number of values in a character vector
#' @param vec vector in which each element is a single character
#' @return numeric vector with 5 elements

num_vowels <- function(vec){
  v <- rep(0,5)
  names(v) <- c('a','e','i','o','u')
  for (i in 1:length(vec)){
    if(vec[i]=="a"|vec[i]=="A"){
      v[1] <- v[1]+1
    }
    if(vec[i]=="e"|vec[i]=="E"){
      v[2] <- v[2]+1
    }
    if(vec[i]=="i"|vec[i]=="I"){
      v[3] <- v[3]+1
    }
    if(vec[i]=="o"|vec[i]=="O"){
      v[4] <- v[4]+1
    }
    if(vec[i]=="u"|vec[i]=="U"){
      v[5] <- v[5]+1
    }
  }
  v
}

num_vowels(vec1)
num_vowels(vec2)

#' @title count_vowels(phrase)
#' @description rcomputers number of vowels in a character string
#' @param phrase character string
#' @return numeric vector with 5 elements

count_vowels <- function(phrase){
  num_vowels(split_chars(phrase))
}

count_vowels('the quick brown fox jumps over the lazy dog')
count_vowels('THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG')

#' @title reverse_chars(phrase)
#' @description reverses order
#' @param vec character vector
#' @return character vector with reversed characters

reverse_chars <- function(phrase){
  l <- str_length(phrase)
  v <- rep(0,l)
  for (i in 1:l){
    v[i] <- str_sub(phrase, start=-i,end=-i)
  }
  str_c(v,collapse="")
}
reverse_chars("gattaca")
reverse_chars("Lumos Maxima")
reverse_chars("put my thing down flip it and reverse it")

#' @title reverse_words(phrase)
#' @description reverses order by words
#' @param vec character vector
#' @return character vector with reversed words
#' 
reverse_words <- function(phrase){
  words <- strsplit(phrase," ")[[1]]
  l <- length(words)
  v <- rep(0,l)
  for (i in 1:l){
    v[i] <- words[l+1-i]
  }
  str_c(v,collapse=" ")
}
reverse_words("reverse this sentence!")
reverse_words("string")