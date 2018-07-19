library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(highcharter)
library(ggplot2)
library(ggthemes)
library(knitr)
theme_set(theme_minimal())

setwd('/Users/aminrakhsha/Documents/University/Term4 - 97spring/Data Analysis/HW/HW12')
readDatFile <- function(dir, colnames) {
  return(
    read_lines(dir) %>% 
    str_replace_all("::", "\036") %>%
    paste(collapse = "\n") %>%
    read_delim(delim = "\036",
               col_names = colnames,
               escape_double = F,
               trim_ws = T)
  )
}

movies = readDatFile('data/movies.dat', c('MovieID', 'Title', 'Genres'))
# ratings = readDatFile('data/ratings.dat', c("UserID", "MovieID", "Rating", "Timestamp"))  
ratings = read_delim(
  'data/ratings.dat',
  delim = '::',
  col_names = c("UserID", NA, "MovieID", NA, "Rating", NA, "Timestamp")
)
ratings = ratings %>% select(1, 3, 5, 7)
tags = readDatFile('data/tags.dat', c("UserID", "MovieID", "Tag", "Timestamp"))  

