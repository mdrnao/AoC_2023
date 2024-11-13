
input <- read.table("./day1/input.txt", header = FALSE)

# Remove all characters
numbers <- lapply(input$V1, function(x) gsub("[a-z]", "", x))

# Make them into a 
numbers_vect <-  lapply(numbers, function(x) stringr::str_split(x, ""))

# Dplyr
as.numeric(
  paste0(
    lapply(numbers_vect, function(x) first(unlist(x))),
    lapply(numbers_vect, function(x) last(unlist(x)))
  )
) %>%
  sum()


## Base
# first <- lapply(numbers_vect, function(x) x[[1]][1])
# last <- lapply(numbers_vect, function(x) x[[1]][length(x[[1]])])
# sum(as.numeric(paste0(first, last)))
