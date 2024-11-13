# Part one ---------------------
input <- read.table("./day1/input.txt", header = FALSE)

# Remove all characters
numbers <- lapply(input$V1, function(x) gsub("[a-z]", "", x))

# Convert string to individual characters
numbers_vect <-  lapply(numbers, function(x) stringr::str_split(x, ""))

# Dplyr
as.numeric(
  paste0(
    lapply(numbers_vect, function(x) first(unlist(x))),
    lapply(numbers_vect, function(x) last(unlist(x)))
  )
) %>%
  sum()


# Base solution
# first <- lapply(numbers_vect, function(x) x[[1]][1])
# last <- lapply(numbers_vect, function(x) x[[1]][length(x[[1]])])
# sum(as.numeric(paste0(first, last)))

# Part two ---------------------

# Convert words into numbers

input <- read.table("./day1/input.txt", header = FALSE)

# Some of the numbers overlap - must be a better way to automate this?
input_mod <- str_replace_all(
  input$V1,
  c("eightwo" = "eighttwo",
    "nineight" = "nineeight",
    "zerone" = "zeroone",
    "fiveight" = "fiveeight",
    "threeight" = "threeeight",
    "twone" = "twoone",
    "oneight" = "oneeight"
  )
)

cnv_table <- data.frame(
  num = 1:9,
  word =
    c("one", "two", "three", "four", "five",
      "six", "seven", "eight", "nine")
)


input_list <- as.list(input_mod)
for (i in 1:nrow(cnv_table)){
  word <- cnv_table$word[i]
  num <- cnv_table$num[i]

  input_list <- lapply(input_list, function(x) gsub(word, num, x))


}

# Remove all characters
numbers <- lapply(input_list, function(x) gsub("[a-z]", "", x))

# Convert string to individual characters
numbers_vect <-  lapply(numbers, function(x) stringr::str_split(x, ""))

# Dplyr
as.numeric(
  paste0(
    lapply(numbers_vect, function(x) first(unlist(x))),
    lapply(numbers_vect, function(x) last(unlist(x)))
  )
) %>%
  sum()
