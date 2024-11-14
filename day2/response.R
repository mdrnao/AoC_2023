# Part one ---------------------
r_max <- 12
g_max <- 13
b_max <- 14

# Use read lines since the input data isn't pretty
input <- readLines("./day2/input.txt")

# Separate the game IDs
input <- limma::strsplit2(input, split = ":")

# Pivot the data long, keep the game IDs with the individual draws
input_long <- data.frame(
  game = input[, 1],
  limma::strsplit2(input[, 2], split = ";")
) %>%
  tidyr::pivot_longer(2:7) %>%
  mutate(value = gsub("^ ", "", value)) %>% # Remove excess spaces at the start
  filter(value != "")

# Pivot the data long, keep the game IDs, keep the draw ID
input_longer <- data.frame(
  game = input_long$game,
  draw = input_long$name,
  limma::strsplit2(input_long$value, split = ", ")
)  %>%
  tidyr::pivot_longer(3:5) %>%
  filter(value != "")

# Split the colours and counts apart
input_longest <- data.frame(
  input_longer[1:3],
  limma::strsplit2(input_longer$value, split = " ")
) %>%
  mutate(
    X1 = as.numeric(X1),
    X2 = gsub("^ ", "", X2)
  )
colnames(input_longest)[c(4, 5)] <- c("val", "colour")

# Create a pass/fail column
input_longest$pass <-
  ifelse(input_longest$colour == "red" & input_longest$val <= 12, "pass",
  ifelse(input_longest$colour == "blue" & input_longest$val <= 14, "pass",
  ifelse(input_longest$colour == "green" & input_longest$val <= 13, "pass",
  "fail"
)))
input_longest$game_id <- as.numeric(limma::strsplit2(input_longest$game, " ")[,2]) # nolint

poss_games <- input_longest %>%
  group_by(game, pass) %>%
  summarise(n = n()) %>%
  tidyr::pivot_wider(id_cols = "game",
    names_from = "pass",
    values_from = "n"
  ) %>%
  filter(is.na(fail))
tmp2 <- limma::strsplit2(poss_games$game, " ")
sum(as.numeric(tmp2[, 2]))

# Part two ---------------------

# Max power

part_2 <- input_longest %>%
  group_by(game, colour) %>%
  summarise(max = max(val)) %>%
  group_by(game) %>%
  summarise(prod = prod(max))
sum(part_2$prod)
