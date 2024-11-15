# Part one ---------------------
input <-  readLines("./day3/input.txt")

input_list <- lapply(input, function(x) unlist(strsplit(x, "")))

# Change numbers to N characters
input_list <- lapply(input_list, function(x) gsub("[0-9]", "N", x))
input_list <- lapply(input_list, function(x) gsub("\\.", "x", x))
input_list <- lapply(input_list, function(x) gsub("[[:punct:]]", "P", x, perl = TRUE))

pos_matches <- list()
for (i in 1:length(input_list)){
  in_row <- input_list[[i]]
  pos <- which(in_row == "N")

  punct_mat <- data.frame(row = NA, pos = NA, punct = NA)
  for (j in pos){
    above <- NULL
    below <- NULL
    res <- c(
      in_row[c(j - 1, j, j + 1)],
      if (i > 1) input_list[[i - 1]][c(j - 1, j, j + 1)],
      if (i < 140) input_list[[i + 1]][c(j - 1, j, j + 1)]
    )
    tmp <- data.frame(
      row = i,
      pos = j,
      punct = c("P") %in% res[!is.na(res)]
    )
    punct_mat <- rbind(punct_mat, tmp)
  }
  pos_matches[[i]] <- punct_mat
}
res <- do.call(rbind, pos_matches) %>%
  data.frame() %>%
  filter(punct == TRUE)



input_list <- lapply(input, function(x) unlist(strsplit(x, "")))

# Number positions
boo_list <- lapply(input_list, function(x) grep("[[:digit:]]+", x, perl = TRUE))

# Split into sets of consecutive values
boo_list2 <- lapply(boo_list, function(x) split(x, cumsum(c(1, diff(x) != 1))))

num_list <- list()
for (i in 1:140){
  roi <- boo_list2[[i]]
  input_roi <- input_list[[i]]
  part_nums <- res %>%
    filter(row == i)

  keep <- as.character(
    unlist(
      lapply(roi, function(x)  any(x %in% part_nums$pos))
    )
  )
  roi <- roi[which(keep != "FALSE")]

  nums <- lapply(roi, function(x) as.numeric(paste0(input_roi[x], collapse = "")))

  num_list[[i]] <- unlist(nums)
}
sum(unlist(num_list))