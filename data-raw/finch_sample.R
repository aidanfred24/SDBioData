## code to prepare `finch_sample` dataset goes here

# Load "finch" raw gene counts (Zebra Finch genes) prior to this code

# Random sample of 100 genes
set.seed(67)
index <- sample(1:21407, 100, replace = FALSE)

finch_sample <- finch[index,]

usethis::use_data(finch_sample, overwrite = TRUE)
