# Testing function

Bali_BBN <- read.csv(file.choose())
priors1 <- read.csv(file.choose())
priors2 <- read.csv(file.choose())
priors3 <- read.csv(file.choose())

library(bbnet)

input.files(Bali_BBN, priors1, priors2, priors3)

bbn.predict()
