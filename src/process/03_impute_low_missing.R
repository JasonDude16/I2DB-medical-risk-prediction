
library(micemd)
library(parallel)
library(tidyverse)

setwd("/Users/Tanner/Library/CloudStorage/Box-Box/I2DB Datathon 2024/processed")

training <- readRDS("Training_Set.RDS")

num_cores <- detectCores()
use_cores <- num_cores - 1

cl <- makeCluster(use_cores)


m_size <- 150
n_iter <- 25

imputed_data <- futuremice(Training_Set[,-1], m = m_size, maxit = n_iter, n.core = use_cores)

saveRDS(imputed_data, "Training_set_imputed_03212024.rds")

getwd()
