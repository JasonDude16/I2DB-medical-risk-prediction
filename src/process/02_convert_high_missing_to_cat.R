library(dplyr)
library(purrr)

df_train <- readRDS("./data/raw/Training_Set.RDS")
df_valid <- readRDS("./data/raw/Validation_Set.RDS")
df_test <- readRDS("./data/raw/Test_Sest.RDS")

# get variables with high missingness
# decided in meeting (3/20) to use 1000 as cutoff 
vars_high_miss <- df_train %>% 
  is.na() %>% 
  colSums() %>% 
  .[. > 1000] %>% 
  names()

# obtain cutoffs "tertile" cutoffs from train set
terts <- map(df_train[vars_high_miss], quantile, probs = seq(0, 1, 0.33), na.rm = T)

# create factor level for missing values, as dicussed (3/20)
continuous_to_tertile <- function(df, terts) {
  for (i in seq_along(terts)) {
    var <- names(terts)[i]
    df[[var]] <- case_when(
      df[[var]] <= terts[[i]][2] ~ "Low",
      between(df[[var]], terts[[i]][2], terts[[i]][3]) ~ "Medium",
      df[[var]] >= terts[[i]][3] ~ "High",
      TRUE ~ "Unknown"
    )
  }
  return(df)
}

# create a wrapper so we can easily apply to train, test and validation
process_data <- function(df, terts) {
  df %>% 
    continuous_to_tertile(terts) %>% 
    mutate(across(names(terts), as.factor))
}

df_train <- process_data(df_train, terts)
df_valid <- process_data(df_valid, terts)
df_test <- process_data(df_test, terts)

saveRDS(df_train, "./data/processed/Training_Set.RDS")
saveRDS(df_valid, "./data/processed/Validation_Set.RDS")
saveRDS(df_test, "./data/processed/Test_Sest.RDS")

# clear workspace so we can run scripts in succession
rm(list = ls())