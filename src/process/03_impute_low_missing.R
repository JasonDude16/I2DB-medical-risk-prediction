library(mice)
library(parallel)
library(dplyr)

# need to read in all data 
df_train <- readRDS("./data/processed/Training_Set.RDS")
df_valid <- readRDS("./data/processed/Validation_Set.RDS")
df_test <- readRDS("./data/processed/Test_Sest.RDS")

# create "type" variable so we know which set the row belongs to
df_train$type <- "train"
df_valid$type <- "valid"
df_test$type <- "test"
df_test$X1Yr_Death <- NA

# combine all
df_all <- rbind(
  df_train,
  df_valid,
  df_test
)

# remove vars that we don't want to use as predictors
df_exclude <- df_all %>% select(Patient_ID, X1Yr_Death)
df_sub <- df_all %>% select(-Patient_ID, -X1Yr_Death)

# for the mice algorithm we can choose which rows are ignored during 
# imputation, so we will ignore valid and test sets to avoid data leakage
ignore_rows <- df_all$type %in% c("valid", "test")

# using multiple imputation with predictive mean matching 
mice_imp_obj <- futuremice(
  data = df_sub, 
  m = 3, 
  parallelseed = 16,
  n.core = (detectCores()/2) - 2,
  maxit = 5,
  ignore = ignore_rows
)

# extract one of imputed datasets and add back vars
df_imp <- complete(mice_imp_obj, 1)
df_all <- bind_cols(df_exclude, df_imp)

# now re-split data into their respective sets and save
for (type in c("train", "validation", "test")) {
  df_imp %>% 
    filter(type == type) %>% 
    saveRDS(file.path("./data/processed/", paste0(type, "_set_imputed.RDS")))  
}

# also save mice object (for diagnostics)
if (!dir.exists("./data/processed/supp")) {
  dir.create("./data/processed/supp", recursive = TRUE)
}
saveRDS(mice_imp_obj, "./data/processed/supp/mice_object.rds")

# clear workspace so we can run scripts in succession
rm(list = ls())