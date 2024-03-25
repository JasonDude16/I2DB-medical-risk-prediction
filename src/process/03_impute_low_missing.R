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

# also need to add NA variable
df_test$X1Yr_Death <- NA

# combine all
df_all <- rbind(
  df_train,
  df_valid,
  df_test
)

# remove vars that we don't want to use as predictors
df_exclude <- df_all %>% dplyr::select(Patient_ID, X1Yr_Death)
df_sub <- df_all %>% dplyr::select(-Patient_ID, -X1Yr_Death)

# for the mice algorithm we can choose which rows are ignored during 
# imputation, so we will ignore valid and test sets to avoid data leakage
ignore_rows <- df_all$type %in% c("valid", "test")

# using multiple imputation with predictive mean matching 
mice_imp_obj <- df_sub %>% 
  futuremice(
    m = 50, 
    parallelseed = 16,
    n.core = (detectCores()/2) - 2,
    maxit = 25,
    ignore = ignore_rows
)

# average imputed datasets
# note that this would note not be appropriate if we were interested in
# statistical estimates, but we made a judgment call that this is ok for ML purposes
df_imp <- mice_imp_obj %>% 
  complete(action = "long") %>%
  dplyr::select(-.imp) %>%
  group_by(.id) %>%
  dplyr::summarize(
    across(where(is.numeric), mean),
    across(where(is.factor), .fns = ~ .x[1]),
    across(where(is.character), .fns = ~ .x[1])
  ) %>% 
  dplyr::select(-.id)

# add back excluded vars
df_all <- bind_cols(df_exclude, df_imp)

# now re-split data into their respective sets and save
for (type_ in c("train", "validation", "test")) {
  
  out <- file.path("./data/processed", paste0(type_, "_set_imputed.RDS"))
  res <- df_all %>% filter(type == type_) %>% dplyr::select(-type)
  
  # remove outcome that we added earlier
  if (type_ == "test") {
    res <- res %>% dplyr::select(-X1Yr_Death)
  }
    saveRDS(res, out)  
}

# also save mice object (for diagnostics)
if (!dir.exists("./data/processed/supp")) {
  dir.create("./data/processed/supp", recursive = TRUE)
}
saveRDS(mice_imp_obj, "./data/processed/supp/mice_object.rds")


# clear workspace so we can run scripts in succession
rm(list = ls())
