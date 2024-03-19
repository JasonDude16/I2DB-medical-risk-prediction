library(dplyr)

# read raw data
df_train <- read.csv("./data/raw/Training_Set.csv")
df_test <- read.csv("./data/raw/Test_Set_NO_LABELS.csv")

# first convert outcome to factor 
df_train <- df_train %>% mutate(X1Yr_Death = as.factor(X1Yr_Death))

# function for processing test and train sets
# 1) remove variables with no predictive value 
# 2) replace empty race fields with "unknown"
# 3) convert characters to factor 
# 4) convert to tibble bc we like tibbles
process_data <- function(df){
  df <- df %>% 
    select(
      -Primary.Insurance,
      -Admission.Year
    ) %>% 
    mutate(
      Race.Simplified = ifelse(Race.Simplified == "", "Unknown", Race.Simplified),
      across(c(Gender, Race.Simplified, starts_with("Hx_")), as.factor)
    ) %>% 
    as_tibble()
  return(df)
}

# apply function
df_train <- process_data(df_train)
df_test <- process_data(df_test)

# now we'll create a validation set for testing our models 
set.seed(1)
in_train <- sample(1:nrow(df_train), round(nrow(df_train) * .85))
df_train <- df_train[in_train, ]
df_validation <- df_train[-in_train, ]

# let's ensure outcome is similar between splits
purrr::map(list(df_train, df_validation), function(df) {
  df %>% 
    group_by(X1Yr_Death) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    mutate(prop = count / sum(count))
})

# importantly, we export as RDS so we can maintain data types during subsequent imports
saveRDS(df_train, "./data/raw/Training_Set.RDS")
saveRDS(df_validation, "./data/raw/Validation_Set.RDS")
saveRDS(df_test, "./data/raw/Test_Sest.RDS")
