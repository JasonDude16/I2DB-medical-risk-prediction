library(tidyverse)

# read raw data
df <- read.csv("./data/raw/Training_Set.csv")

# remove variables with no predictive value 
df <- df %>% 
  select(
    -Primary.Insurance,
    -Admission.Year
  )

# replace empty race fields with "unknown"
df <- df %>% mutate(Race.Simplified = ifelse(Race.Simplified == "", "Unknown", Race.Simplified))

# convert characters to factor 
df <- df %>% mutate(across(c(Gender, Race.Simplified, X1Yr_Death, starts_with("Hx_")), as.factor))

# convert to tibble bc we like tibbles
df <- as_tibble(df)

# check types
str(df)

# importantly, we export as RDS so we can maintain data types during subsequent imports
saveRDS(df, "./data/raw/Training_Set.RDS")