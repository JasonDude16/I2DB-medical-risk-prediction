library(tidymodels)
library(tidyverse)

df_train <- readRDS("./data/processed/train_set_imputed.RDS")
df_valid <- readRDS("./data/processed/valid_set_imputed.RDS")
df_test <- readRDS("./data/processed/test_set_imputed.RDS")

df_train_valid <- rbind(df_train, df_valid)

rec <- df_train_valid %>% 
  recipe(X1Yr_Death ~ ., data = .) %>% 
  step_center(all_numeric_predictors()) %>% 
  step_scale(all_numeric_predictors())
  
# specify model and backend
mod <- logistic_reg(
  mode = "classification",
  engine = "glm"
)

# combine recipe and model by using a workflow
wf <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(mod)

fit_and_predict <- function(wf, train, test) {
  wf_fit <- wf %>% fit(train)
  wf_mod <- wf_fit %>% extract_fit_parsnip()
  wf_rec <- wf_fit %>% extract_recipe()
  cbind(test, predict(wf_mod, type = "prob", new_data = bake(wf_rec, new_data = test)))
}

df_test_preds <- fit_and_predict(wf, df_train_valid, df_test)

df_test_preds %>% 
  mutate(.pred_True = round(.pred_True, 2)) %>% 
  transmute(Person_id = Patient_ID, `Predicted probability` = .pred_True) %>% 
  write.csv(file = "./predictions/test_predictions.csv", row.names = F)