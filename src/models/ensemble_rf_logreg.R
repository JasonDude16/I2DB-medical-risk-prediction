library(tidyverse)
library(tidymodels)

df_train <- readRDS("./data/processed/train_set_imputed.RDS")

mod_logreg <- readRDS("./models/baseline-logreg-10foldcv.RDS")
mod_rf <- readRDS("./models/baseline-randomforest-10foldcv-tunegrid.rds")

mod_preds_logreg <- mod_logreg %>% 
  collect_predictions() %>% 
  arrange(.row)

mod_rf_best <- select_best(mod_rf, metric = "roc_auc")

mod_preds_rf <- mod_rf %>% 
  collect_predictions() %>% 
  filter(.config == mod_rf_best$.config) %>% 
  arrange(.row)

df_preds <- tibble(
  X1Yr_Death = ifelse(mod_preds_rf$X1Yr_Death == "False", 0, 1),
  pred_rf = mod_preds_rf$.pred_False,
  pred_logreg = mod_preds_logreg$.pred_False,
  pred_rf_x_pred_logreg = pred_rf*pred_logreg
)
# ---------------------------------------------------------------------------------------------

rec <- df_preds %>% 
  recipe(X1Yr_Death ~ ., data = .) %>% 
  step_mutate(X1Yr_Death = as.factor(X1Yr_Death))

# specify model and backend
mod <- logistic_reg(
  mode = "classification",
  engine = "glm"
)

# combine recipe and model by using a workflow
wf <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(mod)

# use 10-fold CV to fit model to data
set.seed(123)
folds <- vfold_cv(df_preds, v = 10)
cntrl_opts <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

# fit model
wf_res <- fit_resamples(wf, resamples = folds, control = cntrl_opts)
collect_metrics(wf_res)

saveRDS(wf_res, "./models/ensemble-rf-logreg-10foldcv.rds")
saveRDS(wf, "./workflows/ensemble-rf-logreg-10foldcv.rds")
