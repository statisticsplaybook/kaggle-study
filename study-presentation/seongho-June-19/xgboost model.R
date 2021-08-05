

#### XGBoost - basic

cores <- parallel::detectCores()
cores

set.seed(125)
xgb_mod <- boost_tree() %>% 
  set_mode("regression") %>% 
  set_engine("xgboost",
             num.threads = cores)


xgb_wf_lunch <- 
  workflow() %>% 
  add_model(xgb_mod) %>% 
  add_formula(중식계~.)
xgb_fit_lunch <- xgb_wf_lunch %>% 
  fit(train_base_lunch)
xgb_pred_lunch <- predict(xgb_fit_lunch,new_data = test_base_lunch)

xgb_wf_dinner <- 
  workflow() %>% 
  add_model(xgb_mod) %>% 
  add_formula(석식계~.)
xgb_fit_dinner <- xgb_wf_dinner %>% 
  fit(train_base_dinner)
xgb_pred_dinner <- predict(xgb_fit_dinner,new_data = test_base_dinner)

#변수중요도
xgb_fit_lunch%>% 
  pull_workflow_fit() %>% 
  vip(num_features = 30)
xgb_fit_dinner %>% 
  pull_workflow_fit() %>% 
  vip(num_features = 30)

xb_pred_lunch <- predict(xgb_fit_lunch, new_data = test_base_lunch) 
xb_pred_dinner <- predict(xgb_fit_dinner, new_data = test_base_dinner) 

sample_submission$중식계<-xb_pred_lunch$.pred
sample_submission$석식계<-xb_pred_dinner$.pred

write.csv(sample_submission,"xgboost_model.csv",row.names = F,fileEncoding = "UTF-8")



#### 4-2)XGBoost - tuning model

set.seed(126)
xgb_tuning_mod <- 
  boost_tree(trees = tune(), learn_rate = tune(),
             tree_depth = tune(), min_n = tune()) %>% 
  #mtry = tune()) %>% 
  #loss_reduction = tune()) %>% 
  #sample_size = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("xgboost")
xgb_tuning_params <- parameters(xgb_tuning_mod) %>% 
  finalize(train_base_lunch)
xgb_tuning_wf <- 
  workflow() %>% 
  add_model(xgb_tuning_mod) %>% 
  add_formula(중식계~.)
data_folds4 <- vfold_cv(train_base_lunch,v=10,strata = 중식계)
xgb_tuned <- tune_bayes(
  object = xgb_tuning_wf,
  resamples = data_folds4,
  param_info = xgb_tuning_params,
  iter = 10,
  metrics = metric_set(rmse),
  #    initial =5,
  control = control_bayes(
    verbose = F,
    #        no_improve = 5,
    save_pred = T,
    save_workflow = T
  )
)
best_xgb_model <- xgb_tuned %>% 
  select_best("rmse")
xgb_final_wf <- 
  xgb_tuning_wf %>% 
  finalize_workflow(best_xgb_model)
xgb_final_fit <- xgb_final_wf %>% 
  fit(train_base_lunch)
xgb_final_pred <- predict(xgb_final_fit,new_data = test_base_lunch)




xgb_tuning_params_dinner <- parameters(xgb_tuning_mod) %>% 
  finalize(train_base_dinner)
xgb_tuning_wf_dinner <- 
  workflow() %>% 
  add_model(xgb_tuning_mod) %>% 
  add_formula(석식계~.)
data_folds_dinner <- vfold_cv(train_base_dinner,v=10,strata = 석식계)
xgb_tuned_dinner <- tune_bayes(
  object = xgb_tuning_wf_dinner,
  resamples = data_folds_dinner,
  param_info = xgb_tuning_params_dinner,
  iter = 10,
  metrics = metric_set(rmse),
  #    initial =5,
  control = control_bayes(
    verbose = F,
    #        no_improve = 5,
    save_pred = T,
    save_workflow = T
  )
)
best_xgb_model_dinner <- xgb_tuned %>% 
  select_best("rmse")
xgb_final_wf_dinner <- 
  xgb_tuning_wf_dinner %>% 
  finalize_workflow(best_xgb_model_dinner)
xgb_final_fit_dinner <- xgb_final_wf_dinner %>% 
  fit(train_base_dinner)
xgb_final_pred_dinner <- predict(xgb_final_fit_dinner,new_data = test_base_dinner)


sample_submission$중식계<-xgb_final_pred$.pred
sample_submission$석식계<-xgb_final_pred_dinner$.pred

write.csv(sample_submission,"xgboost_tuned_model.csv",row.names = F,fileEncoding = "UTF-8")
