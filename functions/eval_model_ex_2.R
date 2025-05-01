eval_model_ex_2 <- function(k_val, df_train, df_test){ 
  
  # Model Fitting:
  mod <- caret::train(
    pp_ex_2, 
    data = daily_fluxes_train_ex_2 |> drop_na(), 
    method = "knn",
    trControl = caret::trainControl(method = "none"),
    tuneGrid = data.frame(k = k_val),
    metric = "RMSE"
  )
  
  # Add predictions to the data frames
  df_train <- df_train |> 
    drop_na()
  df_train$fitted <- predict(mod, newdata = df_train)
  
  df_test <- df_test |> 
    drop_na()
  df_test$fitted <- predict(mod, newdata = df_test)
  
  # Get metrics tables
  metrics_train <- df_train |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  metrics_test <- df_test |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  # Extract values from metrics tables
  rmse_train <- metrics_train |> 
    filter(.metric == "rmse") |> 
    pull(.estimate)
  rsq_train <- metrics_train |> 
    filter(.metric == "rsq") |> 
    pull(.estimate)
  
  rmse_test <- metrics_test |> 
    filter(.metric == "rmse") |> 
    pull(.estimate)
  rsq_test <- metrics_test |> 
    filter(.metric == "rsq") |> 
    pull(.estimate)
  
  
  return(c(k_val ,rsq_train, rmse_train, rsq_test, rmse_test))
  
  } 