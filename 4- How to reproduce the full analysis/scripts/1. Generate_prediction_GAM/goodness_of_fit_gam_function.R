################################################################################
# Title:        Goodness of fit function for GAM prediction 
# Description:  Generates a function to assess the goodness of fit of GAM models for both training and test datasets
#               Evaluates, for benthic and pelagic separatedly correlations between observation and predictions, RMSE,
#               and AIC values & explained variance for the full model
# Date:         2025-05-05
# Version:      6.0
# Notes:        Any additional information or context
# Dependencies: mgcv
################################################################################

goodness_of_fit_gam <- function(GAM_model, response, test_df, Hab_col, trophic_col) {
  require("mgcv")
  require("dplyr")
  
  df_train_1 <- data.frame(
    obs_values = GAM_model$model[, 1],
    pred_val = GAM_model$fitted.values,
    Hab = GAM_model$model[, Hab_col],
    trophic = GAM_model$model[, trophic_col],
    dataset = rep("train", nrow(GAM_model$model))
  )
  
  obs_values <- test_df[, which(colnames(test_df) == response)]
  
  df_test_1 <- data.frame(
    obs_values = obs_values,
    pred_val = predict(GAM_model, newdata = test_df),
    Hab = test_df$Hab,
    trophic = test_df$Trophic,
    dataset = rep("test", nrow(test_df))
  )
  names(df_test_1) <- names(df_train_1)
  df_all <- rbind(df_train_1, df_test_1)
  
  df_train_benthic <- filter(df_train_1, Hab == "Benthic")
  df_train_pelagic <- filter(df_train_1, Hab == "Pelagic")
  df_test_benthic <- filter(df_test_1, Hab == "Benthic")
  df_test_pelagic <- filter(df_test_1, Hab == "Pelagic")
  
  sum <- summary(GAM_model)
  expl_dev_mod <- sum$dev.expl
  AIC_mod <- GAM_model$aic
  
  safe_cor_test <- function(x, y) {
    result <- tryCatch({
      if (length(na.omit(x)) >= 3 && length(na.omit(y)) >= 3) {
        cor.test(x, y)
      } else {
        list(estimate = NA, p.value = NA)
      }
    }, error = function(e) {
      list(estimate = NA, p.value = NA)
    })
    return(result)
  }
  
  safe_rmse <- function(pred, obs) {
    if (length(na.omit(obs)) == 0 || length(na.omit(pred)) == 0) return(NA)
    return(sqrt(mean((pred - obs)^2, na.rm = TRUE)))
  }
  
  # Correlations
  cor1_test <- safe_cor_test(df_test_benthic$obs_values, df_test_benthic$pred_val)
  cor1_train <- safe_cor_test(df_train_benthic$obs_values, df_train_benthic$pred_val)
  
  cor2_test <- safe_cor_test(df_test_pelagic$obs_values, df_test_pelagic$pred_val)
  cor2_train <- safe_cor_test(df_train_pelagic$obs_values, df_train_pelagic$pred_val)
  
  # RMSEs
  RMSE1_test <- safe_rmse(df_test_benthic$pred_val, df_test_benthic$obs_values)
  RMSE1_train <- safe_rmse(df_train_benthic$pred_val, df_train_benthic$obs_values)
  RMSE2_test <- safe_rmse(df_test_pelagic$pred_val, df_test_pelagic$obs_values)
  RMSE2_train <- safe_rmse(df_train_pelagic$pred_val, df_train_pelagic$obs_values)
  
  GoF_results <- data.frame(
    response = response,
    dataset = "PC and PR",
    expl_dev = expl_dev_mod,
    AIC = AIC_mod,
    cor_Benthic_train = cor1_train$estimate,
    RMSE_Benthic_train = RMSE1_train,
    cor_Benthic_test = cor1_test$estimate,
    RMSE_Benthic_test = RMSE1_test,
    cor_Pelagic_train = cor2_train$estimate,
    RMSE_Pelagic_train = RMSE2_train,
    cor_Pelagic_test = cor2_test$estimate,
    RMSE_Pelagic_test = RMSE2_test
  )
  
  return(list("df_all" = df_all, "GoF_results" = GoF_results))
}
