# Multitreatment uplift modeling prediction
# Based on single treatment uplift modeling by Floris Devriendt

predict_mtum <- function(approach,df_test,outcome,features,outcome_positive,outcome_negative, treatment_1,treatment_2,model){
  
  if(approach == "sma"){
  
    # Factorize outcome variable
    df_test[,outcome] <- factor(df_test[,outcome)
    levels(df_test[,outcome]) <- c(outcome_negative, outcome_positive)
    # Scores control
    pred_control <- extractProb(list(model$model_control), testX = df_test[,features],testY = df_test[,outcome])
    pred_control <- pred_control[pred_control$dataType == "Test",]
    # Scores treatment 1
    pred_t1 <- extractProb(list(model$model_t1), testX = df_test[,features],testY = df_test[,outcome])
    pred_t1 <- pred_t1[pred_t1$dataType == "Test",]
    # Scores treatment 2
    pred_t2 <- extractProb(list(model$model_t2), testX = df_test[,features],testY = df_test[,outcome])
    pred_t2 <- pred_t2[pred_t2$dataType == "Test",]
    # Df predictions
    predictions = data.frame(treatment_1 =numeric(nrow(pred_t1)),
                             treatment_2 =numeric(nrow(pred_t2)),
                             control=numeric(nrow(pred_control)))
    predictions$treatment_1 <- pred_t1[,outcome_positive]
    predictions$treatment_2 <- pred_t2[,outcome_positive]
    predictions$control <- pred_control[,outcome_positive]
    return(predictions)                               
    
  } else if (approach == "dia"){
  
    # Factorize outcome variable
    df_test[,outcome] <- factor(df_test[,outcome)
    levels(df_test[,outcome]) <- c(outcome_negative, outcome_positive)
    # Scores control
    df_test_control <- df_test
    df_test_control[, treatment_1] <- 0
    df_test_control[, treatment_2] <- 0
    xt1_test_control <- df_test_control[,features] * df_test_control[,treatment_1]
    colnames(xt1_test_control) <- paste("Inter1", colnames(xt1_test_control), sep = "_")
    xt2_test_control <- df_test_control[,features] * df_test_control[,treatment_2]
    colnames(xt2_test_control) <- paste("Inter2", colnames(xt2_test_control), sep = "_")
    df_test_control_inter <- cbind(df_test_control,xt1_test_control,xt2_test_control)
    features_interaction <- c(features, colnames(xt1_test_control), colnames(xt2_test_control), treatment_1, treatment_2)
    pred_control <- extractProb(list(model), testX = df_test_control_inter[,features_interaction], testY = df_test_control_inter[,outcome])
    pred_control <- pred_control[pred_control$dataType == "Test",]
    # Scores treatment 1
    df_test_t1 <- df_test
    df_test_t1[,treatment_1] <- 1
    df_test_t1[,treatment_2] <- 0
    xt1_test_t1 <- df_test_t1[,features] * df_test_t1[,treatment_1]
    colnames(xt1_test_t1) <- paste("Inter1", colnames(xt1_test_t1), sep = "_")
    xt2_test_t1 <- df_test_t1[,features] * df_test_t1[,treatment_2]
    colnames(xt2_test_t1) <- paste("Inter2", colnames(xt2_test_t1), sep = "_")
    df_test_t1_inter <- cbind(df_test_t1, xt1_test_t1, xt2_test_t1)
    pred_t1 <- extractProb(list(model), testX = df_test_t1_inter[,features_interaction],testY = df_test_t1_inter[,outcome])
    pred_t1 <- pred_t1[pred_t1$dataType == "Test",]
    # Scores treatment 2
    df_test_t2 <- df_test
    df_test_t2[, treatment_1] <- 0
    df_test_t2[, treatment_2] <- 1
    xt1_test_t2 <- df_test_t2[,features] * df_test_t2[,treatment_1]
    colnames(xt1_test_t2) <- paste("Inter1", colnames(xt1_test_t2), sep = "_")
    xt2_test_t2 <- df_test_t2[,features] * df_test_t2[,treatment_2]
    colnames(xt2_test_t2) <- paste("Inter2", colnames(xt2_test_t2), sep = "_")
    df_test_t2_inter <- cbind(df_test_t2, xt1_test_t2, xt2_test_t2)
    pred_t2 <- extractProb(list(model), testX = df_test_t2_inter[,features_interaction],testY = df_test_t2_inter[,outcome])
    pred_t2 <- pred_t2[pred_t2$dataType == "Test",]
    predictions = data.frame(treatment_1=numeric(nrow(pred_t1)),
                             treatment_2=numeric(nrow(pred_t2)),
                             control=numeric(nrow(pred_control)))
    predictions$treatment_1 <- pred_t1[,outcome_positive]
    predictions$treatment_2 <- pred_t2[,outcome_positive]
    predictions$control <- pred_control[,outcome_positive]
    return(predictions)   
                                        
  } else if (approach == "nua"){
    
    # Scores treatment 1 vs. control
    pred_t1 <- predict(model$model_t1,df_test[,features])
    # Scores treatment 2 vs. control
    pred_t2 <- predict(model$model_t2,df_test[,features])
    predictions = data.frame(treatment_1=numeric(nrow(pred_t1)),
                             treatment_2=numeric(nrow(pred_t2)),
                             control_t1=numeric(nrow(pred_t1)),
                             control_t2=numeric(nrow(pred_t2)))
    predictions$treatment_1 <- pred_t1[,1]
    predictions$treatment_2 <- pred_t2[,1]
    predictions$control_t1 <- pred_t1[,2]
    predictions$control_t2 <- pred_t2[,2]
    return(predictions)   
    
  } else if (approach == "mmoa"){
  
    predictions <- function(model,df_test){
    predictions <- car::Predict(model, df_test, type = "prob")
    return(predictions)   
  }
}

