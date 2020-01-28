# Multitreatment uplift modeling approaches
# Based on single treatment uplift modeling by Floris Devriendt

model_mtum <- function(df_train,
                         treatment_1,
                         treatment_2, 
                         outcome, 
                         outcome_positive, 
                         outcome_negative, 
                         features,
                         model){
  seed <- 100
  
  if (model == "SMALR"){

    # Factorize outcome variable
    df_train[,outcome] <- factor(df_train[,outcome])
    levels(df_train[,outcome]) <- c(outcome_negative, outcome_positive)
    # DF treatment_1
    df_treatment_1 <- subset(df_train, df_train[,treatment_1] == 1)
    # DF treatment_2
    df_treatment_2 <- subset(df_train, df_train[,treatment_2] == 1)
    # DF control
    df_control <- subset(df_train, df_train[,treatment_1] == 0 & df_train[,treatment_2] == 0)
    # Training
      set.seed(seed)
      ctrl <- trainControl(method = "none", summaryFunction = twoClassSummary, classProbs = TRUE, savePredictions = TRUE)
      # Model treatment 1
      set.seed(seed)
      model_treatment_1 <- train(df_treatment_1[,features], df_treatment_1[,outcome], method="glmStepAIC", family=binomial(), metric="ROC", trControl=ctrl)
      # Model treatment 2
      set.seed(seed)
      model_treatment_2 <- train(df_treatment_2[,features], df_treatment_2[,outcome], method="glmStepAIC", family=binomial(), metric="ROC", trControl=ctrl)
      # Model control
      set.seed(seed)
      model_control <- train(df_control[,features], df_control[,outcome], method="glmStepAIC", family=binomial(), metric="ROC", trControl=ctrl)
      
      ans <- list(model_t1 = model_treatment_1,
                  model_t2 = model_treatment_2,
                  model_control = model_control)
    return(ans)
  
  } else if (model == "SMARF"){

    # Factorize outcome variable
    df_train[,outcome] <- factor(df_train[,outcome])
    levels(df_train[,outcome]) <- c(outcome_negative, outcome_positive)
    # DF treatment_1
    df_treatment_1 <- subset(df_train, df_train[,treatment_1] == 1)
    # DF treatment_2
    df_treatment_2 <- subset(df_train, df_train[,treatment_2] == 1)
    # DF control
    df_control <- subset(df_train, df_train[,treatment_1] == 0 & df_train[,treatment_2] == 0)
    # Training
      set.seed(seed)
      ctrl <- trainControl(method = "none", summaryFunction = twoClassSummary, classProbs = TRUE, savePredictions = TRUE)
      # Model treatment 1
      set.seed(seed)
      model_treatment_1 <- train(df_treatment_1[,features], df_treatment_1[,outcome], method="rf", ntree = 500, metric="ROC", trControl=ctrl)
      # Model treatment 2
      set.seed(seed)
      model_treatment_2 <- train(df_treatment_2[,features], df_treatment_2[,outcome], method="rf", ntree = 500, metric="ROC", trControl=ctrl)
      # Model control
      set.seed(seed)
      model_control <- train(df_control[,features], df_control[,outcome], method="rf", ntree = 500, metric="ROC", trControl=ctrl)
  
    ans <- list(model_t1 = model_treatment_1,
                model_t2 = model_treatment_2,
                model_control = model_control)
    return(ans)
              
  } else if (model == "DIALR"){
  
    # Factorize outcome variable
    df_train[,outcome] <- factor(df_train[,outcome])
    levels(df_train[,outcome]) <- c(outcome_negative, outcome_positive)
    # Feature selection
      if(length(features) <= 5){
        features <- features
      } else if(length(features) > 5){
        # DF treatment_1
        df_treatment_1 <- subset(df_train, df_train[,treatment_1] == 1)
        # DF treatment_2
        df_treatment_2 <- subset(df_train, df_train[,treatment_2] == 1)
        # DF control
        df_control <- subset(df_train, df_train[,treatment_1] == 0 & df_train[,treatment_2] == 0)
        set.seed(seed)
        ctrl <- trainControl(method = "none", summaryFunction = twoClassSummary, classProbs = TRUE, savePredictions = TRUE)
        set.seed(seed)
        model_treatment_1 <- train(df_treatment_1[,features], df_treatment_1[,outcome], method="glmStepAIC", family=binomial(), metric="ROC", trControl=ctrl)
        final_model_treatment_1 <- model_treatment_1[["finalModel"]][["formula"]][[3]]
        features_model_treatment_1 <- all.vars(final_model_treatment_1)
        set.seed(seed)
        model_treatment_2 <- train(df_treatment_2[,features], df_treatment_2[,outcome], method="glmStepAIC", family=binomial(), metric="ROC", trControl=ctrl)
        final_model_treatment_2 <- model_treatment_2[["finalModel"]][["formula"]][[3]]
        features_model_treatment_2 <- all.vars(final_model_treatment_2)
        features <- union(features_model_treatment_1, features_model_treatment_2)
        set.seed(seed)
        model_control <- train(df_control[,features], df_control[,outcome], method="glmStepAIC", family=binomial(), metric="ROC", trControl=ctrl)
        final_model_control <- model_control[["finalModel"]][["formula"]][[3]]
        features_model_treatment_control <- all.vars(final_model_control)
        features <- union(features, features_model_treatment_control)
      }
    # Interactions 
      # Treatment 1
      xt1 <- df_train[,features] * df_train[,treatment_1]
      colnames(xt1) <- paste("Inter1", colnames(xt1), sep = "_")
      # Treatment 2
      xt2 <- df_train[,features] * df_train[,treatment_2]
      colnames(xt2) <- paste("Inter2", colnames(xt2), sep = "_")
      df_interactions <- cbind(df_train[,c(features,treatment_1,treatment_2, outcome)], xt1, xt2)
    # Features
      features <- c(features, colnames(xt1), colnames(xt2), treatment_1, treatment_2)
    # Training
      set.seed(seed)
      ctrl <- trainControl(method = "none", summaryFunction = twoClassSummary, classProbs = TRUE, savePredictions = TRUE)
      set.seed(seed)
      model <- train(df_interactions[,features], df_interactions[,outcome], method="glm", family=binomial(), metric="ROC", trControl=ctrl)
      ans <- list(model)  
      return(ans)
    
    } else if (model == "DIARF"){
    
    # Factorize outcome variable
    df_train[,outcome] <- factor(df_train[,outcome])
    levels(df_train[,outcome]) <- c(outcome_negative, outcome_positive)
    # Feature selection
      if(length(features) <= 5){
        features <- features
      } else if(length(features) > 5){
        # DF treatment_1
        df_treatment_1 <- subset(df_train, df_train[,treatment_1] == 1)
        # DF treatment_2
        df_treatment_2 <- subset(df_train, df_train[,treatment_2] == 1)
        # DF control
        df_control <- subset(df_train, df_train[,treatment_1] == 0 & df_train[,treatment_2] == 0)
        set.seed(seed)
        ctrl <- trainControl(method = "none", summaryFunction = twoClassSummary, classProbs = TRUE, savePredictions = TRUE)
        set.seed(seed)
        model_treatment_1 <- train(df_treatment_1[,features], df_treatment_1[,outcome], method="glmStepAIC", family=binomial(), metric="ROC", trControl=ctrl)
        final_model_treatment_1 <- model_treatment_1[["finalModel"]][["formula"]][[3]]
        features_model_treatment_1 <- all.vars(final_model_treatment_1)
        set.seed(seed)
        model_treatment_2 <- train(df_treatment_2[,features], df_treatment_2[,outcome], method="glmStepAIC", family=binomial(), metric="ROC", trControl=ctrl)
        final_model_treatment_2 <- model_treatment_2[["finalModel"]][["formula"]][[3]]
        features_model_treatment_2 <- all.vars(final_model_treatment_2)
        features <- union(features_model_treatment_1, features_model_treatment_2)
        set.seed(seed)
        model_control <- train(df_control[,features], df_control[,outcome], method="glmStepAIC", family=binomial(), metric="ROC", trControl=ctrl)
        final_model_control <- model_control[["finalModel"]][["formula"]][[3]]
        features_model_treatment_control <- all.vars(final_model_control)
        features <- union(features, features_model_treatment_control)
      }
     # Interactions 
      # Treatment 1
      xt1 <- df_train[,features] * df_train[,treatment_1]
      colnames(xt1) <- paste("Inter1", colnames(xt1), sep = "_")
      # Treatment 2
      xt2 <- df_train[,features] * df_train[,treatment_2]
      colnames(xt2) <- paste("Inter2", colnames(xt2), sep = "_")
      df_interactions <- cbind(df_train[,c(features,treatment_1,treatment_2, outcome)], xt1, xt2)
    # Features
      features <- c(features, colnames(xt1), colnames(xt2), treatment_1, treatment_2)
    # Training
      set.seed(seed)
      ctrl <- trainControl(method = "none", summaryFunction = twoClassSummary, classProbs = TRUE, savePredictions = TRUE)
      set.seed(seed)
      model <- train(df_interactions[,features], df_interactions[,outcome], method="rf", ntree = 500, metric="ROC", trControl=ctrl)
      ans <- model 
      return(ans)
      
   } else if (model == "CKNN"){
      
     # Factorize outcome variable
     df_train[,outcome] <- factor(df_train[,outcome])
     levels(df_train[,outcome]) <- c(outcome_negative, outcome_positive)
     # Treatment indicator in training set
     df_train$indicator_treatment <- ifelse(df_train[,treatment_1] == 1, 1, ifelse(df_train[,treatment_2] == 1, 2, 0))
     df_train$indicator_treatment <- as.factor(df_train$indicator_treatment)
     # Treatment indicator in test set
     df_test$indicator_treatment <- ifelse(df_test[,treatment_1] == 1, 1, ifelse(df_test[,treatment_2] == 1, 2, 0))
     df_test$indicator_treatment <- as.factor(df_test$indicator_treatment)
     # Training
     set.seed(seed)
     model <- upliftKNN(df_train[,features], df_test[,features], df_train[,"outcome"], df_train[,"indicator_treatment"], k = 10)
     ans <- model
     return(ans)
   
   } else if (model == "NUARF"){
   
    # Treatment 1 vs. control
    df_t1 <- df_train
    df_t1 <- subset(df_t1, df_t1[,treatment_1] == 1 | df_t1[,treatment_2] == 0)
    variables_t1 <- c(features, treatment_1, outcome)
    df_t1 <- df_t1[,variables_t1]
    t1 <- df_t1[,treatment_1]
    y1 <- df_t1[,outcome]
    # Treatment 2 vs. control
    df_t2 <- df_train
    df_t2 <- subset(df_t2, df_t2[,treatment_1] == 0 | df_t2[,treatment_2] == 1)
    variables_t2 <- c(features, treatment_2, outcome)
    df_t2 <- df_t2[,variables_t2]
    t2 <- df_t2[,treatment_2]
    y2 <- df_t2[,outcome]
    # Training
    # Formula Treatment 1 vs. control
    formula_t1 <- as.formula(paste("y1 ~", "trt(t1) +", paste(features, sep = "", collapse = "+")))
    formula_t2 <- as.formula(paste("y2 ~", "trt(t2) +", paste(features, sep = "", collapse = "+")))
    set.seed(seed)
    model_t1 <- upliftRF(formula_t1, data = df_t1, ntree = 500, split_method = "KL")
    set.seed(seed)
    model_t2 <- upliftRF(formula_t2, data = df_t2, ntree = 500, split_method = "KL")
    
    ans <- list(model_t1 = model_t1, 
                model_t2 = model_t2)
    return(ans)
    
    } else if (model == "NUACCIF"){
     
    # Treatment 1 vs. control
    df_t1 <- df_train
    df_t1 <- subset(df_t1, df_t1[,treatment_1] == 1 | df_t1[,treatment_2] == 0)
    variables_t1 <- c(features, treatment_1, outcome)
    df_t1 <- df_t1[,variables_t1]
    t1 <- df_t1[,treatment_1]
    y1 <- df_t1[,outcome]
    # Treatment 2 vs. control
    df_t2 <- df_train
    df_t2 <- subset(df_t2, df_t2[,treatment_1] == 0 | df_t2[,treatment_2] == 1)
    variables_t2 <- c(features, treatment_2, outcome)
    df_t2 <- df_t2[,variables_t2]
    t2 <- df_t2[,treatment_2]
    y2 <- df_t2[,outcome]
    # Training
    # Formula Treatment 1 vs. control
    formula_t1 <- as.formula(paste("y1 ~", "trt(t1) +", paste(features, sep = "", collapse = "+")))
    formula_t2 <- as.formula(paste("y2 ~", "trt(t2) +", paste(features, sep = "", collapse = "+")))
    set.seed(seed)
    model_t1 <- ccif(formula_t1, data = df_t1, ntree = 500, split_method = "ED", pvalue = 0.01)
    set.seed(seed)
    model_t2 <- ccif(formula_t2, data = df_t2, ntree = 500, split_method = "ED", pvalue = 0.01)
    
    ans <- list(model_t1 = model_t1, 
                model_t2 = model_t2)
    return(ans)
                
    } else if (model == "MMOALR"){   
    
    formula <- as.formula(paste("multi_outcome ~", paste(features, sep = "", collapse = "+")))
    set.seed(seed)
    model <- nnet::multinom(formula, data = df_train)
    ans <- model
    return(ans)
    
    } else if (model == "MMOARF"){ 
    
    set.seed(seed)
    formula <- as.formula(paste("multi_outcome ~", paste(features, sep = "", collapse = "+")))
    df_train$multi_outcome <- as.factor(df_train$multi_outcome)
    model <- randomForest::randomForest(formula, data = df_train,ntree = 500)
    ans <- model                                 
    return(ans)
    }
}
