# Based on Leo Guelman and Floris Devriendt
# Evaluating the performance of MTUM

# Compute the uplift score based on model predictions
uplift_score <- function(predictions,model_name,df_test,indicator_treatment,control,treatment_1,treatment_2){
  
  if(model_name == "sma" | model_name == "dia"){
    
    pr.y1_ct <- predictions[,3]
    pr.y1_t1 <- predictions[,1]
    pr.y1_t2 <- predictions[,2]
    dif.pred.t1 <- as.numeric(pr.y1_t1 - pr.y1_ct)
    dif.pred.t2 <- as.numeric(pr.y1_t2 - pr.y1_ct)
    max.dif <- pmax(dif.pred.t1,dif.pred.t2)
    pred.treat <- ifelse(max.dif < 0,0,ifelse(max.dif == dif.pred.t1,1,2))
    uplift_predictions <- cbind(max.dif,pred.treat)
    return(uplift_predictions)
    
  } else if(model == "cknn"){
    
    pr.y1_ct <- predictions[,1]
    pr.y1_t1 <- predictions[,2]
    pr.y1_t2 <- predictions[,3]
    dif.pred.t1 <- as.numeric(pr.y1_t1 - pr.y1_ct)
    dif.pred.t2 <- as.numeric(pr.y1_t2 - pr.y1_ct)
    max.dif <- pmax(dif.pred.t1,dif.pred.t2)
    pred.treat <- ifelse(max.dif < 0,0,ifelse(max.dif == dif.pred.t1,1,2))
    uplift_predictions <- cbind(max.dif,pred.treat)
    return(uplift_predictions)
    
  } else if(model == "nua"){
    
    pr.y1_ct_m1 <- predictions[,3]
    pr.y1_ct_m2 <- predictions[,4]
    pr.y1_t1 <- predictions[,1]
    pr.y1_t2 <- predictions[,2]
    dif.pred.t1 <- as.numeric(pr.y1_t1 - pr.y1_ct_m1)
    dif.pred.t2 <- as.numeric(pr.y1_t2 - pr.y1_ct_m2)
    max.dif <- pmax(dif.pred.t1,dif.pred.t2)
    pred.treat <- ifelse(max.dif < 0,0,ifelse(max.dif == dif.pred.t1,1,2))
    uplift_predictions <- cbind(max.dif,pred.treat)
    return(uplift_predictions)
    
  } else if(model == "mmoa"){
    # Predicted probabilities for the classes of interest
    pr.y0.ct <- predictions[,1]
    pr.y0.t1 <- predictions[,2]
    pr.y0.t2 <- predictions[,3]
    pr.y1.ct <- predictions[,4]
    pr.y1.t1 <- predictions[,5]
    pr.y1.t2 <- predictions[,6]
    # Probabilities of each treatment group
    obs.treat.probs <- as.data.frame(prop.table(table(df_test[,indicator_treatment])))
    obs.prob.ct <- obs.treat.probs[which(obs.treat.probs$Var1 == control),2]
    obs.prob.t1 <- obs.treat.probs[which(obs.treat.probs$Var1 == treatment_1),2]
    obs.prob.t2 <- obs.treat.probs[which(obs.treat.probs$Var1 == treatment_2),2]  
    # Calculating the model score for both treatments
    dif.pred.t1 <- (pr.y1.t1/obs.prob.t1 + pr.y0.ct/obs.prob.ct) - (pr.y0.t1/obs.prob.t1 + pr.y1.ct/obs.prob.ct)
    dif.pred.t2 <- (pr.y1.t2/obs.prob.t2 + pr.y0.ct/obs.prob.ct) - (pr.y0.t2/obs.prob.t2 + pr.y1.ct/obs.prob.ct)
    max.dif <- pmax(dif.pred.t1,dif.pred.t2)
    pred.treat <- ifelse(max.dif < 0,0,ifelse(max.dif == dif.pred.t1,1,2))
    uplift_predictions <- cbind(max.dif,pred.treat)
    return(uplift_predictions)
  
  } else if(model == "python"){
    
    dif.pred.t1 <- predictions[,2]
    dif.pred.t2 <- predictions[,3]
    max.dif <- pmax(dif.pred.t1,dif.pred.t2)
    pred.treat <- ifelse(max.dif < 0,0,ifelse(max.dif == dif.pred.t1,1,2))
    uplift_predictions <- cbind(max.dif,pred.treat)
    return(uplift_predictions)
  }
}
    
# Compute the performance table 
performance_table <- function(predictions,df_test,uplift_predictions,indicator_treatment,outcome,control,treatment_1,treatment_2){

  df_test <- df_test[,c(indicator_treatment,outcome)]
  df_performance <- cbind(df_test,predictions,uplift_predictions)
  df_performance$correct_pred <- ifelse(df_performance[,indicator_treatment] == treatment_1 & df_performance$pred.treat == 1,1, 
                                 ifelse(df_performance[,indicator_treatment] == treatment_1 & df_performance$pred.treat == 2,1,
                                 ifelse(df_performance[,indicator_treatment] == control,1,0)))
  performance.order <- order(df_performance$max.dif, decreasing = TRUE)
  performance.sorted <- df_performance[performance.order,]
  performance.sorted[,"n.treat"] <- ifelse(performance.sorted[,indicator_treatment] == treatment_1 | performance.sorted[,indicator_treatment] == treatment_2,1,0)
  performance.sorted[,"n.control"] <- ifelse(performance.sorted[,indicator_treatment] == control,1,0)
  performance.sorted[,"n.y1_treat"] <- ifelse(performance.sorted[,"n.treat"] & performance.sorted[,outcome] == 1,1,0)
  performance.sorted[,"n.y1_control"] <- ifelse(performance.sorted[,"n.control"] == 1 & performance.sorted[,outcome] == 1,1,0)
  performance.sorted[,"r.y1_treat"] <- performance.sorted$n.y1_treat/performance.sorted$n.treat
  performance.sorted[,"r.y1_control"] <- performance.sorted$n.y1_control/performance.sorted$n.control
  performance.sorted[is.na(performance.sorted)] <- 0
  performance.sorted[,"uplift"] <- performance.sorted[,"r.y1_treat"] - performance.sorted[,"r.y1_control"]
  df_performance <- subset(performance.sorted, correct_pred == 1)
  res <- cbind.data.frame(group   = 1:nrow(df_performance),
                          n.treat    = df_performance$n.treat,
                          n.control    = df_performance$n.control, 
                          n.y1_treat = df_performance$n.y1_treat, 
                          n.y1_control = df_performance$n.y1_control,
                          r.y1_treat = df_performance$r.y1_treat, 
                          r.y1_control = df_performance$r.y1_control,
                          uplift   = df_performance$uplift)
                
  #class(res) <- "performance"
  return(res)
}

# Expected response
expected_response <- function(df_test,predictions,uplift_predictions,indicator_treatment,treatment_1,treatment_2,control){
  obs.treat.probs <- as.data.frame(prop.table(table(df_test[,indicator_treatment])))
  obs.prob.ct <- obs.treat.probs[which(obs.treat.probs$Var1 == ppdf$control),2]
  obs.prob.t1 <- obs.treat.probs[which(obs.treat.probs$Var1 == ppdf$treatment_1),2]
  obs.prob.t2 <- obs.treat.probs[which(obs.treat.probs$Var1 == ppdf$treatment_2),2]
  df_predictions <- as.data.frame(predictions)
  df_predictions$dif.pred <- uplift_predictions[,1]
  df_predictions$opt.treat <- uplift_predictions[,2]
  df_predictions$obs.treat <- ifelse(df_test[,indicator_treatment] == treatment_1,1,ifelse(df_test[,indicator_treatment] == treatment_2,2,0))
  df_predictions$outcome <- df_test[,outcome]
  df_predictions$z.opt <- ((df_predictions$outcome/obs.prob.ct)*(ifelse(df_predictions$opt.treat == 0,1,0))*(ifelse(df_predictions$obs.treat == 0,1,0)))+
                          ((df_predictions$outcome/obs.prob.t1)*(ifelse(df_predictions$opt.treat == 1,1,0))*(ifelse(df_predictions$obs.treat == 1,1,0)))+
                          ((df_predictions$outcome/obs.prob.t2)*(ifelse(df_predictions$opt.treat == 2,1,0))*ifelse(df_predictions$obs.treat == 2,1,0))
  df_predictions$z.ct <- (df_predictions$outcome/obs.prob.ct)*ifelse(df_predictions$obs.treat==0,1,0)
  predictions_order <- order(df_predictions$dif.pred,decreasing = TRUE)
  predictions_sorted <- df_predictions[predictions_order,]
  average.expected.response <- c()
  for (x in 1:(nrow(predictions_sorted)-1)){
    average.expected.response[x] <- (sum(predictions_sorted[1:x,"z.opt"])+sum(predictions_sorted[(x+1):nrow(predictions_sorted),"z.ct"]))/nrow(predictions_sorted)
  }
  all.control <- mean(predictions_sorted$z.ct)
  all.opt.treat <- mean(predictions_sorted$z.opt)
  average.expected.response <- c(all.control,average.expected.response,all.opt.treat)
  res <- data.frame(x = seq(0,length(average.expected.response)-1),z=average.expected.response)
  return(res)
}
