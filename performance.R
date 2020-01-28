# Evaluating the performance of MTUM

# Compute the uplift score based on model predictions

uplift_score <- function(predictions,model_name,df_test,control,treatment_1,treatment_2){
  
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
