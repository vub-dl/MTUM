# Evaluating the performance of MTUM

# Compute the uplift score based on model predictions

uplift_score <- function(predictions,model_name,df_test){
  
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
    
  } else if(model == "knn"){
    pr.y1_ct0 <- predictions[,1]
    pr.y1_ct1 <- predictions[,2]
    pr.y1_ct2 <- predictions[,3]
    
    dif.pred.t1 <- as.numeric(pr.y1_ct1 - pr.y1_ct0)
    dif.pred.t2 <- as.numeric(pr.y1_ct2 - pr.y1_ct0)
    
    max.dif <- pmax(dif.pred.t1,dif.pred.t2)
    pred.treat <- ifelse(max.dif < 0,0,ifelse(max.dif == dif.pred.t1,1,2))
    
    model.uplift <- cbind(max.dif,pred.treat)
    
    return(model.uplift)
    
  } else if(model == "naive uplift"){
    pr.y1_ct01 <- predictions[,3]
    pr.y1_ct02 <- predictions[,4]
    pr.y1_ct1 <- predictions[,1]
    pr.y1_ct2 <- predictions[,2]
    
    dif.pred.t1 <- as.numeric(pr.y1_ct1 - pr.y1_ct01)
    dif.pred.t2 <- as.numeric(pr.y1_ct2 - pr.y1_ct02)
    
    max.dif <- pmax(dif.pred.t1,dif.pred.t2)
    pred.treat <- ifelse(max.dif < 0,0,ifelse(max.dif == dif.pred.t1,1,2))
    
    model.uplift <- cbind(max.dif,pred.treat)
    
    return(model.uplift)
    
  } else if(model == "multiClass"){
    # Predicted probabilities for the classes of interest
    pr.nr.ct0 <- predictions[,1]
    pr.nr.ct1 <- predictions[,2]
    pr.nr.ct2 <- predictions[,3]
    pr.y1.ct0 <- predictions[,4]
    pr.y1.ct1 <- predictions[,5]
    pr.y1.ct2 <- predictions[,6]
    
    # Probabilities of each treatment group
    obs.treat.probs <- as.data.frame(prop.table(table(df.test[,ppdf$treatmentIndicator])))
    obs.prob.ct0 <- obs.treat.probs[which(obs.treat.probs$Var1 == ppdf$treatmentVariable3),2]
    obs.prob.ct1 <- obs.treat.probs[which(obs.treat.probs$Var1 == ppdf$treatmentVariable1),2]
    obs.prob.ct2 <- obs.treat.probs[which(obs.treat.probs$Var1 == ppdf$treatmentVariable2),2]
    
    # Calculating the model score for both treatments
    dif.pred.t1 <- (pr.y1.ct1/obs.prob.ct1 + pr.nr.ct0/obs.prob.ct0) - (pr.nr.ct1/obs.prob.ct1 + pr.y1.ct0/obs.prob.ct0)
    dif.pred.t2 <- (pr.y1.ct2/obs.prob.ct2 + pr.nr.ct0/obs.prob.ct0) - (pr.nr.ct2/obs.prob.ct2 + pr.y1.ct0/obs.prob.ct0)
    
    max.dif <- pmax(dif.pred.t1,dif.pred.t2)
    pred.treat <- ifelse(max.dif < 0,0,ifelse(max.dif == dif.pred.t1,1,2))
    
    model.uplift <- cbind(max.dif,pred.treat)
    
    return(model.uplift)
  } else if(model == "python"){
    dif.pred.t1 <- predictions[,2]
    dif.pred.t2 <- predictions[,3]
    max.dif <- pmax(dif.pred.t1,dif.pred.t2)
    pred.treat <- ifelse(max.dif < 0,0,ifelse(max.dif == dif.pred.t1,1,2))
    model.uplift <- cbind(max.dif,pred.treat)
    return(model.uplift)
  }
}
