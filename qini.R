# Adapted version by Floris Devriendt from Leo Guelman

qini <- function(x, direction = 1, plotit = TRUE, title="",...) {
   
  perf <- x
  groups <- nrow(perf)
  r.cumul.y1_treat <- cumsum(perf[,"n.y1_treat"]) / cumsum(perf[,"n.treat"])
  r.cumul.y1_control <- cumsum(perf[,"n.y1_control"]) / cumsum(perf[,"n.control"])
  r.cumul.y1_treat[is.na(r.cumul.y1_treat)] <- 0
  r.cumul.y1_control[is.na(r.cumul.y1_control)] <- 0
  perf <- cbind(perf,r.cumul.y1_treat,r.cumul.y1_control) 
  if (direction == 1) {
    inc.gains <- c(0, (r.cumul.y1_treat - r.cumul.y1_control) * ((cumsum(perf[,2]) + cumsum(perf[,3]))/(sum(perf[,2]) + sum(perf[,3]))))
    overall.inc.gains <- sum(perf[, "n.y1_ct1"]) / sum(perf[, "n.ct1"]) - sum(perf[, "n.y1_ct0"]) / sum(perf[, "n.ct0"])  
  } else {
    
    ### Model Incremental gains 
    # inc.gains <- c(0, (r.cumul.y1_ct0 - r.cumul.y1_ct1) * deciles)
    # inc.gains <- c(0,(cumsum(perf[, 5])  / cumsum(perf[, 3]) - cumsum(perf[, 4]) / cumsum(perf[, 2])) * ((cumsum(perf[,2]) + cumsum(perf[,3]))/(sum(perf[,2]) + sum(perf[,3]))))
    inc.gains <- c(0, (r.cumul.y1_ct0 - r.cumul.y1_ct1) * ((cumsum(perf[,2]) + cumsum(perf[,3]))/(sum(perf[,2]) + sum(perf[,3]))))
    
    ### Overall incremental gains
    overall.inc.gains <- sum(perf[, "n.y1_ct0"]) / sum(perf[, "n.ct0"]) - sum(perf[, "n.y1_ct1"]) / sum(perf[, "n.ct1"]) 
    
  }
  
  ### Random incremental gains
  random.inc.gains <- c(0, cumsum(rep(overall.inc.gains / groups, groups)))
  
  ### Compute area under the model incremental gains (uplift) curve 
  x <- c(0.0, seq(1 / groups, 1, 1 / groups))
  y <- inc.gains
  
  auuc <- 0
  auuc.rand <- 0
  
  for (i in 2:length(x)) {
    auuc <- auuc + 0.5 * (x[i] - x[i-1]) * (y[i] + y[i-1])
  }
  
  ### Compute area under the random incremental gains curve
  y.rand <- random.inc.gains
  
  for (i in 2:length(x)) {
    auuc.rand <- auuc.rand + 0.5 * (x[i] - x[i-1]) * (y.rand[i] + y.rand[i-1])
  }
  
  ### Compute the difference between the areas (Qini coefficient)
  Qini <- auuc - auuc.rand
  miny <- 100 * min(c(random.inc.gains, inc.gains))
  maxy <- 100 * max(c(random.inc.gains, inc.gains))
  
  if(plotit){
    plot(inc.gains * 100 ~ c(0, seq(100 / groups, 100, 100 / groups)), type ="l",
         col = "blue", lty = 1, xlab = "Proportion of population targeted (%)", 
         ylab = "Cumulative incremental gains (pc pt)", ylim = c(miny, maxy),
         main = title)
    lines(random.inc.gains * 100 ~ c(0, seq(100 / groups, 100, 100 / groups)), type = "l", col = "red", lty = 1)
    legend("topright", c("Model", "Random"), 
           col=c("blue", "red"), lty=c(1,1))
  }
  
  res <- list(Qini = Qini,
              inc.gains = inc.gains,
              random.inc.gains = random.inc.gains)
  
  return(res)
  
}
