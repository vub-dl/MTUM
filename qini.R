# Adapted version by Floris Devriendt from Leo Guelman
qini <- function(x, direction) {
   
  perf <- x
  groups <- nrow(perf)
  r.cumul.y1_treat <- cumsum(perf[,"n.y1_treat"]) / cumsum(perf[,"n.treat"])
  r.cumul.y1_control <- cumsum(perf[,"n.y1_control"]) / cumsum(perf[,"n.control"])
  r.cumul.y1_treat[is.na(r.cumul.y1_treat)] <- 0
  r.cumul.y1_control[is.na(r.cumul.y1_control)] <- 0
  perf <- cbind(perf,r.cumul.y1_treat,r.cumul.y1_control) 
  if (direction == 1) {
    inc.gains <- c(0, (r.cumul.y1_treat - r.cumul.y1_control) * ((cumsum(perf[,2]) + cumsum(perf[,3]))/(sum(perf[,2]) + sum(perf[,3]))))
    overall.inc.gains <- sum(perf[, "n.y1_treat"]) / sum(perf[, "n.treat"]) - sum(perf[, "n.y1_control"]) / sum(perf[, "n.control"])  
  } else {
    inc.gains <- c(0, (r.cumul.y1_control - r.cumul.y1_treat) * ((cumsum(perf[,2]) + cumsum(perf[,3]))/(sum(perf[,2]) + sum(perf[,3]))))
    overall.inc.gains <- sum(perf[, "n.y1_control"]) / sum(perf[, "n.control"]) - sum(perf[, "n.y1_treat"]) / sum(perf[, "n.treat"])  
  }
  random.inc.gains <- c(0, cumsum(rep(overall.inc.gains / groups, groups)))
  x <- c(0.0, seq(1 / groups, 1, 1 / groups))
  y <- inc.gains
  auuc <- 0
  auuc.rand <- 0
  for (i in 2:length(x)) {
    auuc <- auuc + 0.5 * (x[i] - x[i-1]) * (y[i] + y[i-1])
  }
  y.rand <- random.inc.gains
  for (i in 2:length(x)) {
    auuc.rand <- auuc.rand + 0.5 * (x[i] - x[i-1]) * (y.rand[i] + y.rand[i-1])
  }
  Qini <- auuc - auuc.rand
  miny <- 100 * min(c(random.inc.gains, inc.gains))
  maxy <- 100 * max(c(random.inc.gains, inc.gains))
  res <- list(Qini = Qini,
              inc.gains = inc.gains,
              random.inc.gains = random.inc.gains)
  return(res)
}
