# custom caret evaluation metric



# For caret, the custom summary is to be passed into summaryFunction = custom_summary 

library(Metric) 

###################
# some mae example 
###################

mae_summary = function(data, lev = NULL, model = NULL){
  #out = accuracy(data[, "obs"], data[, "pred"])[5]
  #out = accuracy(data$obs, data$pred)[5]
  #out = mae(data[, "obs"], data[, "pred"])
  out = mae(data$obs,data$pred)
  names(out) = c("mae")
  out
}



###################
# some R^2 example 
###################
# used in mercedes kaggle 



rs_summary = function(data, lev = NULL, model = NULL){
    out = 1 - (sum((data[, "obs"]- data[, "pred"] )^2)/sum((data[, "obs"]-mean(data[, "obs"]))^2))
    names(out) = c("rsquare")
    out
}








########################################
# some classification example : logloss
########################################


LogLosSummary <- function (data, lev = NULL, model = NULL) {
  
  # you can define your own function here or use metric library 
  
  LogLos <- function(actual, pred, eps = 1e-15) {
    stopifnot(all(dim(actual) == dim(pred)))
    pred[pred < eps] <- eps
    pred[pred > 1 - eps] <- 1 - eps
    -sum(actual * log(pred)) / nrow(pred) 
  }
  
  
  
  if (is.character(data$obs)) data$obs <- factor(data$obs, levels = lev)
  pred <- data[, "pred"]
  obs <- data[, "obs"]
  isNA <- is.na(pred)
  pred <- pred[!isNA]
  obs <- obs[!isNA]
  data <- data[!isNA, ]
  cls <- levels(obs)
  
  if (length(obs) + length(pred) == 0) {
    out <- rep(NA, 2)
  } else {
    pred <- factor(pred, levels = levels(obs))
    require("e1071")
    out <- unlist(e1071::classAgreement(table(obs, pred)))[c("diag",                                                                                                                                                             "kappa")]
    
    probs <- data[, cls]
    actual <- model.matrix(~ obs - 1)
    out2 <- LogLos(actual = actual, pred = probs)
  }
  out <- c(out, out2)
  names(out) <- c("Accuracy", "Kappa", "LogLoss")
  
  if (any(is.nan(out))) out[is.nan(out)] <- NA 
  
  out
}


