# custom metric evaluation 


# pass this into feval for xgboost. i.e feval = custom_metric 
# dtrain is your xgb.Dmatrix for train data set 
# change mae function to any other evaluation metric 
# note  format = watchlist = list(train = d_train,val = d_test)

library(Metrics)

custom_metric <- function(preds, dtrain) {
  
  labels <- xgboost::getinfo(dtrain, "label")
  elab <- as.numeric(labels)
  epreds <- as.numeric(preds)
  err <- mae(elab, epreds)
  
  return(list(metric = "custom_metric", value = err))
  
}
