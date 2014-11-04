AUC <- function(predictions, labels) { 
  
  if (length(unique(labels)) != 2) {
    stop("AUC only implemented for binary response")
  }
  if (length(predictions) != length(labels)) {
    stop("predictions and labels must be equal length")
  }     
  if (class(labels) == "factor") {
    labels <- as.integer(labels)-1
    warning("labels converted from factor to 0/1")
  }
  if (max(predictions)>1 | min(predictions)<0) {
    if(is.numeric(predictions)==TRUE){
      predictions[which(predictions<0)] <- 0
      predictions[which(predictions>1)] <- 1	
      warning("predictions have been truncated at [0,1]")	
    } else {stop("predictions must be numeric")}
  }
  
  pred <- ROCR::prediction(predictions, labels)
  perf <- ROCR::performance(pred,"tpr", "fpr")
  auc <- as.numeric(ROCR::performance(pred, measure = "auc", x.measure = "cutoff")@y.values)
  return(auc)
}
