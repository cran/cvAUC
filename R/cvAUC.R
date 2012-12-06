cvAUC <- function(predictions, labels, label.ordering = NULL, folds = NULL){
  
  require(ROCR)
  clean <- .process_input(predictions=predictions, labels=labels, label.ordering=label.ordering, folds=folds)
  pred <- prediction(clean$predictions, clean$labels)
  perf <- performance(pred, 'tpr', 'fpr')
  fold.auc <- as.numeric(performance(pred, measure = "auc", x.measure = "cutoff")@y.values)
  cv.auc <- mean(fold.auc)
  return(list(perf=perf, fold.AUC=fold.auc, cvAUC=cv.auc))
}
