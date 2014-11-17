ci.cvAUC <- function(predictions, labels, label.ordering = NULL, folds = NULL, confidence = 0.95) {

  clean <- .process_input(predictions = predictions, labels = labels, 
                          label.ordering = label.ordering, folds = folds,
                          ids = NULL, confidence = confidence)
  predictions <- clean$predictions  #Length-V list of predicted values
  labels <- clean$labels  #Length-V list of true labels
  pos <- levels(labels[[1]])[[2]]
  neg <- levels(labels[[1]])[[1]]  
  n.obs <- length(unlist(labels))
  # Inverse probability weights across entire data set
  w1 <- 1/(sum(unlist(labels)==pos)/n.obs)  #Positive class
  w0 <- 1/(sum(unlist(labels)==neg)/n.obs)  #Negative class
  
  .IC <- function(predictions, labels, pos, neg, w1, w0) {  
    n.rows <- length(labels)
    n.pos <- sum(labels==pos)
    n.neg <- n.rows - n.pos
    .empr <- function(x,Y){
      ifelse(Y==pos, sum(predictions[which(labels==neg)]<x)/n.neg, sum(predictions[which(labels==pos)]>x)/n.pos)
    }
    auc <- AUC(predictions, labels)
    .ic <- function(r, predictions, labels, pos, neg, w1, w0, auc){  #Influence function
      ifelse(labels[r]==pos, w1*.empr(predictions[r], pos) - w1*auc, w0*.empr(predictions[r], neg) - w0*auc)
    } 
    fold.ics <- sapply(1:n.rows, .ic, predictions=predictions, labels=labels, pos=pos, neg=neg, w1=w1, w0=w0, auc=auc)
    return(mean(fold.ics^2))  #Return average IC^2(O_i) for single validation fold
  }
  sighat2 <- mean(unlist(mapply(FUN=.IC, predictions, labels, MoreArgs=list(pos=pos, neg=neg, w1=w1, w0=w0))))
  se <- sqrt(sighat2/n.obs)  
  cv.auc <- cvAUC(predictions, labels)$cvAUC
  z <- qnorm(confidence+(1-confidence)/2)
  ci.cv.auc <- c(cv.auc - z*se, cv.auc + z*se)
  ci.cv.auc[1] <- ifelse(ci.cv.auc[1]<0,0,ci.cv.auc[1]) #Truncate CI at 0 and 1
  ci.cv.auc[2] <- ifelse(ci.cv.auc[2]>1,1,ci.cv.auc[2]) 
  return(list(cvAUC=cv.auc, se=se, ci=ci.cv.auc, confidence=confidence))
}
