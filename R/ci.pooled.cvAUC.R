ci.pooled.cvAUC <- function(predictions, labels, label.ordering = NULL, folds = NULL, ids, confidence = 0.95){

  require(ROCR)
  clean <- .process_input(predictions, labels, label.ordering, folds=folds, ids=ids, confidence=confidence)
  predictions <- clean$predictions
  labels <- clean$labels
  ids <- clean$ids
  pos <- levels(labels[[1]])[[2]]
  neg <- levels(labels[[1]])[[1]]  
  n.obs <- length(unlist(labels))
  n.ids <- length(unique(unlist(ids)))
  taubar <- mean(table(unlist(ids)))  #Avg number of obs per id
  # Inverse probability weights across entire data set
  w1 <- 1/(sum(unlist(labels)==pos)/n.obs)  #Positive class
  w0 <- 1/(sum(unlist(labels)==neg)/n.obs)  #Negative class
  
  .IC.pooled <- function(predictions, labels, ids, taubar, pos, neg, w1, w0){
    
    n.rows <- length(labels)
    n.pos <- sum(labels==pos)
    n.neg <- n.rows - n.pos
    .empr <- function(x,Y){
      ifelse(Y==pos, sum(predictions[which(labels==neg)]<x)/n.neg, sum(predictions[which(labels==pos)]>x)/n.pos)
    }
    auc <- cvAUC(predictions, labels)$cvAUC
    .ic <- function(r, predictions, labels, pos, neg, w1, w0, auc){  #Influence function
      ifelse(labels[r]==pos, w1*.empr(predictions[r], pos) - w1*auc, w0*.empr(predictions[r], neg) - w0*auc)
    } 
    fold.ics <- sapply(1:n.rows, .ic, predictions=predictions, labels=labels, pos=pos, neg=neg, w1=w1, w0=w0, auc=auc)  #Return average IC^2(O_j) for single validation fold
    .IC.Oi <- function(id, taubar){
      sum(fold.ics[which(ids==id)])/taubar
    }
    ic.Ois <- sapply(unique(ids), .IC.Oi, taubar)
    return(mean(ic.Ois^2))  
  }
  sighat2 <- mean(unlist(mapply(FUN=.IC.pooled, predictions, labels, ids, MoreArgs=list(taubar=taubar, pos=pos, neg=neg, w1=w1, w0=w0))))
  se <- sqrt(sighat2/n.ids)  
  cv.auc <- cvAUC(predictions, labels)$cvAUC
  z <- qnorm(confidence+(1-confidence)/2)
  ci.cv.auc = c(cv.auc - z*se, cv.auc + z*se)
  ci.cv.auc[1] <- ifelse(ci.cv.auc[1]<0,0,ci.cv.auc[1])  #Truncate CI at 0 and 1
  ci.cv.auc[2] <- ifelse(ci.cv.auc[2]>1,1,ci.cv.auc[2]) 
  return(list(cvAUC=cv.auc, se=se, ci=ci.cv.auc, confidence=confidence))
}
