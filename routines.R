

metrics <- function(truth, classPreds){
  tp <- sum(truth & classPreds==1)
  tn <- sum(!truth & classPreds!=1)
  sensitivity <- DescTools::BinomCI((tp), n = sum(truth), method = "wald", conf.level = 0.95)
  specificity <- DescTools::BinomCI((tn), n = sum(!truth), method = "wald", conf.level = 0.95)
  
  df <- rbind(sensitivity, specificity)
  colnames(df) <- c("Estimate", "Lower CI Bound", "Upper CI Bound")
  rownames(df) <- c("Sensitivity", "Specificity")
  return(df)
}

rocs <- function(truth, numPreds, classPreds, baseline1, baseline2, print_metrics=T){
  library(pROC)
  r <- roc(as.numeric(truth), numPreds, plot=T, lty=1, lwd=2)
  
  m <- metrics(truth, classPreds)
  points(m[2,1], m[1,1], pch=16)
  abline(v=m[2,1], lty=2)
  abline(h=m[1,1], lty=2)
  
  if(print_metrics){
    text(.6, .6, paste("ROC AUC=", round(r$auc,3)), cex =.5)
    text(m[2,1]-0.05, 0.02, paste("Specificity=", round(m[2,1],3)), cex =.5)
    text(0.85, m[1,1]-.03, paste("Sensitivity=", round(m[1,1],3)), cex =.5)
  }
  
  m <- metrics(truth, baseline1)
  points(m[2,1], m[1,1], pch=18)
  m <- metrics(truth, baseline2)
  points(m[2,1], m[1,1], pch=18)
  
  grid()
  return(r$auc)
}

rocs2 <- function(truth, numPreds, classPreds, baseline1, baseline2){
  library(pROC)
  roc(as.numeric(truth), numPreds, ci=T, plot=T, lty=3)
  roc(as.numeric(truth), classPreds, plot=T, add=T)
  roc(as.numeric(truth), baseline1, plot=T, add=T)
  roc(as.numeric(truth), baseline2, plot=T, add=T)
  #abline(h=0.94, lty=2, col="gray")
  grid()
}
