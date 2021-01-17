y.true <- c(rep(1,1000),rep(0,1000))
y.pre <- c(runif(1000, 0.3, 0.9), runif(1000, 0.1, 0.7))
# ROC曲线和auc值####
simpleRoc <- function(y.pre, y.true){
  library(ROCR)
  pred <- prediction(y.pre,y.true)
  auc <- performance(pred,'auc')@y.values #AUC值
  perf <- performance(pred,'tpr','fpr')
  plot(perf)
  cat("auc=",auc[[1]])
}
simpleRoc(y.pre, y.true)



beatifulRoc <- function(y.pre, y.true){
  library(pROC)
  modelroc <- roc(y.true,y.pre)
  plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
       grid.col=c("green", "red"), max.auc.polygon=TRUE,
       auc.polygon.col="skyblue", print.thres=TRUE)
}
beatifulRoc(y.pre, y.true)


# lift曲线####
ROCRLift <- function(y.pre,y.true){
  library(ROCR)
  pred <- prediction(y.pre,y.true)
  perf <- performance(pred,'lift','rpp')
  plot(perf)
}
ROCRLift(y.pre,y.true)

lift::plotLift(y.pre, y.true)


# ks曲线####
myKS <- function(pre,label){
  true <- sum(label)
  false <- length(label)-true
  tpr <- NULL
  fpr <- NULL
  o_pre <- sort(pre) # let the threshold in an order from small to large
  for (i in o_pre){
    tp <- sum((pre >= i) & label)
    tpr <- c(tpr,tp/true)
    fp <- sum((pre >= i) & (1-label))
    fpr <- c(fpr,fp/false)
  }
  
  # 在这里修改图的标签
  plot(o_pre,tpr,type = "l",col= "green",xlab="threshold",ylab="tpr,fpr")
  lines(o_pre,fpr,type="l", col = "red")
  lines(o_pre,tpr-fpr,type = "l", col = "yellow")
  KSvalue <- max(tpr-fpr)
  sub = paste("KS value =",KSvalue)
  title(sub=sub)
  
  
  cutpoint <- which.max(tpr-fpr)
  thre <- o_pre[cutpoint]
  
  # 修改图的标签
  lines(c(thre,thre),c(fpr[cutpoint],tpr[cutpoint]),col = "blue")
  cat("KS-value:",KSvalue)
}
myKS(y.pre, y.true)





