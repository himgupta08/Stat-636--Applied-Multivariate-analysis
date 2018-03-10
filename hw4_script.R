setwd("D://Himanshu//Acads//03. Stat 636")

library(MASS)
library(caret)

Data <- read.csv("Datasets//hof_data.csv")

# Subsetting Data for relevant columns
vars <- c("HOF", "H", "HR", "RBI", "AVG", "SLG", "OBP")
Data1 <- Data[vars]

attach(Data1)

# Fitting a LDA classifier
lda_out <- lda(HOF ~., data = Data1)

Data1$HOF <- ifelse(Data1$HOF =='Y', 1, 0)

#Create 1 element fold 
folds <- cut(seq(1,nrow(Data1)),breaks=nrow(Data1),labels=FALSE)
kappa <- seq(from = 0, to = 0.50, by = 0.01)

a <- matrix(NA, nrow=nrow(Data1), ncol=length(kappa))
b <- matrix(NA, nrow=nrow(Data1), ncol=length(kappa))

# Create test and train dataset
for(i in 1:nrow(Data1)) {
  testindex <- which(folds==i)
  testdata <- Data1[testindex,]
  traindata <- Data1[-testindex,]
  
  lda_out <- lda(HOF ~., data = traindata)
  lda_pred <- predict(lda_out, newdata = testdata)
  
  for (k in kappa) {
  if (lda_pred$posterior[2] >= k) {
    a[i,(k)*100+1] <- 1
  } else {
    a[i,(k)*100+1] <- 0
  }
  }
    
    for (k in kappa) {
      b[i,(k)*100+1] <-  testdata$HOF
  }
  }
  
# 30th column is still NA

for(i in 1:nrow(Data1)) {
  testindex <- which(folds==i)
  testdata <- Data1[testindex,]
  traindata <- Data1[-testindex,]
  
  lda_out <- lda(HOF ~., data = traindata)
  lda_pred <- predict(lda_out, newdata = testdata)
  
  if (lda_pred$posterior[2] >= 0.29) {
    a[i,30] <- 1
  } else {
    a[i,30] <- 0
  }
  
  b[i,30] <-  testdata$HOF
  
}

# Finding out Specificity and Sensitivity

mat <-  as.list(matrix(NA, 2, 2))
specificity <- matrix(NA, ncol(a))
Sensitivity <- matrix(NA, ncol(a))
posPredValue <- matrix(NA, ncol(a))
negPredValue <- matrix(NA, ncol(a))
Bal_acc <- matrix(NA, ncol(a))

# tpr = tp/(tp+fn)
for (j in 1:ncol(a)){
  pred <- factor(a[,j])
  act <- factor(b[,j])
  # print(table(act, pred))
  Sensitivity[j] <- sensitivity(pred, act,positive = 1)
  specificity[j] <- specificity(pred, act,negative = 0)
  posPredValue[j] <- posPredValue(pred, act,positive = 1)
  negPredValue[j] <- negPredValue(pred, act,negative = 0)
  Bal_acc[j] <- (Sensitivity[j] + 3*specificity[j])/4
    }

# Plotting all accuracy values with Kappa
par(mfrow=c(3,2))
plot(kappa, Sensitivity, type="l")
plot(kappa, specificity, type="l")
plot(kappa, posPredValue, type="l")
plot(kappa, negPredValue, type="l")
plot(kappa, Bal_acc, type="l")
plot(1-specificity, Sensitivity, type="l")

##########################################################
#################### QDA Classifier ######################
##########################################################


# Fitting a QDA classifier
qda_out <- qda(HOF ~., data = Data1)

#Create 1 element fold 
folds <- cut(seq(1,nrow(Data1)),breaks=nrow(Data1),labels=FALSE)
kappa <- seq(from = 0, to = 0.50, by = 0.01)

qda_a <- matrix(NA, nrow=nrow(Data1), ncol=length(kappa))
b <- matrix(NA, nrow=nrow(Data1), ncol=length(kappa))

# Create test and train dataset
for(i in 1:nrow(Data1)) {
  testindex <- which(folds==i)
  testdata <- Data1[testindex,]
  traindata <- Data1[-testindex,]
  
  qda_out <- qda(HOF ~., data = traindata)
  qda_pred <- predict(qda_out, newdata = testdata)
  
  for (k in kappa) {
    if (qda_pred$posterior[2] >= k) {
      qda_a [i,(k)*100+1] <- 1
    } else {
      qda_a [i,(k)*100+1] <- 0
    }
  }
  
  for (k in kappa) {
    b[i,(k)*100+1] <-  testdata$HOF
  }
}

# 30th column is still NA

for(i in 1:nrow(Data1)) {
  testindex <- which(folds==i)
  testdata <- Data1[testindex,]
  traindata <- Data1[-testindex,]
  
  qda_out <- qda(HOF ~., data = traindata)
  qda_pred <- predict(qda_out, newdata = testdata)
  
  if (qda_pred$posterior[2] >= 0.29) {
    qda_a[i,30] <- 1
  } else {
    qda_a[i,30] <- 0
  }
  
  b[i,30] <-  testdata$HOF
  
}

qda_specificity <- matrix(NA, ncol(qda_a))
qda_Sensitivity <- matrix(NA, ncol(qda_a))
qda_posPredValue <- matrix(NA, ncol(qda_a))
qda_negPredValue <- matrix(NA, ncol(qda_a))
qda_Bal_acc <- matrix(NA, ncol(qda_a))

# tpr = tp/(tp+fn)
for (j in 1:ncol(qda_a)){
  pred <- factor(qda_a[,j], levels=0:1)
  act <- factor(b[,j], levels=0:1)
  qda_Sensitivity[j] <- sensitivity(pred, act,positive = 1)
  qda_specificity[j] <- specificity(pred, act,negative = 0)
  qda_posPredValue[j] <- posPredValue(pred, act,positive = 1)
  qda_negPredValue[j] <- negPredValue(pred, act,negative = 0)
  qda_Bal_acc[j] <- (qda_Sensitivity[j] + 3*qda_specificity[j])/4
}

par(mfrow=c(3,2))
plot(kappa, qda_Sensitivity, type="l")
plot(kappa, qda_specificity, type="l")
plot(kappa, qda_posPredValue, type="l")
plot(kappa, qda_negPredValue, type="l")
plot(kappa, qda_Bal_acc, type="l")
plot(1-qda_specificity, qda_Sensitivity, type="l")



##Answer 1: Plot Balanced accuracy for LDA, QDA both

plot(kappa, Bal_acc, type="l", col="Blue", lwd=2.5, ylab="Balanced Accuracy")
par(new = TRUE)
plot(kappa, qda_Bal_acc, type="l", axes=False, col="Green", lwd=3.5)

legend(0.35, 0.6, c("LDA", "QDA"), col=c("Blue", "Green"), lty=1)

##Answer 2: binding balanced accuracies with Kappa

Bal_Accuracy <- as.data.frame(cbind(kappa, Lda_acc=c(Bal_acc), qda_acc=c(qda_Bal_acc)))

# Maximum values for LDA 
Bal_Accuracy[which.max(Bal_Accuracy$Lda_acc),]
# kappa   Lda_acc   qda_acc
# 6  0.05 0.9201982 0.9192168

# Optimal choice for Kappa = 0.05
# Accuracy indexes
lda_indexes <- cbind(Sensitivity[6],specificity[6],posPredValue[6],negPredValue[6])
# [,1]      [,2]      [,3]      [,4]
# [1,] 0.8979592 0.9276112 0.3859649 0.9944568

# Maximum values for QDA
Bal_Accuracy[which.max(Bal_Accuracy$qda_acc),]
# kappa   Lda_acc   qda_acc
# 32  0.31 0.9064063 0.9486894

# Optimal choice for Kappa = 0.31

# Accuracy indexes
qda_indexes <- cbind(qda_Sensitivity[32],qda_specificity[32],qda_posPredValue[32],qda_negPredValue[32])
# [,1]      [,2] [,3]      [,4]
# [1,] 0.9591837 0.9451913 0.47 0.9978166
