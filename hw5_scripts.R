setwd("D://Himanshu//Acads//03. Stat 636//Datasets")

library(MASS)
library(caret)

Data1 <- read.csv("happiness.csv")

colnames(Data1)[5] <- "Economy"
colnames(Data1)[7] <- "Health"
colnames(Data1)[9] <- "Trust"
colnames(Data1)[11] <- "Dystopia"

num_vars <- c("Economy", "Family", "Health", "Freedom", "Trust", "Generosity", "Dystopia")

Data2 <- Data1[num_vars]
n <- nrow(Data2)
sum_sse <-0

for (i in 1:n){
  test_ind <- i
  test_data <- Data2[test_ind,]
  train_data <- Data2[-test_ind,]
  
  mdl <- lm(Dystopia~., data = train_data)
  test_pred <- predict(mdl, test_data)
  SE <- (test_pred - test_data$Dystopia)^2
  
  sum_sse <- sum_sse + SE
}

MSE <- sum_sse/n  #0.3069102 


B <-  1000
MSE1 <- numeric(B)


for (j in 1:B) {
  BS_Data2 <- Data2[sample(1:n, replace = TRUE),]
  
  sum_sse <- 0

  for (i in 1:n){
    test_ind <- i
    test_data <- BS_Data2[test_ind,]
    train_data <- BS_Data2[-test_ind,]
    
    mdl <- lm(Dystopia~., data = train_data)
    test_pred <- predict(mdl, test_data)
    SE <- (test_pred - test_data$Dystopia)^2
    
    sum_sse <- sum_sse + SE
  }
  
  MSE1[j] <- sum_sse/n
}

mean(MSE1) #0.2898434
sd(MSE1) #0.03683273


##########################################################
#################### Problem 2  ##########################
##########################################################

library(glmnet)

Xtrain <- Data2[, 1:6]
Ytrain <- Data2[['Dystopia']]

mdl.lasso <- glmnet(x=as.matrix(Xtrain), y=Ytrain, family = "gaussian", alpha = 1)

cv.glmmod <- cv.glmnet(x=as.matrix(Xtrain), y=Ytrain, alpha=1)

print(best.lambda <- cv.glmmod$lambda.min) #0.02353942
# Log(lamnda) = 0.02353942

# Find Minimum MSE -
cv.glmmod$cvm[cv.glmmod$lambda==cv.glmmod$lambda.min]

# 0.2944128 - This would be equal to min(cv.glmmod$cvm)


###########################
##B. For leave one out cross validation

sum_sse <- 0
for (i in 1:n){
  test_ind <- i
  test_data <- Data2[test_ind,]
  train_data <- Data2[-test_ind,]
  
  Xtrain <- as.matrix(train_data[, 1:6])
  Ytrain <- train_data[['Dystopia']]
  
  Xtest <- as.matrix(test_data[,1:6])
  
  lasso.fit <- glmnet(x=Xtrain, y=Ytrain, lambda = 0.02353942, alpha = 1)
  test_pred <- predict(lasso.fit, newx=Xtest)
  
  SE <- (test_pred - test_data$Dystopia)^2
  
  sum_sse <- sum_sse + SE
}

MSE <- sum_sse/n  #0.301591

# 1.76% reduction in MSE compared to earlier lm function


###########################
##D. Finding out coefficients using lasso

coef(mdl.lasso, s= 0.02353942)

# (Intercept)  2.25449255
# Economy      .         
# Family       0.09923867
# Health       0.03765803
# Freedom      0.23604733
# Trust        .         
# Generosity  -0.47814568

## Coefficients for Linear regression model

# Coefficients:
#               Estimate 
# (Intercept)  2.19027   
# Economy     -0.27859 
# Family       0.22974   
# Health       0.43657    
# Freedom      0.51379    
# Trust       -0.08078    
# Generosity  -0.84072    

# By comparing least square with lasso, it is clear that all coefficients have reduced except Generosity!


