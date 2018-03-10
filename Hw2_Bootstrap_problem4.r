hw2_Data2 <- read.csv("sweat.csv", header=T)
attach(hw2_Data2)

set.seed(101)
B <- 500
lambda <- rep(NA, B) # or write numeric(B) to create 500 null values
mu_0 <- c(4, 45, 10)
for (b in 1:B){
  BS_sweat <- hw2_Data2[sample(1:n, replace=TRUE), ]
  BS_sweat_mean <- colMeans(BS_sweat)
  
  BS_S <- var(BS_sweat) #covariance matrix for bootstrapped sample
  BS_S0 <- as.matrix(t(BS_sweat-mu_0)) %*% as.matrix(BS_sweat-mu_0)/(n-1) #covariance matrix with mu_0
  lambda[b]=(det(BS_S)/det(BS_S0))^(n/2)
}

# Lambda using original sample
S <- var(hw2_Data2)
S0 <- as.matrix(t(hw2_Data2-mu_0)) %*% as.matrix(hw2_Data2-mu_0)/(n-1) #covariance matrix with mu_0
lambda1 = (det(S)/det(S0))^(n/2)

p_value <- mean(lambda>=lambda1) #0.224
# 22.4 % values of bootstrapped sample lambda are higher than main dataset lambda. so mu_0 lies with in the 95% confidence interval.
# this says that H0 is true.


#These were sample lambda values

> lambda
  [1] 3.078472e-50 2.119192e-49 2.767775e-49 8.857429e-51 1.729095e-47 7.499056e-47 1.317774e-49
  [8] 3.262584e-50 1.006979e-48 5.700456e-51 6.313551e-48 1.882152e-49 1.811894e-47 2.347551e-49
 [15] 4.031854e-49 4.344859e-54 5.708177e-49 9.794314e-51 1.958692e-54 7.732496e-50 2.436697e-49
 [22] 2.063127e-49 2.508542e-53 2.393795e-46 2.014226e-46 6.112712e-49 2.852959e-52 6.469124e-48
 [29] 2.062080e-52 3.129518e-50 7.403727e-49 1.534311e-48 3.480374e-49 3.453708e-52 1.846662e-48
 [36] 7.030966e-49 2.168323e-51 2.414657e-49 1.299872e-51 7.833905e-49 2.512664e-48 4.748714e-52
 [43] 9.436085e-46 2.715202e-47