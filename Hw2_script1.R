install.packages("MASS")
setwd("D:\\Himanshu\\Acads\\03. Stat 636\\Homeworks\\hw2")

##########################################################
#################### Problem 3 ###########################
##########################################################

hw2_Data1 <- read.csv("used_cars.csv", header=T)
attach(hw2_Data1)

a <- qqplot(Age, Price)

qqnorm(Age)
qqline(Age, col="red")


# Using Box-cox function to trasnform the variable.
library(MASS)
?boxcox

b1 <- boxcox(lm(Age~Price), lambda = seq(-2, 4, by = 0.001))

lambda <- b1$x[which.max(b1$y)] #lambda = 0.748 has its peak for log-likelihood.

qqnorm(Age^0.748)
qqline(Age^0.748, col="red")

qqnorm(((Age^0.748)-1)/0.748)
qqline(((Age^0.748)-1)/0.748, col="red")

b2 <- boxcox(lm(Price~Age), lambda = seq(-1, 2, by = 0.001))
lambda2 <- b2$x[which.max(b2$y)] # 0.336

qqnorm(Price) 
qqline(Price, col="orange")

qqnorm(((Price^0.336)-1/0.336)) 
qqline(((Price^0.336)-1/0.336), col="orange")

# Using car package powerTransform function
library(car)

powerTransform(Age) #0.3708 
powerTransform(Age~Price) #0.7477 Similar to results above
powerTransform(Age, family = "yjPower") # Using Yeo-Jhonson family parameter value is 0.24118


powerTransform(Price) #0.9361967
powerTransform(Price~Age) #0.3360005 same as above

powerTransform(cbind(Age, Price))
# Age     Price 
# 1.2732157 0.0310405 

qqnorm(Age^1.2732157)
qqline(Age^1.2732157, col="orange")

qqnorm(Price^0.0310405)
qqline(Price^0.0310405, col="orange")


##########################################################
#################### Problem 4 ###########################
##########################################################

hw2_Data2 <- read.csv("sweat.csv", header=T)
attach(hw2_Data2)

##### Problem 4.1 #####

qqnorm(Sweat)
qqline(Sweat, col="green")
powerTransform(Sweat) #0.570771 
qqnorm(Sweat^0.570771)
qqline(Sweat^0.570771, col="green")


qqnorm(Sodium)
qqline(Sodium, col="green")
powerTransform(Sodium) #1.30133 
qqnorm(Sodium^1.30133)
qqline(Sodium^1.30133, col="green")

qqnorm(Potassium)
qqline(Potassium, col="green")
powerTransform(Potassium) # -0.253743 
qqnorm(Potassium^-0.253743)
qqline(Potassium^-0.253743, col="green")

pairs(hw2_Data2)

powerTransform(cbind(Sweat, Sodium, Potassium))
# Sweat     Sodium  Potassium 
# 0.4914538  0.9018033 -0.9012167 

qqnorm(cbind(Sweat, Sodium, Potassium))
qqline(cbind(Sweat, Sodium, Potassium), col="red")

qqnorm(cbind(Sweat^0.4914, Sodium^0.9018, Potassium^-0.9012))
qqline(cbind(Sweat^0.4914, Sodium^0.9018, Potassium^-0.9012), col="red")

# I do not think data was multivariate normal before. However, it does not seem like to be normal even after transformation

###################################
########## Problem 4.2 ############
###################################

mu <- colMeans(hw2_Data2)

# Ellipse is centered at this mu value
# Sweat    Sodium Potassium 
# 4.640    45.400     9.965 

S <- var(hw2_Data2) # covariance matrix
p <- length(hw2_Data2)
n <- nrow(hw2_Data2)
alpha <- 0.05
c_sq <- (n - 1) * p * qf(1 - alpha, p, n - p) / (n - p)
#10.71
ee <- eigen(S)
lambda <- ee$values #[1] 200.462464   4.531591   1.301392
vectors <- ee$vectors # these are the axes

# [,1]        [,2]        [,3]
# [1,] -0.05084144 -0.57370364  0.81748351
# [2,] -0.99828352  0.05302042 -0.02487655
# [3,]  0.02907156  0.81734508  0.57541452

# First column vector belongs to first axes, second column vector belongs to second axes, and third column vector is for 3rd axes

#To obtain the orientation of the ellipse, we simply calculate the angle of the largest eigenvector towards the x-axis:
#theta <- atan(vectors[2, 1] / vectors[1, 1]) * 57.2957795
# 87.08451

# Half-lengths
print(HL1 <- sqrt(c_sq * lambda[1] / n)) #10.36503 for first axes
print(HL2 <- sqrt(c_sq * lambda[2] / n)) #1.558402 for second axes
print(HL3 <- sqrt(c_sq * lambda[3] / n)) #0.835138 for third axes

#Plot the ellipse in 3-D
library(scatterplot3d)
xlim <- mu[1] + c(-1, 1) * 3.5 * sqrt(S[1, 1] / n)
ylim <- mu[2] + c(-1, 1) * 3.5 * sqrt(S[2, 2] / n)
zlim <- mu[3] + c(-1, 1) * 3.5 * sqrt(S[3, 3] / n)
# could not do it here. Used ellipse3d function too but succeeded. 

###################################
########## Problem 4.3 ############
###################################

#simultaneous confidence intervals
c_sq <- (n - 1) * p * qf(1 - alpha, p, n - p) / (n - p)
mu
# Sweat    Sodium Potassium 
# 4.640    45.400     9.965 

#confidence interval for sweat variable
a1 <- c(1, 0, 0)
i1 <- sqrt(c_sq)*sqrt(t(a1)%*%S%*%a1/n)
# or
i1 <- sqrt(c_sq)*sqrt(S[1,1]/n)
print(c1_left <- a1%*%mu - i1)
print(c1_right <- a1%*%mu + i1)

#confidence interval for sodium variable
a2 <- c(0, 1, 0)
i2 <- sqrt(c_sq)*sqrt(t(a2)%*%S%*%a2/n)
print(c2_left <- a2%*%mu - i2)
print(c2_right <- a2%*%mu + i2)

#confidence interval for sodium variable
a3 <- c(0, 0, 1)
i3 <- sqrt(c_sq)*sqrt(t(a3)%*%S%*%a3/n)
print(c3_left <- a3%*%mu - i3)
print(c3_right <- a3%*%mu + i3)


# All 3 confidence intervals above cover 95% simultaneous intervals for sweat data. 
# Actually in reality, they are wider than 95%
# That is the reason bonferroni approach is used to make conservative intervals

###################################
########## Problem 4.4 ############
###################################

# for bonferroni, we need to calculate t(alpha/2p) at n-1 degree of freedom

#for variable 1 - sweat
print(ci1 <- mu[1]+c(1, -1)*qt(0.025/3, 19)*sqrt(S[1,1]/n)) #3.643952 5.636048
#for variable 2 - sodium
print(ci2 <- mu[2]+c(1, -1)*qt(0.025/3, 19)*sqrt(S[2,2]/n)) #37.10308 53.69692
#for variable 3 - potassium
print(ci3 <- mu[3]+c(1, -1)*qt(0.025/3, 19)*sqrt(S[3,3]/n)) #8.846992 11.083008

# it can be seen that these intervals are quite conservative than problem 4.3 intervals and wider than univariate t-distribution intervals


###################################
########## Problem 4.5 ############
###################################

mu
S
mu_0 <- c(4, 45, 10)
p <- length(hw2_Data2)

T2 <- n * t(mu - mu_0) %*% solve(S) %*% (mu - mu_0)
#4.37
p_value <- 1 - pf((n - p) * T2 / ((n - 1) * p), p, n - p) #0.305

# Null hypothesis is correct. Mu_0 is with in the ellipse formed by mu of sweat dataset.
# we can see that T2 value by mu was 10.71 (as calculated in) problem 4.2 above, at a significance level of 0.05
# and T2 value (criticial value) for mu_0 is 4.37 which is lower than 10.71. So mu_0 would lie with in confidence region.
# also, p_value is 0.30 which is > 0.05 so, null hypothesis can not be rejected

###################################
######### Problem 4.6 #############
###################################

#From part 4.2, 95% confidence ellipsoid had squared distance of 10.71 (at aplha = 0.05)
#Lets take alpha = 0.305 now and calculate c_sq

alpha1 <- 0.305
c_Sq1 <- (n-1)*p*qf(1-alpha1, p, n-p)/(n-p) #4.377702
# It is less than 10.71, so it lies with in the 95% confidence ellipsoid

###################################
######### Problem 4.7 #############
###################################

# Using bootstrap to make 500 samples of sweat data

set.seed(101)
B <- 500
lambda <- rep(NA, B) # or write numeric(B) to create 500 null values
mu_0 <- c(4, 45, 10)
n <- nrow(hw2_Data2)

sweat_new <- t(t(hw2_Data2) - mu + mu_0)

for (b in 1:B){
  BS_sweat <- sweat_new[sample(1:n, replace=TRUE), ]
  BS_sweat_mean <- colMeans(BS_sweat)
  
  BS_S <- var(BS_sweat) #covariance matrix for bootstrapped sample
  BS_S0 <- as.matrix(t(BS_sweat)-mu_0) %*% as.matrix(t(t(BS_sweat)-mu_0))/(n-1) #covariance matrix with mu_0
  lambda[b]=(det(BS_S)/det(BS_S0))^(n/2)
}

# Lambda using original sample
S <- var(hw2_Data2)
S0 <- as.matrix(t(hw2_Data2)-mu_0) %*% as.matrix(t(t(hw2_Data2)-mu_0))/(n-1) #covariance matrix with mu_0
lambda1 = (det(S)/det(S0))^(n/2)

p_value <- 1- mean(lambda>=lambda1) #0.334
# 33.4 % values of bootstrapped sample lambda are higher than main dataset lambda. so mu_0 lies with in the 95% confidence interval.
# this says that H0 is true.


###############################################################
####################### Problem 5 #############################
###############################################################

###############################################
################# Problem 5.1 #################
###############################################

hw2_Data3 <- read.csv("peanut.csv", header=T)
attach(hw2_Data3)

loc_manova <- manova(cbind(X_1, X_2, X_3) ~ as.factor(Location))

summary(loc_manova, test="Wilks")
summary(loc_manova, test="Hotelling-Lawley")
# P value is 0.0644; This means null hypothesis is true and mean values of all 3 columns are dependent by location

# Using Variety column
var_manova <- manova(cbind(X_1, X_2, X_3) ~ as.factor(Variety))

summary(var_manova, test="Wilks") # P- 0.02195
summary(var_manova, test="Hotelling-Lawley") #P- 0.03348
# P value is lower than 0.05. So in this case alternate hypothesis is true. it means that all 3 columns are independent from variety

# Using Location and Variety column
Loc_var_manova <- manova(cbind(X_1, X_2, X_3) ~ cbind(as.factor(Location),as.factor(Variety)))
                         
summary(Loc_var_manova, test="Wilks") # P value- 0.0394
summary(Loc_var_manova, test="Hotelling-Lawley") # P value- 0.07253
summary(Loc_var_manova, test="Pillai") # P value- 0.021

# Wilk test suggest that all the three columns grouped by location-variety are different.

#### Doing same thing Using another command --
summary(manova(as.matrix(hw2_Data3[, 3:5]) ~ Location + Variety + Location * Variety), 
        test = "Wilks")

# Location : 0.020502 * 
#   Variety : 0.001928 **
#   Location * Variety: 0.050794

## I am not sure why am i getting different answers in both cases!!!!

###############################################
################# Problem 5.2 #################
###############################################

## Using the code from Plastic_film.r script. I have not calculated the SSP matrix by myself.

n <- 2
p <- 3
g <- 2
b <- 3

Location <- factor(Location)
Variety <- factor(Variety)

x_bar <- colMeans(hw2_Data3[, 3:5])

x_bar_l_dot <- rbind(colMeans(hw2_Data3[Location == 1, 3:5]), colMeans(hw2_Data3[Location == 2, 3:5]))
x_bar_dot_k <- rbind(colMeans(hw2_Data3[Variety == 5, 3:5]), colMeans(hw2_Data3[Variety == 6, 3:5]), 
                     colMeans(hw2_Data3[Variety == 8, 3:5]))

x_bar_lk <- rbind(colMeans(hw2_Data3[Location == 1 & Variety == 5, 3:5]), 
                  colMeans(hw2_Data3[Location == 1 & Variety == 6, 3:5]),
                  colMeans(hw2_Data3[Location == 1 & Variety == 8, 3:5]),
                  colMeans(hw2_Data3[Location == 2 & Variety == 5, 3:5]),
                  colMeans(hw2_Data3[Location == 2 & Variety == 6, 3:5]),
                  colMeans(hw2_Data3[Location == 2 & Variety == 8, 3:5]))

S_lk <- var(x_bar_lk)

X <- hw2_Data3

## Components for MANOVA.
SSP_cor <- SSP_fac_1 <- SSP_fac_2 <- SSP_int <- SSP_res <- matrix(0, nrow = p, ncol = p)
for(l in 1:g) {
  SSP_fac_1 <- SSP_fac_1 + b * n * t(x_bar_l_dot[l, , drop = FALSE] - x_bar) %*% 
    (x_bar_l_dot[l, , drop = FALSE] - x_bar)
  SSP_fac_2 <- SSP_fac_2 + g * n * t(x_bar_dot_k[l, , drop = FALSE] - x_bar) %*% 
    (x_bar_dot_k[l, , drop = FALSE] - x_bar)
  for(k in 1:b) {
    SSP_int <- SSP_int + n * t(x_bar_lk[(l - 1) * 2 + k, , drop = FALSE] - 
                                 x_bar_l_dot[l, , drop = FALSE] - x_bar_dot_k[k, , drop = FALSE] + x_bar) %*% 
      (x_bar_lk[(l - 1) * 2 + k, , drop = FALSE] - x_bar_l_dot[l, , drop = FALSE] - 
         x_bar_dot_k[k, , drop = FALSE] + x_bar)
    for(r in 1:n) {
      SSP_res <- SSP_res + t(as.matrix(X[(l - 1) * 2 * n + (k - 1) * n + r, 3:5]) - 
                               x_bar_lk[(l - 1) * 2 + k, , drop = FALSE]) %*% 
        (as.matrix(X[(l - 1) * 2 * n + (k - 1) * n + r, 3:5]) - 
           x_bar_lk[(l - 1) * 2 + k, , drop = FALSE])
      SSP_cor <- SSP_cor + t(as.matrix(X[(l - 1) * 2 * n + (k - 1) * n + r, 3:5]) - 
                               x_bar) %*% (as.matrix(X[(l - 1) * 2 * n + (k - 1) * n + r, 3:5]) - x_bar)
    }
  }
}

##
## Inference.
##

## There is an effect of rate of extrusion.
Lambda <- det(SSP_res) / det(SSP_fac_1 + SSP_res)
p_value_Fac1 <- 1 - pf((((g * b * (n - 1) - p + 1) / 2) / ((abs((g - 1) - p) + 1) / 2)) * 
         (1 - Lambda) / Lambda, abs((g - 1) - p) + 1, g * b * (n - 1) - p + 1)

# 0.6248354

## There is an effect of amount of additive.
Lambda <- det(SSP_res) / det(SSP_fac_2 + SSP_res)
p_value_Fac2 <- 1 - pf((((g * b * (n - 1) - p + 1) / 2) / ((abs((b - 1) - p) + 1) / 2)) * 
         (1 - Lambda) / Lambda, abs((b - 1) - p) + 1, g * b * (n - 1) - p + 1)

# 0.5702375

## No interaction.
Lambda <- det(SSP_res) / det(SSP_int + SSP_res)
p_value_int <- 1 - pf((((g * b * (n - 1) - p + 1) / 2) / ((abs((g - 1) * (b - 1) - p) + 1) / 2)) * 
         (1 - Lambda) / Lambda, abs((g - 1) * (b - 1) - p) + 1, g * b * (n - 1) - p + 1)

# 0.1334432


