setwd("D:\\Himanshu\\Acads\\03. Stat 636")
library(plotrix)

##########################################################
#################### Problem 1 ###########################
##########################################################

Data1 <- read.delim("Datasets\\oxygen.DAT", header=FALSE, sep="")
View(Data1)
attach(Data1)

colnames(Data1) <- c("x1", "x2", "x3", "x4", "gender")
table(Data1$gender)

############### Part-a ###############
# Use aggregate function - It can simply be done using below aggregate function
report1 = aggregate(. ~ gender, Data1, function(x) c(mean = mean(x), stddev = sd(x)))
# above report1 is solution for part-a. each column has a mean/std dev that is added as a suffix

############### Part-b ###############
pairs(Data1[,1:4])
# There is a linear relationship between x3 and x4;  Similarly x1 and x2 are also seem to be.. 
# ..linearly dependent. There are some outliers in x1 vs x2 curve 

############### Part-c ###############
coplot(Data1$x1~Data1$x3|Data1$gender, col="red", panel = function(x, y,...){
  panel.smooth(x, y, span=.8, iter=5, ...)
  abline(lm(y~x), col="blue")}, rows=1)
# red line is Lowess curve and blue line is best fit line using least square method
# Differences between female and male curve-
# 1. for female data, x1 has some outliers or data has more variance while male curve shows less variance (less hetroscedacity)
# 2. Slope of the linearity between x1 & x3 in male data is higher than female data



##########################################################
#################### Problem 2 ###########################
##########################################################

## Part a is solved in the notebook manually

## Part b to be solved -

######################
## Part b, question 1#
######################


# Define the function -

pdf_x <- function(x, mu, sigma) {
  
  p <- length(x)
  constant_term <- 1 / ((2*pi)^(p/2) * sqrt(det(sigma)))
  exponential_term <- t(x-mu) %*% solve(sigma) %*% (x-mu)*0.5
  
  #final output
  
  OutF <- exp(- exponential_term)/constant_term
  
  return(OutF)
}

# assign the variables
mu <- c(1, -1)
sigma <- matrix(c(1, -1.6, -1.6, 4.0), nrow=2, ncol=2)

# lets take one point for x
x <-c(0,0)
pdf_x(x, mu, sigma)

# Define seequence of x1 that has mean = 1 and std dev = 1 (sqrt of var = 1)
x1_seq <- seq(-2, 4, length= 100)
# Similarly for x2 seq
x2_seq <- seq(-7, 5, length= 100)

# calculate pdf_seq for each point

set.seed(1)
m <- matrix(rnorm(10000),nrow=100,ncol=100)
m <- data.frame(m)
for (i in 1:length(x1_seq)){
  for (j in 1:length(x2_seq)) {
x = c(x1_seq[i], x2_seq[j])

mu <- c(1, -1)

sigma <- matrix(c(1, -1.6, -1.6, 4.0), nrow=2, ncol=2)

m[i, j] <-  pdf_x(x, mu, sigma)
  }}

View(m)
m1 <- as.matrix(m)
View(m1)

# Perspective plot
persp(x1_seq, x2_seq, m1)

######################
## Part b, question 2#
######################

stat_dist <- function(x) { 
  mu <- c(1, -1)
  sigma <- matrix(c(1, -1.6, -1.6, 4.0), nrow=2, ncol=2)
  
  dist = t(x-mu) %*% solve(sigma) %*% (x-mu)
  return(dist)
}

x1 <- c(0,0)
x2 <- c(0,-2)

stat_dist(x1) # 1.25
stat_dist(x2) # 5.69   

# Using statistical distance, point O (x1) is closer

eucl_dist <- function(x) { 
  mu <- c(1, -1)
  dist = sqrt((x[1]-mu[1])^2 + (x[2]-mu[2])^2) 
  return(dist)
}

eucl_dist(x1) # 1.414
eucl_dist(x2) # 1.414

# Both are at the same distance using straight line distance

######################
## Part b, question 3#
######################

# as we have already calculated, x1 is more closer to mean than x2 (statistical distance wise). so concentration/volume 
# of bivariate distribution points is higher in region near x1 as compared to x2. probablity of a point falling into
# region near x1 is high compared to x2.


##########################################################
#################### Problem 6 ###########################
##########################################################

# part a
install.packages("plotrix")

mu = t(c(1,1))

sigma1_inv = solve(sigma1)
# using qchisq function -

# c = 2.447

#Using spectral decomposition for statistical distance -
#t(x)Ax = t(x)[lambda1*e1*t(e1) + lambda2*e2*t(e2)]x
#so lambda1 and lambda2 are eigen values of A and e1, e2 are eigen vectors of A. A would be inverse of sigma matrix

 
c <- sqrt(qchisq(.95, df=2))

# calculate eigen values



eigens <- function(sigma){
  eigen_inv <- eigen(solve(sigma))
  eigen_inv$HL[1] <- c/sqrt(eigen_inv$values[1])
  eigen_inv$HL[2] <- c/sqrt(eigen_inv$values[2])
  return(eigen_inv)
  
}

# 1st sigma matrix

sigma1 = matrix(c(1, 0.8, 0.8, 1), nrow=2)

ee <- eigen(sigma1)
# $values
# [1] 5.0000000 0.5555556
# 
# $vectors
# [,1]       [,2]
# [1,] -0.7071068 -0.7071068
# [2,]  0.7071068 -0.7071068

# $HL
# [1] 1.094666 3.283997

xlim <- mu[1] + c(-1, 1) * 2*c * sqrt(abs(ee$values[1]))
ylim <- mu[2] + c(-1, 1) * 2*c * sqrt(abs(ee$values[2]))

plot(xlim, ylim, xlab = expression(mu[1]), ylab = expression(mu[2]), asp = 1, type = "n")

print(theta <- acos(ee$vectors[1, 1])*360/(2*pi)*sign(ee$vectors[2,1]))

draw.ellipse(mu[1], mu[2], c*sqrt(ee$values[1]), c*sqrt(ee$values[2]), angle = theta, border="blue", lwd=2)

# 2nd sigma matrix

sigma2 = matrix(c(1, 0, 0, 1), nrow=2)
eigens(sigma2)

ee <- eigen(sigma2)
# $values
# [1] 1 1
# 
# $vectors
# [,1] [,2]
# [1,]    0   -1
# [2,]    1    0
# 
# $HL
# [1] 2.447747 2.447747

xlim <- mu[1] + c(-1, 1) * 2*c * sqrt(abs(ee$values[1]))
ylim <- mu[2] + c(-1, 1) * 2*c * sqrt(abs(ee$values[2]))

plot(xlim, ylim, xlab = expression(mu[1]), ylab = expression(mu[2]), asp = 1, type = "n")

print(theta <- acos(ee$vectors[1, 1])*360/(2*pi)*sign(ee$vectors[2,1]))

draw.ellipse(mu[1], mu[2], c*sqrt(ee$values[1]), c*sqrt(ee$values[2]), angle = theta, border="blue", lwd=2)

# 3rd sigma matrix

sigma3 = matrix(c(1, -0.8, -0.8, 1), nrow=2)

eigens(sigma3)

ee <- eigen(sigma3)

# $values
# [1] 5.0000000 0.5555556
# 
# $vectors
# [,1]       [,2]
# [1,] 0.7071068 -0.7071068
# [2,] 0.7071068  0.7071068
# 
# $HL
# [1] 1.094666 3.283997

xlim <- mu[1] + c(-1, 1) * 2*c * sqrt(abs(ee$values[1]))
ylim <- mu[2] + c(-1, 1) * 2*c * sqrt(abs(ee$values[2]))

plot(xlim, ylim, xlab = expression(mu[1]), ylab = expression(mu[2]), asp = 1, type = "n")

print(theta <- acos(ee$vectors[1, 1])*360/(2*pi)*sign(ee$vectors[2,1]))

draw.ellipse(mu[1], mu[2], c*sqrt(ee$values[1]), c*sqrt(ee$values[2]), angle = theta, border="blue", lwd=2)


# 4th sigma matrix

sigma4 = matrix(c(1, 0.4, 0.4, 0.25), nrow=2)

eigens(sigma4)

ee <- eigen(sigma4)
# $values
# [1] 1.1732928 0.0767072
# 
# $vectors
# [,1]       [,2]
# [1,] -0.9175895  0.3975292
# [2,] -0.3975292 -0.9175895

xlim <- mu[1] + c(-1, 1) * 2*c * sqrt(abs(ee$values[1]))
ylim <- mu[2] + c(-1, 1) * 2*c * sqrt(abs(ee$values[2]))

plot(xlim, ylim, xlab = expression(mu[1]), ylab = expression(mu[2]), asp = 1, type = "n")

print(theta <- acos(ee$vectors[1, 1])*360/(2*pi)*sign(ee$vectors[2,1]))

draw.ellipse(mu[1], mu[2], c*sqrt(ee$values[1]), c*sqrt(ee$values[2]), angle = theta, border="blue", lwd=2)

# 5th sigma matrix

sigma5 = matrix(c(1, 0, 0, 0.25), nrow=2)
eigens(sigma5)
ee <- eigen(sigma5)

# $values
# [1] 1.00 0.25
# 
# $vectors
# [,1] [,2]
# [1,]   -1    0
# [2,]    0   -1

xlim <- mu[1] + c(-1, 1) * 2*c * sqrt(abs(ee$values[1]))
ylim <- mu[2] + c(-1, 1) * 2*c * sqrt(abs(ee$values[2]))

plot(xlim, ylim, xlab = expression(mu[1]), ylab = expression(mu[2]), asp = 1, type = "n")

print(theta <- acos(ee$vectors[1, 1])*360/(2*pi)*sign(ee$vectors[2,1]))

draw.ellipse(mu[1], mu[2], c*sqrt(ee$values[1]), c*sqrt(ee$values[2]), angle = theta, border="blue", lwd=2)


# 6th sigma matrix

sigma6 = matrix(c(1, -0.4, -0.4, 0.25), nrow=2)

eigens(sigma6)
ee <- eigen(sigma6)

# $values
# [1] 1.1732928 0.0767072
# 
# $vectors
# [,1]       [,2]
# [1,] -0.9175895 -0.3975292
# [2,]  0.3975292 -0.9175895

xlim <- mu[1] + c(-1, 1) * 2*c * sqrt(abs(ee$values[1]))
ylim <- mu[2] + c(-1, 1) * 2*c * sqrt(abs(ee$values[2]))

plot(xlim, ylim, xlab = expression(mu[1]), ylab = expression(mu[2]), asp = 1, type = "n")

print(theta <- acos(ee$vectors[1, 1])*360/(2*pi)*sign(ee$vectors[2,1]))

draw.ellipse(mu[1], mu[2], c*sqrt(ee$values[1]), c*sqrt(ee$values[2]), angle = theta, border="blue", lwd=2)

# 7th sigma matrix

sigma7 = matrix(c(0.25, 0.4, 0.4, 1), nrow=2)

eigens(sigma7)

ee <- eigen(sigma7)

# $values
# [1] 1.1732928 0.0767072
# 
# $vectors
# [,1]       [,2]
# [1,] 0.3975292 -0.9175895
# [2,] 0.9175895  0.3975292

xlim <- mu[1] + c(-1, 1) * 2*c * sqrt(abs(ee$values[1]))
ylim <- mu[2] + c(-1, 1) * 2*c * sqrt(abs(ee$values[2]))

plot(xlim, ylim, xlab = expression(mu[1]), ylab = expression(mu[2]), asp = 1, type = "n")

print(theta <- acos(ee$vectors[1, 1])*360/(2*pi)*sign(ee$vectors[2,1]))

draw.ellipse(mu[1], mu[2], c*sqrt(ee$values[1]), c*sqrt(ee$values[2]), angle = theta, border="blue", lwd=2)



# 8th sigma matrix

sigma8 = matrix(c(0.25, 0, 0, 1), nrow=2)

eigens(sigma8)
ee <- eigen(sigma8)
 

# $values
# [1] 1.00 0.25
# 
# $vectors
# [,1] [,2]
# [1,]    0   -1
# [2,]    1    0

xlim <- mu[1] + c(-1, 1) * 2*c * sqrt(abs(ee$values[1]))
ylim <- mu[2] + c(-1, 1) * 2*c * sqrt(abs(ee$values[2]))

plot(xlim, ylim, xlab = expression(mu[1]), ylab = expression(mu[2]), asp = 1, type = "n")

print(theta <- acos(ee$vectors[1, 1])*360/(2*pi)*sign(ee$vectors[2,1]))

draw.ellipse(mu[1], mu[2], c*sqrt(ee$values[1]), c*sqrt(ee$values[2]), angle = theta, border="blue", lwd=2)

# 9th sigma matrix

sigma9 = matrix(c(0.25, -0.4, -0.4, 1), nrow=2)

eigens(sigma9)
'$values
[1] 13.0365867  0.8523022

$vectors
[,1]       [,2]
[1,] -0.9175895  0.3975292
[2,] -0.3975292 -0.9175895

$HL
[1] 0.6779295 2.6513661'

##########################################################
########### Problem 6, Part B  ###########################
##########################################################

install.packages("mvtnorm")
library(mvtnorm)
#for sigma1


# we need to find out how many observations are with in the curve.
# i.e. where [t(x)%*%solve(sigma1)%*%x - qchisq(.95, df=2)] <= 0 

output <-   rep(NA, 5000)
WithInellipse <- function(sigma){
random_obs <- rmvnorm(5000, mu, sigma)
mu_0 <- colMeans(random_obs)
for (i in 1:5000){
  lhs <- t(random_obs[i,]-mu_0)%*%solve(sigma)%*%(random_obs[i,]-mu_0)
  rhs <- 5.991465
  
  output[i] = lhs - rhs
}
output1 <- output
# return(output1)
l1 <- length(output[output<=0])/5000
return(l1)
}

# WithInellipse(matrix(c(0.25, -0.4, -0.4, 1), nrow=2))
# 
# WithInellipse(matrix(c(1, 0, 0, 0.25), nrow=2))
# 
# l1 <- length(output[output<=0])
# 
# within_ellipse_pct <- l1*100/5000
# 
# return(within_ellipse_pct)


# Simulations
print(sigma1_sim <- WithInellipse(sigma1))
#0.9522

print(sigma2_sim <- WithInellipse(sigma2))
#0.9444

print(sigma3_sim <- WithInellipse(sigma3))
#0.9516

print(sigma4_sim <- WithInellipse(sigma4))
#0.944

print(sigma5_sim <- WithInellipse(sigma5))
#0.9534

print(sigma6_sim <- WithInellipse(sigma6))
#0.9496

print(sigma7_sim <- WithInellipse(sigma7))
#0.953

print(sigma8_sim <- WithInellipse(sigma8))
#0.9504

print(sigma9_sim <- WithInellipse(sigma9))
#0.9504

#Above are all proportions for all the 9 ellipses
