install.packages("MASS")
setwd("D:\\Himanshu\\Acads\\03. Stat 636\\Homeworks\\hw3")

data1 <- read.csv("stock_prices.csv")
#103*5 dimension

S <- var(data1)
ee <- eigen(S)
# [1] 0.0013676780 0.0007011596 0.0002538024 0.0001426026 0.0001188868
# correlation matrix
R <- cor(data1)
ee_R <- eigen(R)

## 1. % variation explained
pct_var <- ee$values*100/sum(ee$values)
# [1] 52.926066 27.133298  9.821584  5.518400  4.600652
# - First PC explains 52% variation, 2nd PC explains 27% variation

pc <- as.matrix(data1)%*%ee$vectors
# > pc[,1]%*%pc[,2]
# [,1]
# [1,] -0.0004290344
# 2. coefficient of linear combination of PC1
P1 <- princomp(data1)
# Loadings:
#             Comp.1 Comp.2 Comp.3 Comp.4 Comp.5
# JP_Morgan   -0.223 -0.625  0.326  0.663  0.118
# Citibank    -0.307 -0.570 -0.250 -0.414 -0.589
# Wells_Fargo -0.155 -0.345        -0.497  0.780
# Shell       -0.639  0.248 -0.642  0.309  0.148
# ExxonMobil  -0.651  0.322  0.646 -0.216       

# coefficients of 1st principal component are shown in the column of comp1 for each variable
# It is negatively correlated with Shell and Exxon mobile with coeffs -0.639, -0.651 respectively

# Principal components would be given by standardising the variables and multiplying by correlation matrix eigen value
PC <- scale(data1, center=TRUE, scale=TRUE) %*% ee_R$vectors

# 2. What kind of weeks would be highlighted by the PC1
colnames(PC) <- c("PC1", "PC2", "PC3", "PC4", "PC5")
PC <- as.data.frame(PC)

head(PC[(order(abs(PC$PC1), decreasing=TRUE)), ])

#       PC1         PC2         PC3        PC4        PC5
# 63  4.153918  1.05336025  0.33828547  0.2833613  0.4421964
# 56 -3.426350 -1.62398414 -0.17171233  0.1162981 -0.7881517
# 94 -3.397549  0.07585382  0.07487581 -0.5078530  0.9112448
# 42 -3.318823  1.09461839  0.42598343 -0.6661808 -0.1564181
# 71 -3.271870  1.68439061  0.16830186  0.0936420  0.2582239
# 70  3.078601  1.62888853  0.63323708 -0.7704295 -0.2557469

# week 63, 56, 94, 42,  71, 70 would be highlighted

# 3. For PC2

P1$loadings[,2]

# coefficients of PC2
# JP_Morgan    Citibank Wells_Fargo       Shell  ExxonMobil 
# -0.6252260  -0.5703900  -0.3445049   0.2479475   0.3218478 

# It is negatively correlated with JP morgan, citi and wellsfargo and positively correlated with Shell and exxon mobile
head(PC[(order(abs(PC$PC2), decreasing=TRUE)), ])

#       PC1       PC2        PC3        PC4         PC5
# 96 -2.2747272  3.122051  0.9290765 -0.2760455 -0.96719874
# 83 -0.7063666 -2.995851  0.5458960 -0.4391712 -0.17703701
# 93  1.3239384  2.936899 -0.6443173  0.8078748  0.74242835
# 58  0.0788027 -2.932485  0.3224814  0.2851943 -0.24788280
# 14  1.4137087 -2.852100  0.7291694 -0.2239671 -0.05179952
# 91  1.4839567  2.543433 -0.1991815 -0.3167801  0.35496705

# PC2 would highlight week - 96, 83, 93, 58, 14, 91

# 4. variance of first two PCs
(P1$sdev)^2
# Comp.1       Comp.2       Comp.3       Comp.4       Comp.5 
# 0.0013543996 0.0006943522 0.0002513383 0.0001412181 0.0001177325 
ee$values
# 0.0013676780 0.0007011596 0.0002538024 0.0001426026 0.0001188868

plot(PC[,1], PC[,2])
order(PC[,1], decreasing=TRUE)

# 63rd week has the highest PC1 value, while 56th week has the lowest. 

##########################################################
#################### Problem 2 ###########################
##########################################################

x <- c(3, 2, 5, 4, 1, 7)

m <- matrix(, 4, 4)
# this takes entries by column
m[lower.tri(m)] <- x
diag(m) <- 0
m <- as.dist(m)

hc1 <- hclust(m, method="complete")
plot(hc1)
# > hc1$merge
# [,1] [,2]
# [1,]   -2   -4
# [2,]   -1   -3
# [3,]    1    2

# Intermediate Distance matrices are - 1, 2, and 7 for each dendogram height

hc2 <- hclust(m, method="single")
# Intermediate Distance matrices are - 1, 2, and 3

hc3 <- hclust(m, method="average")
# Intermediate Distance matrices are - 1, 2, and 4.6




##########################################################
#################### Problem 3 ###########################
##########################################################

mat1 <- matrix(c(5, 1, -1, 3, 4, -2, 1, 1), 4, 2)

# > dist(mat1)
#       1        2        3
# 2 7.211103                  
# 3 6.708204 3.605551         
# 4 3.605551 3.605551 4.000000

# part 1- IF AB and CD are taken together intially

# distance between 12 (AB) is 7.211 and CD is 4.00
# First merge AB and CD separately. Then find out the mean (x_bar, y_bar) for AB and CD both. 
# for AB (x_bar, y_bar) = (3, 1) and CD, (x_bar, y_bar) = (1, 1)
# for each point, find which centroid is nearest. take all of them together.

# Euclidean distances for each point from centroid -

#     centroid 1, centroid 2
# A    sqrt(13)     5
# B    sqrt(13)     3
# C      4          2
# D      0          2

# using this AD would be closer to centroid 1. and BC would be closer to centorid 2
# new centroids - (4, 2.5) and (0, -0.5)

#     centroid 1, centroid 2
# A       1.5        ~5
# B       ~6        ~1.8
# C       ~5        ~1.8
# D       ~1.8      ~3.2

# again AD are closer and BC are closer - so these 2 would be different clusters


# part 2- IF Ac and BD are taken together intially
# Centorid from AC = (3, 2.5) and BD = (2, -0.5)
# for each point, find which centroid is nearest. take all of them together.

# Euclidean distances for each point from centroid -

#     centroid 1, centroid 2
# A     2.5         5.5
# B      5          1.8
# C      5          3.5
# D      1.5        1.8

# A and D are closer to centorid 1 and BC are closed to centroid 2
# AD would be in one cluster and BC would be in other cluster.

# Conclusion- Irrespecitve of where we start, K-means is going to give us same results


##########################################################
#################### Problem 4 ###########################
##########################################################
setwd("D:\\Himanshu\\Acads\\03. Stat 636\\Datasets")
cereal <- read.csv("cereal.csv")

d1 <- dist(cereal)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.061  73.260 119.900 136.000 189.000 425.800

# Find NAs
cereal[!complete.cases(cereal),]

hc <- hclust(d1, method="complete")
plot(hc)

plot(hc$height, type="l")
# elbow point at height ~ 250

# 3 clusters - one is quite large (having around 25 points, other 2 clusters are smaller having 9 and 7 points respectively)

## part b
# removing first column as it is a string column
KM1 <- kmeans(cereal[,2:9], 3)

# > KM1$cluster
# [1] 3 3 3 3 3 3 3 3 3 3 1 3 1 3 3 3 3 1 3 3 2 1 3 3 3 2 1 3 1 3 3 3 3 1 3 2 3 3 3 3 2 2 2
# > KM1$center
# Calories  Protein       Fat    Sodium    Fiber Carbohydrates     Sugar Potassium
# 1 117.14286 3.142857 1.4285714 190.00000 4.642857      12.50000 10.142857 205.71429
# 2  86.66667 2.333333 0.5000000  26.66667 1.450000      10.00000  5.833333  55.83333
# 3 110.00000 2.333333 0.9666667 209.00000 1.083333      15.51667  7.366667  61.83333

R <-  cor(cereal[,2:9])
S <-  var(cereal[,2:9])
ee_R <- eigen(R)
ee_S <- eigen(S)

PC_Q4 <- scale(cereal[,2:9], center=TRUE, scale=TRUE)%*%ee_R$vectors

# Variation explained by principal components
var_pc <- ee_S$values*100/sum(ee_S$values)
# 58.817721216 38.049708895  2.910601674  0.160611519  0.050540378  0.005721249  0.003238370  0.001856699

# Plot first 2 principal components along with above HC curve
par(mfrow=c(1,2))
plot(hc)
rect.hclust(hc, k=3, border="red")

plot(PC_Q4[,1], PC_Q4[, 2], col=KM1$cluster)
text(0.15+PC_Q4[,1], PC_Q4[, 2], labels=1:43, cex= 0.5)
# Both the curves give similar clustering - hard to distinguish


##########################################################
#################### Problem 5 ###########################
##########################################################

X <- as.matrix(read.table("T12-8.DAT", header = FALSE))
pot_type <- c("A", "B", "C", "D")
pot_site <- paste("P", 0:6, sep = "_")
rownames(X) <- pot_site
colnames(X) <- pot_type

# classical multi dimensional scaling
d <- dist(X)

cms <- cmdscale(d, k = 2, eig = TRUE)

plot(cms$points)
points(cms$points[,1], cms$points[,2], type="p")


# creating a bi-plot
a <- princomp(X)
biplot(a)

# interpretation -
# from attributes (Pot types) perspective
# Pot type C is of greater importance than other pot types in this data
# Pot type A, B and D are quite dissimilar to each other. while Pot type B and C are very similar

# From pot sites perspective 
# P_3 and P_0 are very similar sites
# P_6 site is most important from Pot type C perspective 

par(mfrow=c(1,2))
plot(cms$points)
biplot(a)

# Biplot seems to be more informative.
