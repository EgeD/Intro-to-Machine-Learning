set.seed(521)
## It takes approximately 48 seconds to run the code due to multiple gaussian calculations
library(MASS) # For Multivariate Distribution of data points
#First Data
covariance_f <- matrix(c(+0.8, -0.6, -0.6, +0.8), nrow=2, ncol=2)
mean_f <- c(+2.5, +2.5)
size_f <-50
#Second Data
covariance_s <- matrix(c(+0.8, +0.6, +0.6, +0.8), nrow=2, ncol=2)
mean_s <- c(-2.5, +2.5)
size_s <-50
#Third Data
covariance_t <- matrix(c(+0.8, -0.6, -0.6, +0.8), nrow=2, ncol=2)
mean_t <-c(-2.5, -2.5)
size_t <-50
#Fourth Data
covariance_fo <- matrix(c(+0.8, +0.6, +0.6, +0.8), nrow=2, ncol=2)
mean_fo <- c(+2.5, -2.5)
size_fo <-50
#Fifth Data
covariance_fi <- matrix(c(+1.6, 0.0, 0.0, +1.6), nrow=2, ncol=2)
mean_fi <-c(0, 0)
size_fi <-100

first_data <- mvrnorm(n=size_f, mu=mean_f,Sigma=covariance_f)
second_data <- mvrnorm(n=size_s, mu=mean_s,Sigma=covariance_s)
third_data <- mvrnorm(n=size_t, mu=mean_t,Sigma=covariance_t)
fourth_data <- mvrnorm(n=size_fo, mu=mean_fo,Sigma=covariance_fo)
fifth_data <- mvrnorm(n=size_fi, mu=mean_fi,Sigma=covariance_fi)
X <- rbind(first_data, second_data, third_data, fourth_data,fifth_data)

plot(first_data[,1], first_data[,2], type = "p", pch = 19, col = "black", las = 1,
     xlim = c(-6, 6), ylim = c(-6, 6),
     xlab = "x1", ylab = "x2")

points(second_data[,1], second_data[,2], type = "p", pch = 19, col = "black")
points(third_data[,1], third_data[,2], type = "p", pch = 19, col = "black")
points(fourth_data[,1], fourth_data[,2], type = "p", pch = 19, col = "black")
points(fifth_data[,1], fifth_data[,2], type = "p", pch = 19, col = "black")

#Running K-means algorithm for two iterations to begin EM algorithm
centroids <- NULL
assignments <- NULL
N <-300 
K <- 5

for(i in 1:2){
  if (is.null(centroids) == TRUE) {
    centroids <- X[sample(1:N, K),]
  } else {
    for (k in 1:K) {
      centroids[k,] <- colMeans(X[assignments == k,])
    }  
  }
  centroids <<- centroids
  D <- as.matrix(dist(rbind(centroids, X), method = "euclidean"))
  D <- D[1:nrow(centroids), (nrow(centroids) + 1):(nrow(centroids) + nrow(X))]
  assignments <<- sapply(1:ncol(D), function(i) {which.min(D[,i])})
}

covariance_matrix1 <- cov(X[assignments == 1,])
covariance_matrix2 <- cov(X[assignments == 2,])
covariance_matrix3 <- cov(X[assignments == 3,])
covariance_matrix4 <- cov(X[assignments == 4,])
covariance_matrix5 <- cov(X[assignments == 5,])

mean_vector <-centroids
##Calculating gaussian with trained mean vectors
gaussian1 <- sapply(1:N, function (i) {
  (1/(2*pi * det(covariance_matrix1)^0.5))  * exp (-0.5 * t( X[i,] - mean_vector[1,]) %*% ginv(covariance_matrix1) %*% ((X[i,]-mean_vector[1,])))
})
gaussian2 <- sapply(1:N, function (i) {
  (1/(2*pi * det(covariance_matrix2)^0.5))  * exp (-0.5 * t( X[i,] - mean_vector[2,]) %*% ginv(covariance_matrix2) %*% ((X[i,]-mean_vector[2,])))
})
gaussian3 <- sapply(1:N, function (i) {
  (1/(2*pi * det(covariance_matrix3)^0.5))  * exp (-0.5 * t( X[i,] - mean_vector[3,]) %*% ginv(covariance_matrix3) %*% ((X[i,]-mean_vector[3,])))
})
gaussian4 <- sapply(1:N, function (i) {
  (1/(2*pi * det(covariance_matrix4)^0.5))  * exp (-0.5 * t( X[i,] - mean_vector[4,]) %*% ginv(covariance_matrix4) %*% ((X[i,]-mean_vector[4,])))
})

gaussian5 <- sapply(1:N, function (i) {
  (1/(2*pi * det(covariance_matrix5)^0.5))  * exp (-0.5 * t( X[i,] - mean_vector[5,]) %*% ginv(covariance_matrix5) %*% ((X[i,]-mean_vector[5,])))
})
#Calculating prior probabilities
prior <- c(sum(assignments == 1)/N, sum(assignments == 2)/N, sum(assignments == 3)/N, sum(assignments == 4)/N, sum(assignments == 5)/N)

gaussian_matrix <- matrix(c(gaussian1,gaussian2,gaussian3,gaussian4,gaussian5),nrow=300,ncol = 5)
##Empty matrices for the contour (randomly assigned values inside the matrix)
result_H <- matrix(-1,nrow = 300, ncol = 5)
final_convdraw <- array(c(1,2,3,4,5),c(2,2,5))

H <- NULL

for(i in 1:100){
  for(k in 1:K){
    H <- gaussian_matrix[,k]*prior[k] / rowSums(sapply(1:K, function(x) {
      gaussian_matrix[,x]*prior[x]
    }))
    mean_vector[k,] <- colSums(H*X) / sum(H)
    #Covariance t + 1
    nextcov <- matrix(rowSums(sapply(1:N, function(a) {
      H[a] * ((X[a,] - mean_vector[k,])) %*% t((X[a,] - mean_vector[k,]))
      })),ncol=2)/sum(H)
    #Prior t + 1
    prior[k] <- sum(H) / N
    
    final_convdraw[,,k] <-nextcov 
    #New gaussain calculated with Prior t + 1 and Covariance t + 1
    gaussian_matrix[,k] <- sapply(1:N, function (f) {
      (1/(2*pi * det(nextcov)^0.5))  * exp (-0.5 * t( X[f,] - mean_vector[k,]) %*% ginv(nextcov) %*% ((X[f,]-mean_vector[k,])))
    })
    #Resulting H values are stored in result_H matrix in order to apply the arg max ffor the scores
    result_H[,k] <- H
  }
}

assignments <- sapply(1:N, function(i) {which.max(result_H[i,])})
print(mean_vector)

##Drawing part that is taken from the lab 3 
colors <- c("#ff7f00","#e31a1c","#33a02c","#1f78b4", "#6a3d9a")
plot(X[,1], X[,2], col = colors[assignments], xlim = c(-6, 6), ylim = c(-6, 6), xlab = "x1", ylab = "x2", pch = 19)

x1_interval <- seq(from = -6, to = +6, by = 0.06)
x2_interval <- seq(from = -6, to = +6, by = 0.06)
x1_grid <- matrix(x1_interval, nrow = length(x1_interval), ncol = length(x1_interval), byrow = FALSE)
x2_grid <- matrix(x2_interval, nrow = length(x2_interval), ncol = length(x2_interval), byrow = TRUE)

##Solid Lines
f <- function(x1, x2) {
  (1/(2*pi * det(final_convdraw[,,1])^0.5))  * exp (-0.5 * matrix((c(x1,x2) - mean_vector[1,]), ncol=2) %*% ginv(final_convdraw[,,1]) %*% t(((matrix((c(x1,x2)-mean_vector[1,]),ncol=2)))))}
discriminant_values <- matrix(mapply(f, x1_grid, x2_grid), nrow(x2_grid), ncol(x2_grid))
contour(x1_interval, x2_interval, discriminant_values, levels = c(0.05), add = TRUE, lwd = 2, drawlabels = FALSE)

f <- function(x1, x2) {
  (1/(2*pi * det(final_convdraw[,,2])^0.5))  * exp (-0.5 * matrix((c(x1,x2) - mean_vector[2,]), ncol=2) %*% ginv(final_convdraw[,,2]) %*% t(((matrix((c(x1,x2)-mean_vector[2,]),ncol=2)))))}
discriminant_values <- matrix(mapply(f, x1_grid, x2_grid), nrow(x2_grid), ncol(x2_grid))
contour(x1_interval, x2_interval, discriminant_values, levels = c(0.05), add = TRUE, lwd = 2, drawlabels = FALSE)

f <- function(x1, x2) {
  (1/(2*pi * det(final_convdraw[,,3])^0.5))  * exp (-0.5 * matrix((c(x1,x2) - mean_vector[3,]), ncol=2) %*% ginv(final_convdraw[,,3]) %*% t(((matrix((c(x1,x2)-mean_vector[3,]),ncol=2)))))}
discriminant_values <- matrix(mapply(f, x1_grid, x2_grid), nrow(x2_grid), ncol(x2_grid))
contour(x1_interval, x2_interval, discriminant_values, levels = c(0.05), add = TRUE, lwd = 2, drawlabels = FALSE)

f <- function(x1, x2) {
  (1/(2*pi * det(final_convdraw[,,4])^0.5))  * exp (-0.5 * matrix((c(x1,x2) - mean_vector[4,]), ncol=2) %*% ginv(final_convdraw[,,4]) %*% t(((matrix((c(x1,x2)-mean_vector[4,]),ncol=2)))))}
discriminant_values <- matrix(mapply(f, x1_grid, x2_grid), nrow(x2_grid), ncol(x2_grid))
contour(x1_interval, x2_interval, discriminant_values, levels = c(0.05), add = TRUE, lwd = 2, drawlabels = FALSE)

f <- function(x1, x2) {
  (1/(2*pi * det(final_convdraw[,,5])^0.5))  * exp (-0.5 * matrix((c(x1,x2) - mean_vector[5,]), ncol=2) %*% ginv(final_convdraw[,,5]) %*% t(((matrix((c(x1,x2)-mean_vector[5,]),ncol=2)))))}
discriminant_values <- matrix(mapply(f, x1_grid, x2_grid), nrow(x2_grid), ncol(x2_grid))
contour(x1_interval, x2_interval, discriminant_values, levels = c(0.05), add = TRUE, lwd = 2, drawlabels = FALSE)

##Dashed Lines
f <- function(x1, x2) {
  (1/(2*pi * det(covariance_f)^0.5))  * exp (-0.5 * matrix((c(x1,x2) - mean_f), ncol=2) %*% ginv(covariance_f) %*% t(((matrix((c(x1,x2)-mean_f),ncol=2)))))}
discriminant_values <- matrix(mapply(f, x1_grid, x2_grid), nrow(x2_grid), ncol(x2_grid))
contour(x1_interval, x2_interval, discriminant_values, levels = c(0.05), add = TRUE, lwd = 2, drawlabels = FALSE, lty = 2)

f <- function(x1, x2) {
  (1/(2*pi * det(covariance_s)^0.5))  * exp (-0.5 * matrix((c(x1,x2) - mean_s), ncol=2) %*% ginv(covariance_s) %*% t(((matrix((c(x1,x2)-mean_s),ncol=2)))))}
discriminant_values <- matrix(mapply(f, x1_grid, x2_grid), nrow(x2_grid), ncol(x2_grid))
contour(x1_interval, x2_interval, discriminant_values, levels = c(0.05), add = TRUE, lwd = 2, drawlabels = FALSE, lty = 2)

f <- function(x1, x2) {
  (1/(2*pi * det(covariance_t)^0.5))  * exp (-0.5 * matrix((c(x1,x2) - mean_t), ncol=2) %*% ginv(covariance_t) %*% t(((matrix((c(x1,x2)-mean_t),ncol=2)))))}
discriminant_values <- matrix(mapply(f, x1_grid, x2_grid), nrow(x2_grid), ncol(x2_grid))
contour(x1_interval, x2_interval, discriminant_values, levels = c(0.05), add = TRUE, lwd = 2, drawlabels = FALSE, lty = 2)

f <- function(x1, x2) {
  (1/(2*pi * det(covariance_fo)^0.5))  * exp (-0.5 * matrix((c(x1,x2) - mean_fo), ncol=2) %*% ginv(covariance_fo) %*% t(((matrix((c(x1,x2)-mean_fo),ncol=2)))))}
discriminant_values <- matrix(mapply(f, x1_grid, x2_grid), nrow(x2_grid), ncol(x2_grid))
contour(x1_interval, x2_interval, discriminant_values, levels = c(0.05), add = TRUE, lwd = 2, drawlabels = FALSE, lty = 2)

f <- function(x1, x2) {
  (1/(2*pi * det(covariance_fi)^0.5))  * exp (-0.5 * matrix((c(x1,x2) - mean_fi), ncol=2) %*% ginv(covariance_fi) %*% t(((matrix((c(x1,x2)-mean_fi),ncol=2)))))}
discriminant_values <- matrix(mapply(f, x1_grid, x2_grid), nrow(x2_grid), ncol(x2_grid))
contour(x1_interval, x2_interval, discriminant_values, levels = c(0.05), add = TRUE, lwd = 2, drawlabels = FALSE, lty = 2)
