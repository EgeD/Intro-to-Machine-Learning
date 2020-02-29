data_set <-read.csv("hw04_data_set.csv")
bin_width <- 3

x <- data_set$x
y <- data_set$y

x_train <- x[1:100]
y_train <- y[1:100]

x_test <- x[101:133]
y_test <- y[101:133]

point_colors <- c("blue","red")

minimum_value <- 0
maximum_value <- 60
data_interval <- seq(from = minimum_value, to = maximum_value, by = 0.01)

left_borders <- seq(from = minimum_value, to = maximum_value - bin_width, by = bin_width)
right_borders <- seq(from = minimum_value + bin_width, to = maximum_value, by = bin_width)

#####Regressogram Algorithm
g_head <- sapply(1:length(left_borders), function(x) {(sum((left_borders[x] < x_train & x_train <= right_borders[x])*y_train)) / (sum(left_borders[x] < x_train & x_train <= right_borders[x]))})
    
plot(x_train, y_train, type = "p", pch = 19, col = "blue",
     ylab = "y", xlab = "x", las = 1, main = sprintf("h = %g", bin_width))
points(x_test,y_test,pch=19,col="red")
for (b in 1:length(left_borders)) {
  lines(c(left_borders[b], right_borders[b]), c(g_head[b], g_head[b]), lwd = 2, col = "black")
  if (b < length(left_borders)) {
    lines(c(right_borders[b], right_borders[b]), c(g_head[b], g_head[b + 1]), lwd = 2, col = "black") 
  }
}
##Flooring a number to 1 above since our bins start from 1
up_floor <- function(x, bin_width){

  ifelse(x%%bin_width==0,x,(x/bin_width)+1)
}
g_head_pred <-sapply(up_floor(x_test,bin_width), function(x) {(sum((left_borders[x] < x_train & x_train <= right_borders[x])*y_train)) / (sum(left_borders[x] < x_train & x_train <= right_borders[x]))})

rmse_Regresoggram <- sqrt(sum((y_test-g_head_pred)^2)/33)


#####Running Mean Smoother Algorithm
bin_width <- 3

g_head_running <- sapply(data_interval, function(x) {(sum((x - 0.5 * bin_width < x_train & x_train <= x + 0.5 * bin_width)*y_train)) / (sum(x - 0.5 * bin_width < x_train & x_train <= x + 0.5 * bin_width))})

plot(x_train,  y_train, type = "p", pch = 19, col = "blue",
     ylab = "y", xlab = "x", las = 1, main = sprintf("h = %g", bin_width))
points(x_test,y_test,pch=19,col="red")

lines(data_interval, g_head_running, type = "l", lwd = 2, col = "black")

g_head_running_pred <-sapply(x_test, function(x) {(sum(((x - 0.5 * bin_width) < x_train & x_train <= (x + 0.5 * bin_width)) * y_train)) / sum((x - 0.5 * bin_width) < x_train & x_train <= (x + 0.5 * bin_width))})

rmse_RunningMean <- sqrt(sum((y_test-g_head_running_pred)^2)/33)

#####Kernel Smoother Algorithm

bin_width <- 1

g_head_kernel <-sapply(data_interval, function(x) {sum((1 / sqrt(2 * pi)) * exp(-0.5 * (x - x_train)^2 / bin_width^2)*y_train) / (sum((1 / sqrt(2 * pi)) * exp(-0.5 * (x - x_train)^2 / bin_width^2)))})

plot(x_train, y_train, type = "p", pch = 19, col = "blue",
     ylab = "y", xlab = "x", las = 1, main = sprintf("h = %g", bin_width))
points(x_test,y_test,pch=19,col="red")
lines(data_interval, g_head_kernel, type = "l", lwd = 2, col = "black")

g_head_kernel_pred <-sapply(x_test, 
      function(x) {sum((1 / sqrt(2 * pi)) * exp((-0.5 * (x - x_train)^2 / bin_width^2))*y_train) / (sum((1 / sqrt(2 * pi)) * exp(-0.5 * (x - x_train)^2 / bin_width^2)))})

rmse_Kernel <-sqrt(sum((y_test-g_head_kernel_pred)^2)/33)

cat("For RMSE Regressogram",rmse_Regresoggram,"when h is 3")
cat("For RMSE Running Mean",rmse_RunningMean,"when h is 3")
cat("For RMSE Kernel Smoother",rmse_Kernel,"when h is 1")

