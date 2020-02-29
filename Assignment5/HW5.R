data_set <- read.csv("hw05_data_set.csv")
# get X and y values
X <- data_set$x
y <- data_set$y
# get train and test splits
X_train <- X[1:100]
y_train <- y[1:100]
X_test <- X[101:133]
y_test <- y[101:133]
# get numbers of train and test samples
N_train <- length(y_train)
N_test <- length(y_test)
# create necessary data structures
node_indices <- list()
is_terminal <- c()
need_split <- c()
node_mean <- c()
#Pre_pruning for Q1
P <- 10
node_splits <- c()
# put all training instances into the root node
node_indices <- list(1:N_train)
is_terminal <- c(FALSE)
need_split <- c(TRUE)
RMSE_array <- c()

# learning algorithm
while (1) {
  # find nodes that need splitting
  split_nodes <- which(need_split)
  # check whether we reach all terminal nodes
  if (length(split_nodes) == 0) {
    break
  }
  # find best split positions for all nodes
  for (split_node in split_nodes) {
    data_indices <- node_indices[[split_node]]
    need_split[split_node] <- FALSE
    node_mean[[split_node]] <- mean(y_train[data_indices])
    # check whether node is smaller than P
    if (length(y_train[data_indices]) <= P) {
      is_terminal[split_node] <- TRUE
    } else {
      is_terminal[split_node] <- FALSE
      
      #Initializing Scores and Splits  
      best_scores <- 0
      best_splits <- 0
      
      unique_values <- sort(unique(X_train[data_indices]))
      split_positions <- (unique_values[-1] + unique_values[-length(unique_values)]) / 2
      split_scores <- rep(0, length(split_positions))
      for (s in 1:length(split_positions)) {
        left_indices <- data_indices[which(X_train[data_indices] <= split_positions[s])]
        right_indices <- data_indices[which(X_train[data_indices] > split_positions[s])]
        #(y_predicted-y_truth)^2 for left and right applied here
        split_scores[s] <- sum((mean(y_train[left_indices])-y_train[left_indices])^2)+
          sum((mean(y_train[right_indices])-y_train[right_indices])^2)
        
        best_scores <- min(split_scores)
        best_splits <- split_positions[which.min(split_scores)]
      }
      node_splits[split_node] <- best_splits
      
      # Left Tree
      left_indices <- data_indices[which(X_train[data_indices] <= best_splits)]
      node_indices[[2 * split_node]] <- left_indices
      is_terminal[2 * split_node] <- FALSE
      need_split[2 * split_node] <- TRUE
      
      # Right Tree
      right_indices <- data_indices[which(X_train[data_indices] > best_splits)]
      node_indices[[2 * split_node + 1]] <- right_indices
      is_terminal[2 * split_node + 1] <- FALSE
      need_split[2 * split_node + 1] <- TRUE
    }
  }
}

y_predicted <- rep(0, N_test)

for (i in 1:N_test) {
  index <- 1
  while (1) {
    y_predicted[i] <- node_mean[index]
    if (is_terminal[index] == TRUE || is.na(node_splits[index])) {
      break
    }
    if (X_test[i] <= node_splits[index]) {
      index <- index * 2
    } else {
      index <- index * 2 + 1
    }
  }
}

RMSE1 <- sqrt(sum((y_test-y_predicted)^2)/length(y_test))
sprintf("RMSE is  %s when P is %i", RMSE1, P)

#plotting the fit
data_interval <- seq(from = 0, to = 60, by = 0.01)
regress_line <- rep(0, length(data_interval))

for (i in 1:length(data_interval)) {
  index <- 1
  while (1) {
    regress_line[i] <- node_mean[index]
    if (is_terminal[index] == TRUE || is.na(node_splits[index])) {
      break
    }
    if (data_interval[i] <= node_splits[index]) {
      index <- index * 2
    } else {
      index <- index * 2 + 1
    }
  }
}
plot(X_train,y_train,type = "p", pch = 19, col="blue" ,
     ylim = c(-150, 100), xlim = c(0, 60),
     ylab = "y", xlab = "x", las = 1,main=sprintf("P = %g", P) )
points(X_test,y_test, type = "p" , pch=19 , col="red")
lines(data_interval,regress_line)


###1 to 20 iterations for Question 5
P_values <- c(1:20)
for(p in P_values){
  
  node_indices <- list()
  is_terminal <- c()
  need_split <- c()
  node_mean <- c()
  node_splits <- c()
  node_indices <- list(1:N_train)
  is_terminal <- c(FALSE)
  need_split <- c(TRUE)
  
  while (1) {
    
    split_nodes <- which(need_split)
    
    if (length(split_nodes) == 0) {
      break
    }
    
    for (split_node in split_nodes) {
      data_indices <- node_indices[[split_node]]
      need_split[split_node] <- FALSE
      node_mean[[split_node]] <- mean(y_train[data_indices])
      
      if (length(y_train[data_indices]) <= p) {
        is_terminal[split_node] <- TRUE
      } else {
        is_terminal[split_node] <- FALSE
        
        
        best_scores <- 0
        best_splits <- 0
        
        unique_values <- sort(unique(X_train[data_indices]))
        split_positions <- (unique_values[-1] + unique_values[-length(unique_values)]) / 2
        split_scores <- rep(0, length(split_positions))
        for (s in 1:length(split_positions)) {
          left_indices <- data_indices[which(X_train[data_indices] <= split_positions[s])]
          right_indices <- data_indices[which(X_train[data_indices] > split_positions[s])]
          #(y_predicted-y_truth)^2 for left and right applied here
          split_scores[s] <- sum((mean(y_train[left_indices])-y_train[left_indices])^2)+
            sum((mean(y_train[right_indices])-y_train[right_indices])^2)
          
          best_scores <- min(split_scores)
          best_splits <- split_positions[which.min(split_scores)]
        }
        node_splits[split_node] <- best_splits
        
        #Left Tree
        left_indices <- data_indices[which(X_train[data_indices] <= best_splits)]
        node_indices[[2 * split_node]] <- left_indices
        is_terminal[2 * split_node] <- FALSE
        need_split[2 * split_node] <- TRUE
        
        #Right Tree
        right_indices <- data_indices[which(X_train[data_indices] > best_splits)]
        node_indices[[2 * split_node + 1]] <- right_indices
        is_terminal[2 * split_node + 1] <- FALSE
        need_split[2 * split_node + 1] <- TRUE
      }
    }
  }
  
  y_predicted <- rep(0, N_test)
  
  for (i in 1:N_test) {
    index <- 1
    while (1) {
      y_predicted[i] <- node_mean[index]
      if (is_terminal[index] == TRUE || is.na(node_splits[index])) {
        break
      }
      if (X_test[i] <= node_splits[index]) {
        index <- index * 2
      } else {
        index <- index * 2 + 1
      }
    }
  }
  
  RMSE <- sqrt(sum((y_test-y_predicted)^2)/length(y_test))
  RMSE_array <- c(RMSE_array, RMSE)
}
###Plotting RMSE_array
plot(P_values,RMSE_array,type = "p", pch = 10, col="red" ,
     ylim = c(min(RMSE_array), max(RMSE_array)),
     ylab = "RMSE", xlab = "P", las = 1 )
lines(P_values,RMSE_array)
