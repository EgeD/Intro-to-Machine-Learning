raw_images <- read.csv("hw02_data_set_images.csv", header = FALSE, sep = ",")
labels <- read.csv("hw02_data_set_labels.csv",header = FALSE, sep=",")

K <- 5 ##Number of Classes
N <- 125 # Number of Samples for Training


trainlabelA <- matrix(nrow =25, ncol=1, (rep(1 , 25)))
trainlabelB <- matrix(nrow =25, ncol=1, (rep(2 , 25)))
trainlabelC <- matrix(nrow =25, ncol=1, (rep(3 , 25)))
trainlabelD <- matrix(nrow =25, ncol=1, (rep(4 , 25)))
trainlabelE <- matrix(nrow =25, ncol=1, (rep(5 , 25)))

trainlabelPrev <- rbind(trainlabelA,trainlabelB,trainlabelC,trainlabelD,trainlabelE)


trainlabel <- matrix(0,N,K)
trainlabel[cbind(1:N,trainlabelPrev)] <-1 


trainA <- raw_images[1:25,]
testA <- raw_images[26:39,]

trainB <- raw_images[40:64,]
testB <- raw_images[65:78,]

trainC <- raw_images[79:103,]
testC <- raw_images[104:117,]

trainD <- raw_images[118:142,]
testD <- raw_images[143:156,]

trainE <- raw_images[157:181,]
testE <- raw_images[182:195,]

trainSet <- rbind(trainA,trainB,trainC,trainD,trainE)
trainSet <- as.matrix(trainSet)

testSet <- rbind(testA,testB,testC,testD,testE)
testSet <- as.matrix(testSet)

#####Given learning parameters and Functions
safe_log <- function(x){
  return (log(x + 1e-100))
}
eta <- 0.01
epsilon <- 1e-3
set.seed(521)

#Training Functions that are going to be used
gradient_W <- function(X, Y_truth, Y_predicted) {
  return (-sapply(X = 1:ncol(Y_truth), function(c) colSums(matrix((Y_truth[,c] - Y_predicted[,c])* Y_predicted[,c]*(1- Y_predicted[,c]), nrow = nrow(X), ncol = ncol(X), byrow = FALSE) * X)))
}
gradient_w0 <- function(Y_truth, Y_predicted) {
  return (-colSums((Y_truth - Y_predicted)* Y_predicted*(1- Y_predicted)))
}
sigmoid <-function (X, W, w0){
  
  return (1 / (1 + exp(-(X %*% W + w0))))
}

#Randomly Initializing the W and w0 for training
W <- matrix(runif(ncol(trainSet)*K, min = -0.01, max = 0.01), ncol(trainSet), K)
w0 <- runif(K, min = -0.01, max = 0.01)

iteration <- 1
objective_values <- c()
while (1) {
  Y_predicted <- sigmoid(trainSet, W, w0)
  objective_values <- c(objective_values, 0.5*sum((Y_predicted-trainlabel)^2))
  
  W_old <- W
  w0_old <- w0
  W <- W - eta * gradient_W(trainSet, trainlabel, Y_predicted)
  w0 <- w0 - eta * gradient_w0(trainlabel, Y_predicted)
  
  if (sqrt(sum((w0 - w0_old)^2) + sum((W - W_old)^2)) < epsilon) {
    break
  }
  
  iteration <- iteration + 1
}

plot(1:iteration, objective_values,
     type = "l", lwd = 2, las = 1,
     xlab = "Iteration", ylab = "Error")
###To find the max of the row
estimating_condition <- function(a, b , c , d, e){
  max_score <- max(a,b,c,d,e)
  if(max_score == a){
    return (1)
  }else if(max_score == b){
    return (2)
  }else if(max_score == c){
    return (3)
  }else if(max_score == d){
    return (4)
  }else{
    return (5)
  }
}

y_predicted <- matrix(nrow=125,ncol=1)
for(i in 1:125){
  num <- estimating_condition(Y_predicted[i,1],Y_predicted[i,2],Y_predicted[i,3],Y_predicted[i,4],Y_predicted[i,5])
  y_predicted[i] <- num
  
}


confusion_matrix <- table(y_predicted, trainlabelPrev)
print(confusion_matrix)

##################FOR THE TEST SET##########

testlabelA <- matrix(nrow =14, ncol=1, (rep(1 , 14)))
testlabelB <- matrix(nrow =14, ncol=1, (rep(2 , 14)))
testlabelC <- matrix(nrow =14, ncol=1, (rep(3 , 14)))
testlabelD <- matrix(nrow =14, ncol=1, (rep(4 , 14)))
testlabelE <- matrix(nrow =14, ncol=1, (rep(5 , 14)))

testlabelPrev <- rbind(testlabelA,testlabelB,testlabelC,testlabelD,testlabelE)

testlabel <- matrix(0,70,K)
testlabel[cbind(1:70,testlabelPrev)] <-1 

Y_predicted_test <- sigmoid(testSet, W, w0)

y_predicted2 <- matrix(nrow=70,ncol=1)
for(i in 1:70){
  num <- estimating_condition(Y_predicted_test[i,1],Y_predicted_test[i,2],Y_predicted_test[i,3],Y_predicted_test[i,4],Y_predicted_test[i,5])
  y_predicted2[i] <- num
  
}

confusion_matrix2 <- table(y_predicted2, testlabelPrev)
print(confusion_matrix2)

