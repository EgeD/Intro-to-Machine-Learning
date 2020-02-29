raw_images <- read.csv("hw03_data_set_images.csv", header = FALSE, sep = ",")
labels <- read.csv("hw03_data_set_labels.csv",header = FALSE, sep=",")

K <- 5 ##Number of Classes
N <- 125 # Number of Samples for Training

trainlabelA <- matrix(nrow =25, ncol=1, (rep(1 , 25)))
trainlabelB <- matrix(nrow =25, ncol=1, (rep(2 , 25)))
trainlabelC <- matrix(nrow =25, ncol=1, (rep(3 , 25)))
trainlabelD <- matrix(nrow =25, ncol=1, (rep(4 , 25)))
trainlabelE <- matrix(nrow =25, ncol=1, (rep(5 , 25)))

trained_Labels <- rbind(trainlabelA,trainlabelB,trainlabelC,trainlabelD,trainlabelE)


trainlabel_Final <- matrix(0,N,K)
trainlabel_Final[cbind(1:N,trained_Labels)] <-1 


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

train_set <- rbind(trainA,trainB,trainC,trainD,trainE)
train_set <- as.matrix(train_set)

test_set <- rbind(testA,testB,testC,testD,testE)
test_set <- as.matrix(test_set)

#Constants
D <- ncol(train_set) #Number of pixels 320
N <- 125 
#Given Learning Parameters
eta <- 0.005
epsilon <- 1e-3
H <- 20
max_iteration <- 200
set.seed(521)
#Functions
sigmoid <- function(a) {
  return (1 / (1 + exp(-a)))
}
softmax <- function(X,v) {
  scores <- cbind(1, X) %*% v
  scores <- exp(scores - matrix(apply(scores, MARGIN = 2, FUN = max), nrow = nrow(scores), ncol = ncol(scores), byrow = FALSE))
  scores <- scores / matrix(rowSums(scores), nrow(scores), ncol(scores), byrow = FALSE)
  return (scores)
}
safe_log <- function(x){
  return (log(x + 1e-100))
}
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

#Parameter Initialization
W <- matrix(runif((D + 1) * H, min = -0.01, max = 0.01), D + 1, H)
v <- matrix(runif((H + 1) * 5, min = -0.01, max = 0.01), H + 1, 5) 


Z <- sigmoid(cbind(1,train_set) %*% W)
train_predicted <- softmax(Z, v)
iteration <- 1
objective_values <- -sum(trainlabel_Final * safe_log(train_predicted))
while (1) {

  
  v <- v + eta * (t(cbind(1,Z)) %*% (trainlabel_Final - train_predicted))

  W <- W + eta * t((t((trainlabel_Final - train_predicted) %*% t(v[2:21,]) * (Z * (1 - Z))) %*% cbind(1, train_set)))
  
  Z <- sigmoid(cbind(1, train_set) %*% W)
  
  train_predicted <- softmax(Z, v)
  
  objective_values <- c(objective_values, -sum(trainlabel_Final * safe_log(train_predicted)))
  
  if (abs(objective_values[iteration + 1] - objective_values[iteration]) < epsilon | iteration >= max_iteration) {
    break
  }
  
  iteration <- iteration + 1
}

plot(1:(iteration + 1), objective_values,
     type = "l", lwd = 2, las = 1,
     xlab = "Iteration", ylab = "Error")

y_predicted <- matrix(nrow=125,ncol=1)
for(i in 1:125){
  num <- estimating_condition(train_predicted[i,1],train_predicted[i,2],train_predicted[i,3],train_predicted[i,4],train_predicted[i,5])
  y_predicted[i] <- num
}
confusion_matrix <- table(y_predicted, trained_Labels)
print(confusion_matrix)


###For Test Set
testlabelA <- matrix(nrow =14, ncol=1, (rep(1 , 14)))
testlabelB <- matrix(nrow =14, ncol=1, (rep(2 , 14)))
testlabelC <- matrix(nrow =14, ncol=1, (rep(3 , 14)))
testlabelD <- matrix(nrow =14, ncol=1, (rep(4 , 14)))
testlabelE <- matrix(nrow =14, ncol=1, (rep(5 , 14)))

testlabelPrev <- rbind(testlabelA,testlabelB,testlabelC,testlabelD,testlabelE)

testlabel <- matrix(0,70,K)
testlabel[cbind(1:70,testlabelPrev)] <-1 

Z <-sigmoid(cbind(1,test_set) %*% W)
test_predicted <- softmax(Z,v)

y_predicted2 <- matrix(nrow=70,ncol=1)
for(i in 1:70){
  num <- estimating_condition(test_predicted[i,1],test_predicted[i,2],test_predicted[i,3],test_predicted[i,4],test_predicted[i,5])
  y_predicted2[i] <- num
}
confusion_matrix2 <- table(y_predicted2, testlabelPrev)
print(confusion_matrix2)

