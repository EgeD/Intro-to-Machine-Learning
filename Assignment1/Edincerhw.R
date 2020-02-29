raw_images <- read.csv("hw01_data_set_images.csv", header = FALSE, sep = ",")
labels <- read.csv("hw01_data_set_labels.csv", header = FALSE, sep = ",")

trainingA <- raw_images[1:25,]
testA <- raw_images[26:39,]

trainingB <- raw_images[40:64,]
testB <- raw_images[65:78,]

trainingC <- raw_images[79:103,]
testC <- raw_images[104:117,]

trainingD <- raw_images[118:142,]
testD <- raw_images[143:156,]

trainingE <- raw_images[157:181,]
testE <- raw_images[182:195,]

trainingSet <- rbind(trainingA,trainingB,trainingC,trainingD,trainingE)
testSet <- rbind(testA,testB,testC,testD,testE)

column_sumsA <- colSums(trainingA)
column_sumsA <- column_sumsA / 25

column_sumsB <- colSums(trainingB)
column_sumsB <- column_sumsB /25

column_sumsC <- colSums(trainingC)
column_sumsC <- column_sumsC /25

column_sumsD <- colSums(trainingD)
column_sumsD <- column_sumsD /25

column_sumsE <- colSums(trainingE)
column_sumsE <- column_sumsE /25

pcdA <- matrix(column_sumsA,nrow=1, ncol=320)
pcdB <- matrix(column_sumsB,nrow=1, ncol=320)
pcdC <- matrix(column_sumsC,nrow=1, ncol=320)
pcdD <- matrix(column_sumsD,nrow=1, ncol=320)
pcdE <- matrix(column_sumsE,nrow=1, ncol=320)

pcd <- rbind(pcdA,pcdB,pcdC,pcdD,pcdE)
pcd <- t(pcd) ### For the convention of the given pdf so that the printouts will be same

safe_log <- function(x){
  return (log(x + 1e-100))
}

trainingSet <- as.matrix(trainingSet)

tpcdA <- t(pcdA)

step1A <- trainingSet %*% safe_log(tpcdA)
step2A <-(1-trainingSet) %*% safe_log((1-tpcdA))
step3A <- step1A + step2A
gcdA <- step3A + safe_log(0.2)

tpcdB <- t(pcdB)

step1B <- trainingSet %*% safe_log(tpcdB)
step2B <-(1-trainingSet) %*% safe_log((1-tpcdB))
step3B <- step1B + step2B
gcdB <- step3B + safe_log(0.2)

tpcdC <- t(pcdC)

step1C <- trainingSet %*% safe_log(tpcdC)
step2C <-(1-trainingSet) %*% safe_log((1-tpcdC))
step3C <- step1C + step2C
gcdC <- step3C + safe_log(0.2)

tpcdD <- t(pcdD)

step1D <- trainingSet %*% safe_log(tpcdD)
step2D <-(1-trainingSet) %*% safe_log((1-tpcdD))
step3D <- step1D + step2D
gcdD <- step3D + safe_log(0.2)

tpcdE <- t(pcdE)

step1E <- trainingSet %*% safe_log(tpcdE)
step2E <-(1-trainingSet) %*% safe_log((1-tpcdE))
step3E <- step1E + step2E
gcdE <- step3E + safe_log(0.2)

estimating_condition <- function(a, b , c , d,e){
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

whole_gcd <- cbind(gcdA,gcdB,gcdC,gcdD,gcdE)
gcdMat <- matrix(nrow=125,ncol=1)
for(i in 1:125){
  num <- estimating_condition(whole_gcd[i,1],whole_gcd[i,2],whole_gcd[i,3],whole_gcd[i,4],whole_gcd[i,5])
  gcdMat[i] <- num
  
}
testSet <- as.matrix(testSet)

test_step1A <- testSet %*% safe_log(tpcdA)
test_step2A <-(1-testSet) %*% safe_log((1-tpcdA))
test_step3A <- test_step1A + test_step2A
test_gcdA <- test_step3A + safe_log(0.2)

test_step1B <- testSet %*% safe_log(tpcdB)
test_step2B <-(1-testSet) %*% safe_log((1-tpcdB))
test_step3B <- test_step1B + test_step2B
test_gcdB <- test_step3B + safe_log(0.2)

test_step1C <- testSet %*% safe_log(tpcdC)
test_step2C <-(1-testSet) %*% safe_log((1-tpcdC))
test_step3C <- test_step1C + test_step2C
test_gcdC <- test_step3C + safe_log(0.2)

test_step1D <- testSet %*% safe_log(tpcdD)
test_step2D <-(1-testSet) %*% safe_log((1-tpcdD))
test_step3D <- test_step1D + test_step2D
test_gcdD <- test_step3D + safe_log(0.2)

test_step1E <- testSet %*% safe_log(tpcdE)
test_step2E <-(1-testSet) %*% safe_log((1-tpcdE))
test_step3E <- test_step1E + test_step2E
test_gcdE <- test_step3E + safe_log(0.2)

test_whole_gcd <- cbind(test_gcdA,test_gcdB,test_gcdC,test_gcdD,test_gcdE)
test_gcdMat <- matrix(nrow=70,ncol=1)
for(i in 1:70){
  num <- estimating_condition(test_whole_gcd[i,1],test_whole_gcd[i,2],test_whole_gcd[i,3],test_whole_gcd[i,4],test_whole_gcd[i,5])
  test_gcdMat[i] <- num
}

trainlabelA <- matrix(nrow =25, ncol=1, (rep(1 , 25)))
trainlabelB <- matrix(nrow =25, ncol=1, (rep(2 , 25)))
trainlabelC <- matrix(nrow =25, ncol=1, (rep(3 , 25)))
trainlabelD <- matrix(nrow =25, ncol=1, (rep(4 , 25)))
trainlabelE <- matrix(nrow =25, ncol=1, (rep(5 , 25)))

trainlabel <- rbind(trainlabelA,trainlabelB,trainlabelC,trainlabelD,trainlabelE)

testlabelA <- matrix(nrow =14, ncol=1, (rep(1 , 14)))
testlabelB <- matrix(nrow =14, ncol=1, (rep(2 , 14)))
testlabelC <- matrix(nrow =14, ncol=1, (rep(3 , 14)))
testlabelD <- matrix(nrow =14, ncol=1, (rep(4 , 14)))
testlabelE <- matrix(nrow =14, ncol=1, (rep(5 , 14)))

testlabel <- rbind(testlabelA,testlabelB,testlabelC,testlabelD,testlabelE)

train_confusion_matrix <- table(gcdMat,trainlabel)
test_confusion_matrix <- table(test_gcdMat,testlabel)
