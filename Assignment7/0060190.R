library("lattice")
library("ggplot2")
library("caret")
library("e1071")
library("randomForest")
library("AUC")
library("tree")
set.seed(421)

X_train <- read.csv("training_data.csv", header = TRUE)
X_test <- read.csv("test_data.csv", header = TRUE)
y_train <- as.factor(read.csv("training_labels.csv", header = FALSE)[,1])
X_train_raw <- cbind(X_train, y=y_train)

###Initial Approach with 7 different trees with mtry = 5
### AUC -> 0.8355636
#randomForestClassifier <- randomForest(y ~ ., data=cbind(X_train, y=y_train), mtry=5,ntree=7)
#training_scores <- predict(randomForestClassifier, X_train,type="prob")

#roc_curve <- roc(predictions = training_scores[,2], labels = y_train)
#auc(roc_curve)
#plot(roc_curve$fpr, roc_curve$tpr, lwd = 2, col = "blue", type = "b", las = 1)

###Second Approach with 7 different trees that are trained with 5 fold cross validation TAKES A LOT OF TIME!!
#crossValidated_RandomForest<- rfcv(X_train, y_train, cv.fold=5, ntree=7)
#with(crossValidated_RandomForest, plot(n.var,error.cv, log="x", type="o", lwd=2))
#training_scores2 <- predict(crossValidated_RandomForest, X_train,type="prob")

### Approach with 7 different trees but 7 candidate samples mtry = 7
### AUC -> 0.9221613
randomForestClassifier7 <- randomForest(y ~ ., data=cbind(X_train, y=y_train), mtry=7,ntree=7)
training_scores7 <- predict(randomForestClassifier7, X_train,type="prob")

roc_curve <- roc(predictions = training_scores7[,2], labels = y_train)
auc(roc_curve)
plot(roc_curve$fpr, roc_curve$tpr, lwd = 2, col = "blue", type = "b", las = 1)

### Approach with 7 different trees but 9 candidate samples mtry = 9
### AUC -> 0.9617489
#randomForestClassifier9 <- randomForest(y ~ ., data=cbind(X_train, y=y_train), mtry=9,ntree=7)
#training_scores9 <- predict(randomForestClassifier9, X_train,type="prob")

#roc_curve <- roc(predictions = training_scores9[,2], labels = y_train)
#auc(roc_curve)
#plot(roc_curve$fpr, roc_curve$tpr, lwd = 2, col = "blue", type = "b", las = 1)

###Finding the Best Solution For My train set as divided into test and train set
ind<-sample(seq_len(nrow(X_train_raw)), size = 0.7*nrow(X_train_raw))
X_trainSample<-X_train_raw[ind,1:142]
X_testSample<-X_train_raw[-ind,1:142]

X_train_Y<-X_train_raw[ind,143:143]
X_test_Y<-X_train_raw[-ind,143:143]

names(X_test_Y)[1]<-"X_test_Y"
names(X_train_Y)[1]<-"X_train_Y"

#randomForestClassifierTrainTest <- randomForest(X_train_Y ~. , data=cbind(X_trainSample,X_train_Y), mtry=7,ntree=7) AUC -> 0.6731505
#randomForestClassifierTrainTest3 <- randomForest(X_train_Y ~. , data=cbind(X_trainSample,X_train_Y), mtry=9,ntree=9)  AUC -> 0.6903101
#randomForestClassifierTrainTest <- randomForest(X_train_Y ~. , data=cbind(X_trainSample,X_train_Y), mtry=10,ntree=13) AUC -> 0.7116886

randomForestClassifierTrainTest <- randomForest(X_train_Y ~. , data=cbind(X_trainSample,X_train_Y), mtry=13,ntree=17) # AUC -> 0.7228575
training_scoresForTrainTest <- predict(randomForestClassifierTrainTest, X_testSample,type="prob") ## getting 0's so it means that we don't give any chance for them to be 0

roc_curve <- roc(predictions = training_scoresForTrainTest[,2], labels = X_test_Y)
auc(roc_curve)
plot(roc_curve$fpr, roc_curve$tpr, lwd = 2, col = "blue", type = "b", las = 1)

randomForestClassifierForFinal <- randomForest(y ~., data=cbind(X_train, y=y_train), mtry=13,ntree=17)
training_scoresFinal <-predict(randomForestClassifierForFinal, X_test, type="prob") ### It's a high chance that there is be overfitting due to certain probabilities as 1

roc_curve <- roc(predictions = training_scoresFinal[,2], labels = y_train) ###An Overfitting Case is observed through here
auc(roc_curve)
plot(roc_curve$fpr, roc_curve$tpr, lwd = 2, col = "blue", type = "b", las = 1)

#randomForestClassifierTrainTest <- randomForest(X_train_Y ~. , data=cbind(X_trainSample,X_train_Y), mtry=15,ntree=21)  AUC -> 0.7282156
#training_scoresForTrainTest <- predict(randomForestClassifierTrainTest, X_testSample,type="prob")
#roc_curve <- roc(predictions = training_scoresForTrainTest[,2], labels = X_test_Y)
#auc(roc_curve)
#plot(roc_curve$fpr, roc_curve$tpr, lwd = 2, col = "blue", type = "b", las = 1)

#training_scoresForTrainTest <- predict(randomForestClassifierTrainTest, X_train,type="prob") AUC -> 0.9391187
#roc_curve <- roc(predictions = training_scoresForTrainTest[,2], labels = y_train) 
#auc(roc_curve)

##Combining The Dirty Solution with My solution to get a better Result to reduce the probability of Overfitting my model to the train set

tree_classifier_for_combine <- tree(y ~. , data=cbind(X_train,y=y_train))

dirtyClassifiedScores <- predict(tree_classifier_for_combine,X_test)
final_scores <- (dirtyClassifiedScores+training_scoresFinal)/2 ##Taking the average of two learners

write.table(final_scores[,2], file = "test_predictions.csv", row.names = FALSE, col.names = FALSE)


