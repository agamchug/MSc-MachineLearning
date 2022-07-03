library(caret) 
data("GermanCredit") #loading dataset
str(GermanCredit) #checking variable types
summary(GermanCredit) #checking summary statistics

## two categories of dummy variables have only zeros so they are to be removed.
GermanCredit[,c("Purpose.Vacation","Personal.Female.Single")] = list(NULL)

# create training and test sets
set.seed(12)
trainIndex = createDataPartition(GermanCredit$Class, p = 0.7, list = FALSE, times = 1) 
train.feature=GermanCredit[trainIndex,-10] # training features i.e. X's (-10 to remove the class column)
train.label=GermanCredit$Class[trainIndex] # training labels i.e y (the class column)
test.feature=GermanCredit[-trainIndex,-10] # test features i.e. X's (-10 to remove the class column)
test.label=GermanCredit$Class[-trainIndex] # test labels i.e y (the class column)

train = train.feature
train$Class = train.label

test = test.feature
test$Class = test.label

### DECISION TREE MODEL ###

##training decision tree
fitcontrol = trainControl(method="cv", number=5)
set.seed(100)
dt = train(Class ~., data=train, method = "rpart",
                    tuneLength=10, # trying 10 different cp values for cross-validation
                    trControl = fitcontrol)

plot(dt)
#inspecting the model: pruned tree with cp=0.333 and accuracy=0.737

dt 

plot(varImp(dt),top=10)
dt$finalModel

#plotting the tree

library(rattle)
fancyRpartPlot(dt$finalModel)

#test set predictions and errors
dt.pred = predict(dt, test[,-ncol(test)])
1-mean(dt.pred==test[,ncol(test)]) 
mean(dt.pred==test[,ncol(test)])

dt.pred_prob = predict(dt, test[,-ncol(test)], type='prob')[,2] #probabilities for ROC

#confusion matrix
table(test$Class, dt.pred)

### RANDOM FOREST MODEL ###

##training random forest
fitControl1=trainControl(method='cv', number=5)
mtry = (ncol(train)-1)^0.5 #setting the number of features per tree to cross-validate to square root of the total number of features ≈ 8 
set.seed(100) 
rf = train(Class~.,data=train, method='rf', metric='Accuracy',
           trControl=fitControl1, tuneLength=mtry, ntree=1000)

rf #best model with 18 sampled features per tree in the forest.

rf$finalModel

plot(rf)
varImp(rf)
plot(varImp(rf), main='Random Forest Variable Importance',top=10)

#predictions
rf.pred=predict(rf,test[,-ncol(test)])
1-mean(rf.pred==test[,ncol(test)]) 
mean(rf.pred==test[,ncol(test)])

rf.pred_prob=predict(rf,test[,-ncol(test)],type='prob')[,2] #probabilities for ROC

#confusion matrix
table(test$Class, rf.pred)

#Calculating and plotting ROC
library(ROCR)
roc_pred_dt =  prediction(dt.pred_prob, as.integer(test.label)-1)
roc_dt = performance(roc_pred_dt, measure="tpr", x.measure="fpr")
dt_auc = performance(roc_pred_dt, measure="auc")
dt_auc@y.values[[1]]

roc_pred_rf =  prediction(rf.pred_prob, as.integer(test.label)-1)
roc_rf = performance(roc_pred_rf, measure="tpr", x.measure="fpr")
rf_auc = performance(roc_pred_rf, measure="auc")
rf_auc@y.values[[1]]

par(mar=c(5,4,6,2), xpd=TRUE)
plot(roc_rf, col="orange", lwd=2)
plot(roc_dt, col="blue", lwd=2, add=TRUE) 
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=0.5, lty=2)
legend("topright", c("Random Forest", "Decision Tree"), lty=1, 
       col = c("orange", "blue"), bty="n", inset=c(0,-0.15))

###--------###
