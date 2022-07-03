thyroid = read.table('newthyroid.txt', sep=',', header=TRUE, stringsAsFactors = TRUE)
str(thyroid)
summary(thyroid)

library(caret)

#splitting the dataset 10 times with a 70/30 division
set.seed(7)
trainIndex = createDataPartition(thyroid$class, p = 0.7, list = FALSE, times = 10)

#empty lists to append auc values 
vecKNN = c()
vecLDA = c()

for (i in 1:10) { #iterating over the 10 sets of sampled datasets index values within trainIndex
  
  train = thyroid[trainIndex[,i], ] #training set
  test = thyroid[-trainIndex[,i], ] #testing set
  
  
  ### KNN with k tuned to (3,5,7,9,11,13,15) ###
  fitControlKNN = trainControl(method = "cv", number = 5, 
                               classProbs=TRUE, summaryFunction = twoClassSummary) #to show ROC and probabilities
  
  kGrid = expand.grid(k=c(3,5,7,9,11,13,15)) #the specified k values to cross validate
  
  set.seed(77)
  knnFit = train(class~.,data=train, method='knn', 
                 trControl=fitControlKNN, tuneGrid=kGrid, 
                 preProcess=c('center','scale'), metric='ROC') 
  #knnFit
  print(knnFit$finalModel)
  
  #prediction and accuracy
  knn_pred = predict(knnFit, test[,-1],type='prob')[,2]
  mean(predict(knnFit,test[,-1])==test$class)
  
  library(ROCR)
  #calculating AUC
  roc_knnFit=  prediction(knn_pred, test$class)
  aucKNN= performance(roc_knnFit, measure='auc')
  aucKNN=aucKNN@y.values[[1]]
  
  vecKNN = c(vecKNN, aucKNN) #appending auc in each iteration
  
  ### LDA (two classes only so no parameter to tune) ###
  
  set.seed(777)
  ldaFit = train(class~.,data=train, method='lda',
                 trControl=trainControl(method='none'))
  #ldaFit
  #ldaFit$finalModel
  
  # prediction and accuracy
  lda_pred=predict(ldaFit,test[,-1],type='prob')[,2]
  mean(predict(ldaFit,test[,-1])==test$class)
  
  #calculating AUC
  roc_ldaFit=  prediction(lda_pred, test$class)
  aucLDA= performance(roc_ldaFit, measure='auc')
  aucLDA=aucLDA@y.values[[1]]
  aucLDA
  
  vecLDA = c(vecLDA, aucLDA) #appending auc in each iteration
  
}

vecKNN
vecLDA

aucVals = data.frame(vecKNN, vecLDA) #dataframe of 10 AUCs for KNN and LDA
colnames(aucVals)=c('kNN', 'LDA')

summary(aucVals)

boxplot(aucVals)

###--------###


