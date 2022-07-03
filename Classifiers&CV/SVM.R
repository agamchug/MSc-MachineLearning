set.seed(11)
calories = c(rnorm(50,28,10), rnorm(50,50,10),rnorm(50,45,10))
protein = c(rnorm(50,43,12), rnorm(50,40,10),rnorm(50,32,10)) #rnorm(150, 22, 5)
meatType = (factor(rep(c('pork', 'chicken', 'beef'), each=50)))
meat = data.frame(calories,protein,meatType)
summary(meat)

#creating a scatterplot to visualise if the classes are linearly separable.
library(ggplot2)
scatter = ggplot(data=meat, aes(x=calories,y=protein, shape=meatType, color=meatType))+
  geom_point(size=3)+
  theme_classic()+
  scale_color_brewer(palette='Dark2')+
  labs(x='Calories(kCal)', y='Protein(grams)')

scatter

# create training and test sets (50/50)
library(caret)
set.seed(12)
trainIndex = createDataPartition(meat$meatType, p = 0.5, list = FALSE, times = 1) 
train.feature = meat[trainIndex,-3] # training features i.e. X's (-3 to remove the class column)
train.label = meat$meatType[trainIndex] # training labels i.e y (the class column)
test.feature= meat[-trainIndex,-3] # test features i.e. X's (-10 to remove the class column)
test.label = meat$meatType[-trainIndex] # test labels i.e y (the class column)
#Combining into individual training and test dataframes
train = train.feature
train$Class = train.label
test = test.feature
test$Class = test.label

### SVM: Linear Kernel (svmLinear) ###

fitControl.L=trainControl(
  method = "cv",
  number = 5)


grid_linear = expand.grid(C = c(0.25,0.5,1,2,4,8,16,32,64,128))

set.seed(7)
svm.Linear=train(Class ~., data = train, method = "svmLinear",
                 trControl=fitControl.L,
                 preProcess = c("center", "scale"),
                 tuneGrid = grid_linear)
svm.Linear
svm.Linear$finalModel
plot(svm.Linear)

### SVM: Polynomial Kernel (svmPoly)###

fitControl.P=trainControl(
  method = "cv",
  number = 5)

grid_poly = expand.grid(C = c(0.25,0.5,1,2,4,8,16,32,64,128), 
                        degree = c(2,3,4,5), scale=seq(0.01, 0.1, length = 5))

set.seed(77)
svm.Poly=train(Class ~., data = train, method = "svmPoly",
                 trControl=fitControl.P,
                 preProcess = c("center", "scale"),
                 tuneGrid=grid_poly)
svm.Poly
svm.Poly$finalModel
plot(svm.Poly)

### SVM: RBF Kernel (svmRadial) ###

fitControl.R=trainControl(
  method = "cv",
  number = 5)
grid_radial=expand.grid(sigma = seq(0.001,0.01, length = 10),
                        C = c(0.25,0.5,1,2,4,8,16,32,64,128))

set.seed(777)
svm.Radial=train(Class ~., data = train, method = "svmRadial",
                  trControl=fitControl.R,
                  preProcess = c("center", "scale"),
                  tuneGrid = grid_radial)
svm.Radial
svm.Radial$finalModel
plot(svm.Radial)
### Testing the models: ###

##Linear
pred.Linear = predict(svm.Linear, test[,-ncol(test)])
1-mean(pred.Linear==test[,ncol(test)]) 
mean(pred.Linear==test[,ncol(test)])

#confusion matrix
table(test$Class, pred.Linear)


##Polynomial
pred.Poly = predict(svm.Poly, test[,-ncol(test)])

1-mean(pred.Poly==test[,ncol(test)]) 
mean(pred.Poly==test[,ncol(test)])

#confusion matrix
table(test$Class, pred.Poly)


##Radial
pred.Radial = predict(svm.Radial, test[,-ncol(test)])

1-mean(pred.Radial==test[,ncol(test)]) 
mean(pred.Radial==test[,ncol(test)])

#confusion matrix
table(test$Class, pred.Radial)

###--------###

