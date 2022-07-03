library(caret)
##Loading the dataset
framingham = read.csv('framingham.csv')
str(framingham) #TenYearCHD is the response variable. 
summary(framingham)
##remove na 
framingham=na.omit(framingham)
summary(framingham) #(the split between the classes did not change much after omitting na)

##15.23% of the observartions are classified to develop CHD in ten years.

##need to convert categorical variables into factors from integers
framingham$male=as.factor(framingham$male)
framingham$education=as.factor(framingham$education)
framingham$currentSmoker=as.factor(framingham$currentSmoker)
framingham$BPMeds=as.factor(framingham$BPMeds)
framingham$prevalentStroke=as.factor(framingham$prevalentStroke)
framingham$prevalentHyp=as.factor(framingham$prevalentHyp)
framingham$diabetes=as.factor(framingham$diabetes)
framingham$TenYearCHD=as.factor(framingham$TenYearCHD)

##stratified sampling of 500 observations based on the 15.23% split of TenYearCHD
500*0.1523
##76 observations must belong to class 1 and 124 to class 0
##in order to replicate the same distinction in our sample.
idx_1=sample(which(framingham$TenYearCHD==1),size=76) #indices of 76 random class 1 observations
idx_0=sample(which(framingham$TenYearCHD==0),size=424) #indices 0f 424 random class 0 observations
class_1=framingham[idx_1,] 
class_0=framingham[idx_0,]
framingham_sample = rbind(class_1,class_0) #combining the sampled dataset
str(framingham_sample)
summary(framingham_sample)
row.names(framingham_sample) = NULL #resetting index
##shuffling the combined observations

set.seed(224)
rands=sample(nrow(framingham_sample))  #random index values between 1 and 500
framingham_sample=framingham_sample[rands,] #dataframe with respect to the shuffled index order
row.names(framingham_sample) = NULL

#
set.seed(565) 
train.index=createDataPartition(framingham_sample[,ncol(framingham_sample)],p=0.6,list=FALSE)
train=framingham_sample[train.index,]
test=framingham_sample[-train.index,]

############################################
## MODELING ##
############################################
data = framingham_sample
summary(data)

#data split into training and testing
set.seed(565)
train.index=createDataPartition(data[,ncol(data)],p=0.6, list=FALSE)
train = data[train.index,]
test = data[-train.index,]

### DECISION TREE MODEL ###
### DECISION TREE MODEL ###
library(tree)

##training decision tree
set.seed(300)
data.tree = tree(TenYearCHD ~., train)

#summary of the training tree
summary(data.tree)  
#plot the tree
plot(data.tree)
text(data.tree, pretty=1, cex=0.6)

##testing data
pred = predict(data.tree, test[,-ncol(test)], type="class")
mean(pred==test[,ncol(test)])


#using caret library
library(caret)

fitcontrol = trainControl(method="repeatedcv",
                          number=10, repeats=3)
set.seed(1)
cpGrid=expand.grid(cp=c(0.01,0.02,0.03))
data.rparts = train(train[,-ncol(data)], train[,ncol(data)], method = "rpart",
                    tuneGrid=cpGrid,
                    trControl = fitcontrol)
data.rparts

#details
data.rparts$finalModel

#prediction using caret
pred.dt = predict(data.rparts, newdata=test[,-ncol(test)])

#fancier plot
library(rattle)
fancyRpartPlot(data.rparts$finalModel)

#saving the model to be used in Shiny
saveRDS(data.rparts$finalModel, file = "./decisionTree.rda")

#Importing model
model_dt <- readRDS(file = 'decisionTree.rda')

### RANDOM FOREST MODEL ###

fitControl1=trainControl(method='repeatedcv', number=5, repeats=3)
mtry = (ncol(train)-1)^0.5 
tunegrid <- expand.grid(.mtry=mtry)

set.seed(999) 
rfFit = train(TenYearCHD~.,data=train, method='rf', metric='Accuracy', 
              trControl=fitControl1, tuneLength=mtry)
rfFit
plot(rfFit)
best_tree = rfFit$finalModel
varImp(rfFit)
plot(varImp(rfFit))

#predictions
pred.rf=predict(rfFit,newdata=test[,-ncol(test)])

#confusion matrix
table(test$TenYearCHD, pred.rf)

plot(best_tree)

#predicted vs actual classification split  (can be compared with decision trees)
mean(as.integer(test[,ncol(test)])-1) #actual
mean(as.integer(pred.rf)-1) #predicted


#training vs test accuracy (can be compared with decision trees)
mean(pred.rf==test[,ncol(test)]) #Test accuracy
max(rfFit$results$Accuracy) #Training accuracy
#variable importance (can be compared with decision trees)
plot(varImp(rfFit))
# Accuracy change wrt number of predictors (random forests only)
plot(rfFit)

###METRICS FROM BOTH THE MODELS###

library(ggplot2)
#ACCURACIES
#RF
RF_TestAcc = round(mean(pred.rf==test[,ncol(test)]), digits=4)
RF_TrainAcc = round(max(rfFit$results$Accuracy), digits=4)
#DT
DT_TestAcc = round(mean(pred.dt==test[,ncol(test)]), digits=4)
DT_TrainAcc = round(max(data.rparts$results$Accuracy), digits=4) 

#CLASSIFICATION SPLIT
#RF:
RF_Actual1=round(mean(as.integer(test[,ncol(test)])-1),4)*100  #actual
RF_Actual0=100-RF_Actual1
RF_Pred1=round(mean(as.integer(pred.rf)-1), 4)*100 #predicted
RF_Pred0=100-RF_Pred1
#DT
DT_Actual1=round(mean(as.integer(test[,ncol(test)])-1),4)*100 #actual
DT_Actual0=100-DT_Actual1
DT_Pred1=round(mean(as.integer(pred.dt)-1),4)*100 #predicted
DT_Pred0=100-DT_Pred1

###PLOTS###
#VARIABLE IMPORTANCE PLOT
plot(varImp(rfFit))
plot(varImp(data.rparts))


#ACCURACY PLOT
accuracies = data.frame(Model=c(rep('Random Forest',2), rep('Decision Tree',2)),
                        Type = rep(c('Test','Train'),2), 
                        Accuracy = c(RF_TestAcc,RF_TrainAcc,DT_TestAcc, DT_TrainAcc))

bar_heights=c(RF_TestAcc, RF_TrainAcc, DT_TestAcc, DT_TrainAcc)

ggplot(accuracies, aes(fill=Type, y=Accuracy, x=Model)) + 
  geom_bar(position="dodge", stat="identity", width=0.8) + ylim(0,1) + 
  geom_text(aes(label=Accuracy), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5) + 
  ggtitle('Test VS Train Accuracy') + theme(plot.margin = unit(c(0.9,1.2,0.9,1.2), "cm"))

#CLASSIFICATION SPLIT PLOT of Predictions
split = data.frame(Model=c(rep('Random Forest',2), rep('Decision Tree',2)),
                   Class = rep(c('Yes','No'),2), 
                   Percentage = c(RF_Pred1,RF_Pred0,DT_Pred1,DT_Pred0))

ggplot(split, aes(fill=Class, y=Percentage, x=Model)) + 
  geom_bar(position="dodge", stat="identity", width=0.8) + ylim(0,100) + 
  geom_text(aes(label=Percentage), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5) + 
  ggtitle('Predicted percentage of Test set to have CHD in 10 years') + theme(plot.margin = unit(c(0.9,1.2,0.9,1.2), "cm"))

############################################
## R SHINY ##
############################################
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)


# Defining random forest function
randfor<- function(mtry){
  fitControl = trainControl(method="repeatedcv",
                            number=10, repeats=3)
  set.seed(999)
  rfFit = train(TenYearCHD~.,data=train, method='rf', metric='Accuracy', 
                trControl=fitControl, tuneLength=mtry)
  rfFit
  plot(rfFit)
}

#R Shiny ui
ui <- dashboardPage(
  #Dashboard title
  dashboardHeader(title = 'ARE YOU AT RISK FOR CHD?', titleWidth = 340),
  
  #Sidebar layout
  dashboardSidebar(width = 290,
                   sidebarMenu(menuItem("Home", tabName = 'home', icon = icon('home')),
                               menuItem("Prediction", tabName = 'pred', icon = icon('sync')),
                               menuItem("Decision Tree Visualisation", tabName = 'dtvis', icon = icon('poll')),
                               menuItem("Random Forest", tabName = "plots", icon = icon('poll')),
                               menuItem("Comparison Dashboard", tabName = 'comparison', icon = icon('search')))),
  
  #Tabs layout
  dashboardBody(
    tags$head(tags$style(HTML('.skin-blue .main-header .logo { background-color: #DC143C;font-weight: bold;}
                               .skin-blue .main-header .navbar {background-color: #000000;}
                               .skin-blue .main-sidebar .sidebar{color: #black;}'))),
   
                #Prediction tab content
                tabItems(tabItem('home', 
                                 #Boxes to display the details
                                 box(title = 'INTRODUCTION', status = 'danger', width = 20,
                                     helpText("This app aims to interactively familiarise the user about the working of two classification algorithms in machine learning: Decision Trees and Random Forests; by putting a real dataset in action."),
                                     helpText("The dataset has been chosen from the Framingham Heart Study’s important 1998 paper.  The goal is to predict 10-year risk of developing Coronary Heart Disease (CHD) which is a disease of the blood vessels supplying the heart (https://biolincc.nhlbi.nih.gov/home/)."),
                                     helpText("Heart diseases have been the leading cause of death worldwide since 1921. In order to accurately predict the risk of the disease, important risk factors must be identified, and eventually appropriate medical interventions can be developed based on these factors. Such a study can also elucidate some behaviours or traits that might be linked with a certain disease but not widely known.")),
                                 box(title = 'LEXICON', status = 'danger', width = 20,
                                     helpText("Behavioural risk factors:"),
                                     helpText("currentSmoker: If currently a smoker (1=Yes,0=No)"),
                                     helpText("cigsPerDay: Average number of cigarettes smoked in a day (integer)"),
                                     helpText("Medical history risk factors:"),
                                     helpText("BPmeds: On blood pressure medications at first examination (1=Yes,0=No)"),
                                     helpText("prevalentStroke: Previously had a stroke (1=Yes,0=No)"),
                                     helpText("prevalentHyp: Currently hypertensive (1=Yes,0=No)"),
                                     helpText("diabetes: Currently diabetic (1=Yes,0=No)"),
                                     helpText("Demographic risk factors:"),
                                     helpText("male: Sex (1=male,0=female)"),
                                     helpText("age: In years at first examination (integer)"),
                                     helpText("education: Level of education (1=some high school, 2=high school, 3=some college, 4=college)"),
                                     helpText("Risk factors from physical examination of the patient:"),
                                     helpText("totChol: Total cholesterol (mg/dL)"),
                                     helpText("sysBP: Systolic blood pressure"),
                                     helpText("diaBP: Diastolic blood pressure"),
                                     helpText("BMI: Body Mass Index (weight(kg)/height(m)^2)"),
                                     helpText("heartRate: Heart rate (beats/minute)"),
                                     helpText("glucose: Blood sugar level (mg/dL)")),
                                 box(title = 'MODELS USED', status = 'danger', width = 20,
                                     helpText("Decision Tree:"),
                                     helpText("Supervised machine learning algorithm that can be used to solve classification problems. It builds a set of rules to predict an outcome based on the given features. The dataset gets split until all data points can be assigned to a class. The variables that are strongly correlated with the outcome are used first to organise the split. The algorithm chooses the variables based on how accurately that variable alone predicts the class and consequently further splits are used to validate the results. Often to decide the best number of splits, an alpha parameter is used to penalise the higher number of splits. Higher the number of splits, higher the accuracy but that leads to overfitting. Conversely, lower number of splits lead to lower accuracy and underfitting. Therefore, a trade-off must be made and that is best chosen using Cross Validation of the alpha parameter."),
                                     helpText("Random Forest:"),
                                     helpText("Random Forest is an extension of Decision Trees in the sense that multiple decision trees are used to create a class prediction. The outcome that majority of the trees in the forest predict is chosen as the actual outcome of the forest. All decision trees within the forest are distinct and made using a subset of randomly bootstrapped observations in the dataset and randomly chosen subset of features of the dataset. The subset of features can be limited to a specific number and all the trees will be made using those many features, but the subsets may not be the same for all trees. Again, the optimal number of features to limit within a tree can be chosen using Cross Validation. The integer specified in the mtry value tries that many different values of features and the model returns the tried integer with the highest accuracy."))),
                         tabItem('dtvis', 
                                        #Decision Tree
                                        #Boxes to display the plots
                                        box(title = 'DECISION TREE VISUALISATION', status = 'danger', width = 20, height= 800,
                                            plotOutput('decTree'),
                                            helpText("6 Key Takeaways of this Algorithm:", style = "font-size:18px; color: #0B0B0B"),
                                            helpText("     ~ Easy to interpret (non-parametric model), powerful, versatile."),
                                            helpText("     ~ Effective learning with small training data, however very sensitive to small variations in the training data."),
                                            helpText("     ~ Doesn't require feature scaling."),
                                            helpText("     ~ Very few assumptions about the training data( the data is non-linear)."),
                                            helpText("     ~ Very likely to overfit the training data(not generalizing well)."))),
                         tabItem('pred',
                                 #Filters for categorical variables
                                 box(title = 'CATEGORICAL VARIABLES', status = 'danger', width = 20,
                                     splitLayout(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                                 cellWidths = c('0%', '19%', '4%', '19%', '4%', '19%', '4%', '19%', '4%', '8%'),
                                                 radioButtons('diab', 'Do you have diabetes?', c("1", "0")),
                                                 div(),
                                                 radioButtons('sex', 'What is your Biological Gender?', c("1", "0")),
                                                 div(),
                                                 radioButtons('bp', 'Do you consume medication for BP?', c("1", "0")))),
                                 box(status = 'danger', width = 20,
                                     splitLayout(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                                 cellWidths = c('0%', '19%', '4%', '19%', '4%', '19%', '4%', '19%', '4%', '8%'),
                                                 radioButtons('stroke', 'Have you suffered from a stroke?', c("1", "0")),
                                                 div(),
                                                 radioButtons('hypten', 'Do you suffer from hypertension?', c("1", "0")),
                                                 div(),
                                                 radioButtons('smoker', 'Do you smoke?', c("1", "0")),
                                                 div(),
                                                 selectInput('education', 'Education Level', c('1', '2', '3', "4")))), # Assuming that 1 is no formal education while 4 is Master's level education
                                 #Filters for numeric variables
                                 box(title = 'NUMERICAL VARIABLES', status = 'danger', width = 20,
                                     splitLayout(cellWidths = c('22%', '4%','21%', '4%', '21%', '4%', '21%'),
                                                 numericInput('age', 'Age*', min = 0, max = 120,  0),
                                                 div(),
                                                 numericInput('sysBp', 'Systolic BP', min = 0, max = 300, 0),
                                                 div(),
                                                 numericInput('diasBp', 'Diastolic BP', min = 0, max = 300, 0),
                                                 div(),
                                                 numericInput('chol', 'Cholesterol', min = 0, max = 500, 0))),
                                 
                                 #Filters for numeric variables
                                 box(status = 'danger', width = 20,
                                     splitLayout(cellWidths = c('22%', '4%','21%', '4%', '21%', '4%', '21%'),
                                                 numericInput('bmi', 'BMI', min = 0, 0),
                                                 div(),
                                                 numericInput('hr', 'Resting Heart Rate', min = 0, 0),
                                                 div(),
                                                 numericInput('glu', 'Glucose', min = 0, 0),
                                                 div(),
                                                 numericInput('cigs', 'Cigarettes per Day', min = 0,  0))),
                                 #Box to display the prediction results
                                 box(title = 'PREDICTION RESULT', status = 'danger', solidHeader = TRUE, width = 4, height = 230,
                                     div(h5('Predicted Probability of Heart Disease (1=Yes, 0=No)')),
                                     verbatimTextOutput("value", placeholder = TRUE),
                                     actionButton('cal','Calculate', icon = icon('calculator'))),
                                 #Box to display information about the model
                                 box(title = 'MODEL EXPLANATION', status = 'danger', width = 8, height = 230,
                                     helpText('The following model will predict whether you are at risk of developing Coronary Heart Disease 10 years from today, based on the given inputs. The prediction is based on decision tree supervised learning model (more information on Home page).'),
                                     helpText('FURTHER INFORMATION', style = "font-size:18px; color: #0B0B0B"  ),   
                                     helpText('To learn more on how to prevent Coronary Heart Disease (CHD),', tags$a(href="https://www.nhs.uk/conditions/coronary-heart-disease/prevention/", "Click here!"))),
                                 #helpText(sprintf('The accuracy rate of the model is', DT_TestAcc)))
                                 #helpText(sprintf('The prediction is based on a random forest supervised machine learning model. Furthermore, the models deliver a mean absolute error (MAE) of %s total number of registrations, and a root mean squared error (RMSE) of %s total number of registrations.', round(mae_rf, digits = 0), round(rmse_rf, digits = 0)))))
                         ),
                         tabItem('plots', 
                                 fluidRow( 
                                   box(title = 'Random Forest and Cross-Validation (CV)', status = 'danger', width = 12, height = 250,
                                       helpText('Here, the random forest is not visualised because as mentioned previously, it is made up of many different decision trees (500 in this case) and visualising those many trees is not computationally feasible. However, one can clearly see how choosing different mtry values can lead to different levels of accuracy due to different numbers of features limited per tree. Essentially, the final model is built by calculating the accuracy of all combinations of the feature limits set to each tree. For example, if the bar is set to 4, 4 different random forests will be built with 4 different feature limits per tree. Then, the average accuracy of the 4 forests is on the y-axis. This is how Cross Validation is done – by tuning the mtry parameter and trying different numbers of features to split on to choose the one with the best accuracy.'),
                                   )),
                                 
                                 fluidRow(
                                   box(
                                     "The 'mtry' values can be changed to see how the accuracy changes with the number of forests used for CV", br(),
                                     sliderInput(inputId = "mtry",
                                                 label = "'mtry' values:",
                                                 min = 2,
                                                 max = 16,
                                                 value = 2,
                                                 step=2), width=6),
                                   box(status='danger', plotOutput('linePlot'), width=6), height= 250),
                                 
                         ),
                         
                         tabItem('comparison',
                                 
                                 fluidRow(
                                   box(title = ' Comparison of Variable Importance Between The Two Models ', status = 'danger', width = 12, height = 200,
                                       helpText('Here, the features from both the models that had the highest influence in classification have been ranked. The ranks are based on how purely/accurately the splits are created by that risk factor or feature. The medical interventions can be directed towards controlling the top factors. Note that both the models may not align on the same ranks.')),
                                 ),
                                 
                                 fluidRow(
                                   box(title= 'Importance of Variables in Decision Trees', status= 'danger', plotOutput('VarImpPlot1'),width=6,height= 500),
                                   box(title= 'Importance of Variables in Random Forest',status='danger', plotOutput('VarImpPlot2'), width=6, height= 500)),
                                 
                                 fluidRow(
                                   #
                                   
                                   box(title = 'How Accuracy Compares Between The Two Models  ', status = 'danger', width = 6, height = 200,
                                       helpText('Here, the test and training set accuracy of both the models is visualised. Random Forest seems to be more accurate. The accuracy is the ratio of correctly classified observation.')),
                                   box(title = 'Comparison of Percentage Prediction of The Test Data for The Two Models ', status = 'danger', width = 6, height = 200,
                                       helpText('Percentage of test observations predicted to have and not to have CHD in ten years using Decision Tree and Random Forest. The percentage split is broadly similar. In the actual test set, the split is of 85% and 15%. Clearly, both the models are predicting more values as No than there actually are')
                                   )
                                 ),
                                 
                                 fluidRow(
                                   box(title= 'Comparing Accuracy of Models', status='danger', plotOutput('AccuracyPlot'), width=6, height= 500),
                                   box(title = 'Percentage of Test Observations Predicted',status='danger', plotOutput('PredPerPlot'), width=6, height= 500)
                                 )
                )
  )
)
)


# R Shiny server

server <- function(input, output){
  
  # Analysis
  output$decTree <- renderPlot({
    #Decision Tree
    fancyRpartPlot(model = data.rparts$finalModel)
    
  })
  
  #Prediction model
  #React value when using the action button
  a <- reactiveValues()
  
  observeEvent(input$cal, {
    #Copy of the test data without the dependent variable
    test_pred <- test[-16]
    
    #Dataframe for the single prediction
    values = data.frame(male = input$sex,
                        age = input$age,
                        education = input$education,
                        currentSmoker = input$smoker,
                        cigsPerDay = input$cigs,
                        BPMeds = input$bp,
                        prevalentStroke = input$stroke,
                        prevalentHyp = input$hypten,
                        diabetes = input$diab, 
                        totChol = input$chol,
                        sysBP = input$sysBp, 
                        diaBP = input$diasBp,
                        BMI = input$bmi,
                        heartRate = input$hr,
                        glucose = input$glu)
    
    #Include the values into the new data
    test_pred <- rbind(test_pred,values)
    
    #Single prediction using the decision tree model
    a$result <-  predict(data.rparts, 
                         newdata = test_pred[nrow(test_pred),])
  })
  
  output$value <- renderText({
    #Display the prediction value
    paste(a$result)
  })
  
  output$linePlot = renderPlot({
    mtry=as.numeric(input$mtry)
    randfor(input$mtry)
  })
  output$VarImpPlot1<- renderPlot({
    plot(varImp(rfFit))
  })
  output$VarImpPlot2<- renderPlot({
    plot(varImp(data.rparts))
  })
  output$AccuracyPlot<- renderPlot({
    ggplot(accuracies, aes(fill=Type, y=Accuracy, x=Model)) + 
      geom_bar(position="dodge", stat="identity", width=0.8) + ylim(0,1) + 
      geom_text(aes(label=Accuracy), vjust=1.6, color="white",
                position = position_dodge(0.9), size=3.5) + 
      ggtitle('Test VS Train Accuracy') + theme(plot.margin = unit(c(0.9,1.2,0.9,1.2), "cm"))
  })
  output$PredPerPlot<- renderPlot({
    ggplot(split, aes(fill=Class, y=Percentage, x=Model)) + 
      geom_bar(position="dodge", stat="identity", width=0.8) + ylim(0,100) + 
      geom_text(aes(label=Percentage), vjust=1.6, color="white",
                position = position_dodge(0.9), size=3.5) + 
      ggtitle('Predicted percentage of Test set to have CHD in 10 years') + theme(plot.margin = unit(c(0.9,1.2,0.9,1.2), "cm"))
    
  })
}

shinyApp(ui, server)  

