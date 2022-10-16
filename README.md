# MSc-MachineLearning

### 1. Shiny App
The project goal was to construct an R-Shiny app to interactively explain the decision tree and random forest classifiers and the difference between tge two to an audience new to machine learning. Along with the working of the algorithm, the app covers parameter tuning and cross-validation concepts such as pruning and forest size. To contextualise the app, the famous Framingham Heart study dataset was used to predict if a person is at risk of deceloping Coronoary Heart Disease (CHD) in ten years.

##### i. Tree_shiny.pdf:
The document explains the design choices behind the various features within the app and the data preprocessing steps.
##### ii. framingham.csv:
The Frmaingham heart study dataset.
##### iii. Tree_shiny.r:
The R code file.

### 2. Classifiers&CV:
##### i. Trees-Forests.R using GermanCredit.csv data. 
      a. Split the data to a training set (70%) and a test set (30%)
      b. Fit a decision tree to the training data with an optimal tree size determined by 5-fold cross-validation. 
      c. Create a plot of the pruned tree and interpret the results. 
      d. Compute the test error rate of the pruned tree.
      e. Fit a random forest to the training data with the parameter of the number of features to create the splits tuned by 5-fold cross-validation. 
      f. Set the number of trees to 1000. 
      g. Compute the test error rate and comment on the results. 
      h .Create a plot showing variable importance for the model with the tuned parameter and comment on the plot.
      i. Draw one plot with two ROC curves for the test predictions in (a) and (e). Comment on the plot.
##### ii. SVM.R using self-simulated dataset.
       a. Simulate a three-class dataset with 50 observations in each class and two features. Make sure that this dataset is not linearly separable.
       b. Make a scatter plot to show your dataset. 
       c. Split the dataset to a training set (50%) and a test set (50%). 
       d. Train the support vector machine with a linear kernel, a polynomial kernel and an RBF kernel on the training data. 
          The parameters associated with each model should be tuned by 5-fold cross-validation. 
       e. Test the models on the test data and discuss the results.
##### iii. kNN-LDA.R using thyroid.txt data
        a. This data contain measurements for normal patients and those with hyperthyroidism. The first variable class=n if a patient is normal and class=h
           if a patients suffers from hyperthyroidism. The rest variables feature1 to feature5 are some medical test measurements.
        b. Apply kNN and LDA to classify the newthyroid.txt data: randomly split the data to a training set (70%) and a test set (30%) and repeat the
           random split 10 times.
        c. For kNN, use 5-fold cross-validation to choose k from (3, 5, 7, 9, 11, 13, 15). Use AUC as the metric to choose k, i.e. choose k with the
           largest AUC.
        d. Record the 10 test AUC values of kNN and LDA in two vectors.
        e. Draw two boxplots in one plot based on the 10 AUC values of kNN and LDA.
##### iv. manual_Crossvalidation.ipynb - a user-defined function to provide the training indexes for K-fold cross-validation with the following requirements:
        a. Input: the label vector, K, the seed or random state.
        b. Output: the training indexes for each iteration.
        c. Make sure that the class labels in each fold follow the original distribution. For example, if the numbers of instances in three classes are
          (100,50,50), then in each fold the ratio of the numbers of instances in the three classes should be roughly 2 : 1 : 1.
        d. Cannot use functions such as RepeatedKFold, createFolds, etc. need to write own function.
        e. Use this function to produce the training indexes for 10-fold cross-validation on the Ger- manCredit data.
##### v. Analysis.pdf - refer to this file for the interpetation and in-depth comments on the above classifiers.

### 3. PCAclustering
The project goal was to data on customer personality and use it to derive insights into their purchasing behaviour. Unsupervised methods such as PCA and clustering were used to profile customers based on their demographic and behavioural features to then understand which products can be pushed to which segment.
##### i. customer-personality.pdf:
explains the exploratory data analysis, the rationale behind each step taken to reach the conclusion, and the final analysis itself.
##### ii. customer-personality.csv:
the dataset with customer demographics and purchase activity from last 2 years. 
##### iii. customer-personality.R:
the code file.

### 4. customer_churn

Training and comparing different classifiers (ML and DL) to predict if a bank customer is likely to churn based on their banking behaviour, demographics, and creditworthiness. Creating a contrast between different metrics such as Accuracy, Specificity, Sensitivity etc to decide which might be the most appropriate for the given dataset.

##### 1. churn.csv: The labelled dataset of bank customers
##### 2. churn_classifiers.ipynb: Jupyter Notebook with detailed analysis of the dataset and comparison between classifier performances. 



      
