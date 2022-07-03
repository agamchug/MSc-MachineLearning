customer=read.csv('customer-personality.csv',stringsAsFactors = TRUE)

customer$ID=NULL
str(customer)
summary(customer)
customer$Dt_Customer=as.Date(customer$Dt_Customer, "%d-%m-%Y")
table(is.na(customer))
which(is.na(customer))
customer[which(is.na(customer)),]
customer=na.omit(customer)

#Observing univariate distributions of the features
par(mfrow=c(4,5),cex=0.5, mai=c(0.4,0.4,0,0))
for (i in 1:19)
  {if (is.numeric(customer[,i])==TRUE)
    {
      hist(customer[,i], xlab=colnames(customer)[i], main=' ')
    }
}

# Observing distributions of the categorical features 
par(mfrow=c(2,1),mai=c(1,1,1,1)) 
for (i in c(2,3))
{
  plot(customer[,i], xlab=colnames(customer)[i], main=' ')
}

# Data cleaning -----------------------------------------------

# The feature "Income" has an outlier of 666666. We can remove that line item
customer = subset(customer, Income!= 666666)

# The feature "Marital Cycle" has an outlier of "Alone" which we combine with "Single"
# The feature "Marital Cycle" has an outlier of "Absurd" and "YOLO" which we combine into a new feature "Other"
levels(customer$Marital_Status)=c(levels(customer$Marital_Status),'Other')
customer$Marital_Status[customer$Marital_Status=="Absurd"]<-"Other"
customer$Marital_Status[customer$Marital_Status=="YOLO"]<-"Other"
customer$Marital_Status=droplevels(customer$Marital_Status)
str(customer$Marital_Status)

data = data.frame(customer)

# Creating duplicate dataframe for linear regression
cust <- data.frame(data)


N=nrow(customer)

# Calculating how long a customer has been our client
today = "3-4-2022"
today=as.Date(today, "%d-%m-%Y")
days = rep(NA,nrow(customer))
for (p in 1:nrow(customer)){
  days[p]= today-customer$Dt_Customer[p]
}
customer$timeAsCustomer <- days

# current age of the customer
currentDate <- Sys.Date()
for ( i in 1:N){
  current_year= as.numeric(format(currentDate,"%Y"))
  customer$customer_age[i] = current_year - customer$Year_Birth[i]
}

# removing age outliers
customer_mix= subset(customer, select = -c(Year_Birth,Dt_Customer))
customer_mix= subset(customer_mix,customer_age<100)

#creating dataset for PCA mix; without any dummy variable
customer_mix= subset(customer, select = -c(Year_Birth,Dt_Customer))

# creating dummies for categorical
library(fastDummies)
customer=dummy_columns(customer,remove_first_dummy = TRUE)
str(customer)

#creating dataset for normal PCA
customer_pca= subset(customer, select = -c(Dt_Customer, Year_Birth, Education, Marital_Status))
customer_pca=subset(customer_pca,customer_age<100)


##### data exploration --------------------------------------------
#correlation heatmap
library(reshape)
library(ggplot2)
#without categorical variables, year_birth, Dt_Customer
ggheatmap = ggplot(melt(cor(customer[,-c(1,2,3,7,17)])), aes(X1, X2, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())
ggheatmap

#Income has strong correlation with NumTotalPurchases and TotalAmtSpent
#Kids has strong correlation with NumDealsPurchases and NumWebVisitsMonth


##### PCA -----------------------------------------------------
# apply PCA
pr.out = prcomp(customer_pca, scale = TRUE)
names(pr.out)

pr.out$center
pr.out$scale
pr.out$rotation

dim(pr.out$x)

# check the variance explained by each PC
pr.out$sdev
pr.var = pr.out$sdev ^2
pr.var

# proportion of variance explained
pve = pr.var/sum(pr.var)
pve
plot(pve, xlab = " Principal Component", ylab = "Proportion of
Variance Explained", ylim = c(0,1), type = "b")

plot(cumsum(pve), xlab = "Principal Component", ylab ="
Cumulative Proportion of Variance Explained", ylim = c(0,1),
     type = "b")

##### PCA with mix data ####
library(PCAmixdata)

split=splitmix(customer_mix)
X1 = split$X.quanti
X2 = split$X.quali
res.pcamix = PCAmix(X.quanti = X1, X.quali = X2, rename.level = TRUE, graph = FALSE)
res.pcamix$eig

# visualizing proportion of variance explained 
par(mfrow=c(1,2))
plot(res.pcamix$eig[,2]/100, xlab = " Principal Component", ylab = "Proportion of
Variance Explained", ylim = c(0,1), type = "b")

# plotting the cumulative variance graph
plot(res.pcamix$eig[,3]/100, xlab = "Principal Component", ylab ="
Cumulative Proportion of Variance Explained", ylim = c(0,1),
     type = "b")


##### PCA with education variable ordinally encoded, without marital status ####

#Encoding the education levels with labels from 1 to 5, ranking 'PhD' as the 
#highest level of education and '2n Cycle' as the lowest

# Dropping the Marital status column from the dataset because from the heatmap it is evident that it has very little correlation with all the other variables. 
#It can be said that Marital status data is adding noise to the dataset because of which the proportion of variance explained by PC1 decreases. 
#When we run the PCA without the Marital Status column, it increases the variance explained by PC1 to 30%

customer_mix$Education = as.numeric(customer_mix$Education) 
customer_ordinal = subset(customer_mix, select=-(Marital_Status))

customer_ordinal=rescaler(x=customer_ordinal, type='range')
split=splitmix(customer_ordinal)
X1 = split$X.quanti
X2 = split$X.quali
res.pcamix1 = PCAmix(X.quanti = X1, X.quali = X2, rename.level = TRUE, graph = FALSE)
res.pcamix1

# the proportion of variance explained for PC1 is 30% and further decreases drastically. 
# When only 5 out of 21 PCs are considered, the total variance explained comes up to 57%
# Further increasing the number of PCs to maximise the variance explained does not provide any new information that the 5 Pcs dont.


#plotting 
par(mfrow=c(1,2))
plot(res.pcamix1$eig[,2]/100, xlab = " Principal Component", ylab = "Proportion of
Variance Explained", ylim = c(0,1), type = "b")

plot(res.pcamix1$eig[,3]/100, xlab = "Principal Component", ylab ="
Cumulative Proportion of Variance Explained", ylim = c(0,1),
     type = "b")

#### PCA with education variable ordinally encoded, with marital status ####
customer_mix=rescaler(x=customer_mix, type='range')

split=splitmix(customer_mix)
X1 = split$X.quanti
X2 = split$X.quali
res.pcamix2 = PCAmix(X.quanti = X1, X.quali = X2, rename.level = TRUE, graph = FALSE)
res.pcamix2$eig

#plotting 
par(mfrow=c(1,2))
plot(res.pcamix2$eig[,2]/100, xlab = " Principal Component", ylab = "Proportion of
Variance Explained", ylim = c(0,1), type = "b")

plot(res.pcamix2$eig[,3]/100, xlab = "Principal Component", ylab ="
Cumulative Proportion of Variance Explained", ylim = c(0,1),
     type = "b")


#saving the loadings for the first 5 Pcs
#moving forward with Education ordinally encoded and without marital status
loadings = as.data.frame(round(res.pcamix1$sqload, 4)) 

#checking which features are dominating which dimensions 
for (i in 1:nrow(loadings)){
  for (column in 1:ncol(loadings)){
    if(loadings[i,column]>=0.3){
      print(paste(rownames(loadings)[i],colnames(loadings)[column],loadings[i,column]))
    }
  }
}

par(mfrow=c(1,1),mai=c(1,1,1,1))
plot(res.pcamix2, choice = "ind", coloring.ind = X2$Marital_Status, label = FALSE,
     posleg = "bottomright", main ="(a) Observations")
plot(res.pcamix2, choice = "levels", xlim = c(-1.5,2.5), main = "(b) Levels")
plot(res.pcamix2, choice = "cor", main = "(c) Numerical variables")
plot(res.pcamix2, choice = "sqload", coloring.var = T, leg = TRUE,
     posleg = "topright", main = "(d) All variables")


# Only consider those components of each PC that is has a loading greater than or equal to 0.3

for (i in 1:nrow(loadings)){
  for (column in 1:ncol(loadings)){
    if(loadings[i,column]>=0.25){
      print(paste(rownames(loadings)[i],colnames(loadings)[column]))
    }
  }
}

##### Clustering with PCA ----------------------------------------------

library(cluster)
library(factoextra)
#install.packages('factoextra')
loadings_df = as.data.frame(loadings)

# saving the individual scores of each observation for the PCs as a dataframe
clustering_df =  as.data.frame(res.pcamix1$ind$coord)
clustering_df =  as.data.frame(res.pcamix1$ind$coord)


# Finding the optimal value of k
fviz_nbclust(clustering_df, kmeans, method = 'wss') #within sum of square method
fviz_nbclust(clustering_df, kmeans, method = 'gap_stat') #gap statistics methos

## Since there is no significant decrease in the total within sum of square, k=3 
#is chosen. Further to be more precise, gap statistic is also maximum at k=3. 
#Thus, we choose 3 clusters

k = 3
kmeans_pca = kmeans(clustering_df, centers = k, nstart = 50)
clusters = as.data.frame(kmeans_pca$cluster)

#profiling
cluster_1= res.pcamix2$ind$coord[which(clusters==1),]
cluster_2= res.pcamix2$ind$coord[which(clusters==2),]
cluster_3= res.pcamix2$ind$coord[which(clusters==3),]
cluster_1

nrow(cluster_2)
summary(cluster_3)
customers_5PC= as.data.frame(res.pcamix2$ind$coord)
customers_5PC$Cluster=as.factor(kmeans_pca$cluster)
customer_ordinal$Cluster= kmeans_pca$cluster

#visualising
library(viridis)
melted = melt(customers_5PC, id.vars='Cluster')
melted$Cluster=as.factor(melted$Cluster)
snakeplot = ggplot(data=melted, aes(x=variable, y=value, group=Cluster, color=Cluster))+
  geom_line(size=1.5) +
  theme_minimal()+
  scale_color_viridis(discrete = TRUE)
snakeplot


clusterMeans=as.data.frame(rbind(colMeans(cluster_1),colMeans(cluster_2),colMeans(cluster_3)))
clusterMeans$Cluster=as.factor(c(1:3))
colnames(clusterMeans)=c('PC1','PC2','PC3','PC4','PC5', 'Cluster')
clusterMap= ggplot(melt(clusterMeans,id.vars='Cluster'), aes(variable, Cluster, fill = value))+
  geom_tile(color = "white") + theme_minimal()+
  theme(axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15))+
  xlab(' ')

clusterMap



clusterScatter= ggplot(data=customers_5PC, aes(x=`dim 1`, y=`dim 2`, group=Cluster, color=Cluster))+
  geom_point()+
  scale_color_viridis(discrete = TRUE)+
  theme_minimal()+
  xlab("Income and purchase activity")+
  ylab('Home-bound and deal preference')+
  scale_x_continuous(breaks=c(-5,5),labels=c("-5" = "-", "5" = "+"))+
  scale_y_continuous(breaks=c(-3,5),labels=c("-3" = "-", "5" = "+"))+
  theme(axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.title=element_text(size=15))
clusterScatter



#Observing the means of the main dataset but grouped by clusters
mainCluster_1= customer_ordinal[which(clusters==1),]
mainCluster_2= customer_ordinal[which(clusters==2),]
mainCluster_3= customer_ordinal[which(clusters==3),]
mainClusterMeans=as.data.frame(rbind(colMeans(mainCluster_1),colMeans(mainCluster_2),colMeans(mainCluster_3)))
print(paste(nrow(mainCluster_1), nrow(mainCluster_2), nrow(mainCluster_3)))