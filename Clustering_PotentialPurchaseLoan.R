

#======================================================
#     PotentialPurchaseLoan - Clustering
#======================================================
#Set the seed to constant value
seed=1000
set.seed(seed)

#All the numerical variables are scaled before applying #clustering and seed is set to a constant. So, all the #variables except the last 5 variables are scaled for this #dataset.

ScaledTheraData = scale(TheraData[,c(-9,-10,-11,-12,-13)])
View(ScaledTheraData)


# finding the number of clusters from Silhouette
library(factoextra)
library(dendextend)
fviz_nbclust(ScaledTheraData,kmeans,method = "silhouette",k.max=25)+theme_minimal()+ggtitle("Silhouette Plot")
#number of clusters from Silhouette = 3


#K means
KMeansclust = kmeans(x=ScaledTheraData, centers = 3, nstart = 5)
print(KMeansclust)
library(cluster)
clusplot(ScaledTheraData, KMeansclust$cluster, color=TRUE, shade=TRUE, labels=2, lines=1)

#Customer Segmentation
#New column is added to dataset to assign every row n the #dataset to one of the three clusters.

TheraData$Clusters = KMeansclust$cluster
print(TheraData)
print(KMeansclust$cluster)

#Aggregate function is used to segment the customers to three #groups:

custProfileKMeans = aggregate(TheraData[,-c(9,10,11,12,13)],list(TheraData$Clusters),FUN="mean")
View(custProfileKMeans)
View(TheraData)

#Observation of the clusters:
#Cluster 1 belongs to customers having high - income, money #spent on credit card, Mortgage and in age group of 43 years #and experience, family members in between the other two #groups.
#Cluster 2 are aged customers having more experience and #education with low income and mortgage ,money spent on #credit card
#Cluster 3 belongs to customers in age group of 35 years with #income, mortgage, and education in between other two groups #and have more family members.


#Dendogram
#Cluster and factoextra libraries are used to build #dendograms. Distance matrix is first calculated with #Euclidean method. Hclust method is used to build the cluster #and fviz_dend is used to visualize the Dendogram.

library(cluster)
TheraDistance <- dist(ScaledTheraData, method = "euclidean")
TheraHclust <- hclust(TheraDistance, method = "ward.D2")
library(factoextra) 
fviz_dend(TheraHclust, cex = 0.5)
fviz_dend(TheraHclust, cex = 0.5, main = "Dendrogram - ward.D2", xlab = "Objects", ylab = "Distance", sub = "")
