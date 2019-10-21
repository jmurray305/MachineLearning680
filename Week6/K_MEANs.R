rm(list = ls()) 
graphics.off()
cat("\014") 

# Describe how K-means works.
# Perform K-means using R, including interpret and conclude the results.
# Describe how HCA works.
# Describe different methods for merging clusters.
# Explain what is dendogram.
# Illustrate the result of cutting the height of dendrogram, and explain its meaning.
# Perform HCA using R,  draw the dendrogram, interpret and summarize the results.

library(cluster)
library(factoextra)
library(gridExtra)

# source("C:\\Users\\pandr\\OneDrive\\R_code\\installPackages.R")
setwd('/Users/justinmurray/Desktop/ML680/Week6/data')
water = read.csv('water-treatment.data', header = FALSE, na.strings = TRUE)
names(water) = c("Q-E", "ZN-E", "PH-E", "DBO-E", "DQO-E", "SS-E", "SSV-E", "SED-E", "COND-E", 
          "PH-P", "DBO-P", "SS-P", "SSV-P", "SED-P", "COND-P", 
          "PD-D", "DBO-D", "DQO-D", "SS-D", "SSV-D","SED-D", "COND-D", 
          "PH-S", "DBO-S", "DQO-S", "SS-S", "SSV-S", "SED-S", "COND-S", 
          "RD-DBO-P", "RD-SS-P", "RD-SED-P", "RD-DBO-S", "RD-DQO-S", 
          "RD-DBO-G", "RD-DBO-G", "RD-QO-G", "RD-SS-G", "RD-SED-G")

# Preprocess / cleaning the data
# replacing ?'s with NA's then droping the NA's. We keep about 70% of the data
water = lapply(water, as.numeric)
idx = water == "?"
is.na(water) = idx
na_sum = sapply(water, function(x) sum(is.na(x)))
water = na.omit(water)

# Normalize the data to the same sclae
water = sapply(water, scale)

# K-means aslo known as flat clustering. It does not create a hierarchly of clusters like hierarchical clustering,
# it requires  the number of clusters as an input. K-means does perform faster the nhierarchical clustering. K-means
# clustering uses partitioning clustering with the goal is to partition the number of objects into the number of given 
# clusters with the nearest mean by minimizing the within-cluster sum of squares. The process of k-means can be broken
# into 5 steps
# 1. Specify the number of clusters, k
# 2. Randomly create k partitions
# 3. Calculate the center of the partitions
# 4. Associate objects closest to the wcluster center
# 5. Repeat steps 2,3,4 until the WCSS changes very little
# K-means requires the user to input a number of clusters to

set.seed(23)
fit = kmeans(water, 4)
# Inspect the center of each cluster
barplot(t(fit$centers), beside = TRUE, xlab="cluster", ylab="value")

#Draw a scatter plot of the data and color the points according to the clusters
plot(water, col = fit$cluster)
k2 <- kmeans(water, centers = 2, nstart = 25)
k4 <- kmeans(water, centers = 4, nstart = 25)
k5 <- kmeans(water, centers = 5, nstart = 25)
k6 <- kmeans(water, centers = 6, nstart = 25)
p1 = fviz_cluster(k2, geom = "point", data = water) + ggtitle("K = 2")
p2 = fviz_cluster(k4, geom = "point", data = water) + ggtitle("K = 4")
p3 = fviz_cluster(k5, geom = "point", data = water) + ggtitle("K = 5")
p4 = fviz_cluster(k6, geom = "point", data = water) + ggtitle("K = 6")
grid.arrange(p1, p2, p3, p4, nrow = 2)

# obtaining the optimum number of clusters
nk = 2:10
fviz_nbclust(water, kmeans, method = "wss")
fviz_nbclust(water, kmeans, method = "silhouette")

#Best K would be 4 then followed by 5 and 6 clusters and  2 clusters clusters exist with no overlap.

