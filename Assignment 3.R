##################################################
###                                             ##
### RStudio - Assignment 3                      ## 
##################################################
#                                               ##
##################################################
# Written by Keerthy Raaj Shanmugam
# ID: 8779954
#
##################################################
### Basic Set Up                                ##
##################################################

# Clear all plots
if(!is.null(dev.list())) dev.off()

# Clear entire console
cat("\014") 

# Clean and clear theworkspace
rm(list=ls())

#Set work directory to an appropriate location
setwd("C:/Users/keert/Documents/Data")

options(scipen=9)

##################################################
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

if(!require(dplyr)){install.packages("dplyr")}
library("dplyr")

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(ggplot2)){install.packages("ggplot2")}
library("ggplot2")

#Read the PROG8430_Clst_21F data which is in CSV format
clst_data_KS <- read.csv("PROG8430_Clst_21F.csv",header=TRUE,sep=",")
head(clst_data_KS)

#summary of data
summary(clst_data_KS)

#1 Data Transformation,

#1.1 Standardize all of the variables using either of the two functions demonstrated in class. Describe why you chose the method you did.

#using standardization method 
std_fun_KS <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

clst_data_std_fun_KS <- std_fun_KS(clst_data_KS)
head(clst_data_std_fun_KS)

#2 Descriptive Data Analysis,

#2.1 Create graphical summaries of the data (as demonstrated in class: boxplots or histograms) and comment on any observations you make.
hist(clst_data_std_fun_KS$Food)
hist(clst_data_std_fun_KS$Entr)
hist(clst_data_std_fun_KS$Educ)
hist(clst_data_std_fun_KS$Tran)
hist(clst_data_std_fun_KS$Work)
hist(clst_data_std_fun_KS$Hous)
hist(clst_data_std_fun_KS$Othr)

#Fetching 2 columns - Food and Work
str(clst_data_std_fun_KS)
summary(clst_data_std_fun_KS)
clst_data_std_fun_KS <- clst_data_std_fun_KS[c(1,5)]
str(clst_data_std_fun_KS)

#3 Clustering,

#3.1 Create segmentation/cluster schemes for k=3,4,5,6,7. 
km_Clstr_3_KS <- kmeans(clst_data_std_fun_KS, iter.max=10, centers=3, nstart=10)
km_Clstr_3_KS

km_Clstr_4_KS <- kmeans(clst_data_std_fun_KS, iter.max=10, centers=4, nstart=10)
km_Clstr_4_KS

km_Clstr_5_KS <- kmeans(clst_data_std_fun_KS, iter.max=10, centers=5, nstart=10)
km_Clstr_5_KS

km_Clstr_6_KS <- kmeans(clst_data_std_fun_KS, iter.max=10, centers=6, nstart=10)
km_Clstr_6_KS

km_Clstr_7_KS <- kmeans(clst_data_std_fun_KS, iter.max=10, centers=7, nstart=10)
km_Clstr_7_KS

clst_data_KS$cluster_3 <- factor(km_Clstr_3_KS$cluster) 
clst_data_KS$cluster_4 <- factor(km_Clstr_4_KS$cluster) 
clst_data_KS$cluster_5 <- factor(km_Clstr_5_KS$cluster) 
clst_data_KS$cluster_6 <- factor(km_Clstr_6_KS$cluster) 
clst_data_KS$cluster_7 <- factor(km_Clstr_7_KS$cluster) 
head(clst_data_KS)

#4.1 Based on the "k" chosen above, create a scatter plot showing the clusters and colour-coded datapoints for each of "k-1", "k", "k+1". For example, if you think the "elbow" is at k=4 create the charts for k=3, k=4 and k=5.
#K = 5
centers <- data.frame(cluster=factor(1:5), km_Clstr_5_KS$centers)


ggplot(data=clst_data_KS, aes(x=Food, y=Work, color=cluster_5)) + geom_point()

ggplot(data=clst_data_KS, aes(x=Food, y=Work, color=cluster_5, shape=cluster_5)) + 
  geom_point(alpha=.8) +
  geom_point(data=clst_data_KS, aes(x=Food, y=Work), size=5, stroke=2)

#K-1 = 4
centers <- data.frame(cluster=factor(1:4), km_Clstr_4_KS$centers)


ggplot(data=clst_data_KS, aes(x=Food, y=Work, color=cluster_4)) + geom_point()

ggplot(data=clst_data_KS, aes(x=Food, y=Work, color=cluster_4, shape=cluster_4)) + 
  geom_point(alpha=.8) +
  geom_point(data=clst_data_KS, aes(x=Food, y=Work), size=4, stroke=2)

#K+1 = 6
centers <- data.frame(cluster=factor(1:6), km_Clstr_6_KS$centers)


ggplot(data=clst_data_KS, aes(x=Food, y=Work, color=cluster_6)) + geom_point()

ggplot(data=clst_data_KS, aes(x=Food, y=Work, color=cluster_6, shape=cluster_6)) + 
  geom_point(alpha=.8) +
  geom_point(data=clst_data_KS, aes(x=Food, y=Work), size=6, stroke=2)

#4.3 Create summary tables for the segmentation/clustering scheme (selected in step 4.2).

km_Clstr_5_KS <- kmeans(clst_data_std_fun_KS, iter.max=10, centers=5, nstart=10)
km_Clstr_5_KS

#4.4 Create suitable descriptive names for each cluster.
CLUSTER_3_KS <- kmeans(clst_data_std_fun_KS, iter.max=10, centers=3, nstart=10)
CLUSTER_3_KS

CLUSTER_4_KS <- kmeans(clst_data_std_fun_KS, iter.max=10, centers=4, nstart=10)
CLUSTER_4_KS

CLUSTER_5_KS <- kmeans(clst_data_std_fun_KS, iter.max=10, centers=5, nstart=10)
CLUSTER_5_KS

CLUSTER_6_KS <- kmeans(clst_data_std_fun_KS, iter.max=10, centers=6, nstart=10)
CLUSTER_6_KS

CLUSTER_7_KS <- kmeans(clst_data_std_fun_KS, iter.max=10, centers=7, nstart=10)
CLUSTER_7_KS
