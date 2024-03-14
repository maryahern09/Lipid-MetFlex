#Updated code for sPLS-DA on 3-11-24 by Mary 

#Here are the steps to make a sPLS-DA using cluster components as the predictor
#in a multilevel structure

# Step 0 data prep and import
#Sri ran code for cluster components by week so that needs to be imported (only HB and LB)
#structure of the data: needs to be the clusters as the rows and components as columns
#X is the cluster components
#Y is the categorical NBC groups as a vector and factor
#multilevel is the Week
#ind names is going to be a vector as a factor with the clusters listed 1-X number of clusters

# Step 1 PCA and sPLS-DA no multilevel

# Step 2 PCA sPLS-DA Multilevel

############Important Note######################################################
#The multilevel sPLSDA will only run on version 4.2.3 of R so you need to run it in that
#you will come across an error when you run the sPLSDA model if you try to run on a different version


################################################################################
### Step 0 data prep and import ################################################

#install relevant packages
install.packages("readxl")
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(mixOmics)

# download cluster components data set (already no vb)
cluster_components <- read_csv("Cluster components from elimination for splsda HB LB only.csv")

# variable that demonstrates the repeats needs to be a factor
cluster_components$`Subject ID` <- as.factor(cluster_components$`Subject ID`)

# creating a cluster components data set that just includes the cc
cluster_components_cleaned <- cluster_components[
  , !(names(cluster_components) %in% c("Week", "NBC", "Subject ID"))]

################################################################################
### Step 1 Perform PCA and sPLSDA no ML ########################################

#PCA without as a test multilevel
pca.result <- pca(cluster_components_cleaned, scale = TRUE)
plotIndiv(pca.result, 
          ind.names = cluster_components$`Subject ID`, 
          group = cluster_components$NBC, 
          legend = TRUE, 
          legend.title = "Treatment", 
          title = 'Figure 1a: PCA on CC')

#initial sPLS-DA model- no ML
cc.splsda <- splsda(cluster_components_cleaned, cluster_components$NBC, 
                    ncomp = 10) 

# plot the samples- no ML
plotIndiv(cc.splsda , comp = 1:2, 
          group = cluster_components$NBC, ind.names = cluster_components$`Subject ID`,  # color points by class
          ellipse = TRUE, # include 95% confidence ellipse for each class
          legend = TRUE, title = '(a) PLSDA with confidence ellipses')

#Need to calculate VIP variables
cc.splsda.vip <- vip(cc.splsda) #assign VIP variables and their scores to an object
View(cc.splsda) #view VIP scores by component
cc.splsda.scores <- cc.splsda$loadings #Get the loadings from PCs of PLS

#Plot loadings
plotLoadings(cc.splsda, method = 'median', contrib = 'max',  
             title = "(a) Loadings no ML")

###Above models work (no ML- still working on ML)

#############################################################################
### Step 2 PCA and sPLS-DA with Multilevel ##################################

#PCA with the multilevel
pca.result_ML <- pca(cluster_components_cleaned, 
                     multilevel = cluster_components$`Subject ID`, scale = TRUE)
plotIndiv(pca.result_ML, 
          ind.names = cluster_components$`Subject ID`, 
          group = cluster_components$NBC, 
          legend = TRUE, 
          legend.title = "Treatment", 
          title = 'Figure 1b: Multilevel PCA on CC data')

#sPLS-DA with ML
#different ex of multilevel error correction attempts
cluster_components$NBC <- as.factor(cluster_components$NBC)
sample <- cluster_components$'Subject ID'
sample_df <- data.frame(sample)

#splsda ML
cc.splsda_ML <- splsda(cluster_components_cleaned, cluster_components$NBC, 
                       multilevel = cluster_components$Week, ncomp = 10) #used the same ML that worked above
                     
# plot the samples ML
plotIndiv(cc.splsda_ML, ind.names = cluster_components$NBC,  # colour points by class
          legend = TRUE, title = '(a) PLSDA with confidence ellipses')

#Need to calculate VIP variables
cc.splsda_ML.vip <- vip(cc.splsda_ML) #assign VIP variables and their scores to an object
View(cc.splsda_ML) #view VIP scores by component
cc.splsda_ML.scores <- cc.splsda_ML$loadings #Get the loadings from PCs of PLS

#Plot loadings
plotLoadings(cc.splsda_ML, method = 'median', contrib = 'max',  
             title = "(a) Loadings")
