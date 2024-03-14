#Updated code for sPLS-DA on 3-11-24 by Mary 

#Here is a code for fine tuning the sPLSDA code in the "splsda 2nd try" code 
#see other code for the correct models but this is code for tuning

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

#sPLS-DA with ML
#different ex of multilevel error correction attempts
cluster_components$NBC <- as.factor(cluster_components$NBC)
sample <- cluster_components$'Subject ID'
sample_df <- data.frame(sample)

#I think I need to do the ncomp = optimal.ncomp, keepX = optimal.keepX but thinking that needs an ML specification and that won't download on my comp
# tune basic model
#n comp
tuned.cc.splsda <- perf(cc.splsda, validation = "Mfold", 
                        folds = 5, nrepeat = 10, # use repeated cross-validation
                        progressBar = FALSE, auc = TRUE, multilevel = cluster_components$`Subject ID`) 
plot(tuned.cc.splsda, col = color.mixo(5:7), sd = TRUE,
     legend.position = "horizontal")

tuned.cc.splsda$choice.ncomp

#keep x parameter
# grid of possible keepX values that will be tested for each component
list.keepX <- c(1:10,  seq(20, 300, 10))

# undergo the tuning process to determine the optimal number of variables
tunedX.cc.splsda <- tune.splsda(cluster_components_cleaned, cluster_components$NBC, ncomp = 2, # calculate for first 4 components
                                validation = 'Mfold',
                                folds = 5, nrepeat = 10, # use repeated cross-validation
                                dist = 'max.dist', # use max.dist measure
                                measure = "BER", # use balanced error rate of dist measure
                                test.keepX = list.keepX,
                                cpus = 2) # allow for paralleliation to decrease runtime

plot(tunedX.cc.splsda) # plot output of variable number tuning

# determining final ncomp and keep X
tunedX.cc.splsda$choice.ncomp$ncomp # what is the optimal value of components according to tune.splsda()
tunedX.cc.splsda$choice.keepX # what are the optimal values of variables according to tune.splsda()
optimal.ncomp <- tunedX.cc.splsda$choice.ncomp$ncomp
optimal.keepX <- tunedX.cc.splsda$choice.keepX[1:optimal.ncomp]

#splsda ML
design <- data.frame(sample = cluster_components$`Subject ID`)# this is the way they did it in the ex online

cc.splsda_ML <- splsda(cluster_components_cleaned, cluster_components$NBC, 
                       ncomp = optimal.ncomp, 
                       keepX = optimal.keepX,
                       multilevel = design) #used the same ML that worked above

# plot the samples ML
plotIndiv(cc.splsda_ML, comp = 1:2, 
          group = cluster_components$NBC, ind.names = cluster_components$`Subject ID`,  # colour points by class
          ellipse = TRUE, # include 95% confidence ellipse for each class
          legend = TRUE, title = '(a) PLSDA with confidence ellipses')

#Need to calculate VIP variables- these were calculated on the not ML and ran fine

plotLoadings(cc.splsda, comp = 1, 
             method = 'mean', contrib = 'max',  
             size.name = 0.8, legend = FALSE,  
             ndisplay = 20,
             title = "(a) Loadings of comp. 1")

plotLoadings(cc.splsda, comp = 2, 
             method = 'median', contrib = 'max',   
             size.name = 0.5,
             ndisplay = 36,
             title = "(b) Loadings of comp. 2")
