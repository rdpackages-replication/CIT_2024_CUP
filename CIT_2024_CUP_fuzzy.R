#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# A Practical Introduction to Regression Discontinuity Designs: Extensions
# Authors: Matias D. Cattaneo, Nicolás Idrobo and Rocío Titiunik
#------------------------------------------------------------------------------#
# SOFTWARE WEBSITE: https://rdpackages.github.io/
#------------------------------------------------------------------------------#
# TO INSTALL/DOWNLOAD R PACKAGES/FUNCTIONS:
# install.packages('lpdensity')
# install.packages('rddensity')
# install.packages('rdlocrand')
# install.packages('rdrobust')
# install.packages('rdmulti')
# install.packages('foreign')
# install.packages('ggplot2')
# install.packages('grid')
# install.packages('geosphere')
# install.packages('tidyverse')
# install.packages('sf')
#------------------------------------------------------------------------------#

##################################################
##################################################
##################################################
######### Section 3: The Fuzzy RD Design #########
##################################################
##################################################
##################################################

# Cleaning the R environment
rm(list=ls())

# Loading packages
library(foreign)
library(ggplot2)
library(lpdensity)
library(rddensity)
library(rdrobust)
library(rdlocrand)
library(rdmulti)
library(geosphere)
library(tidyverse)
library(sf)

#------------------#
# Loading the data #
#------------------#
data <- read.dta("CIT_2024_CUP_fuzzy.dta")
Y <- data$Y
X1 <- data$X1
T <- data$T
D <- data$D

# Defining the list of covariates
# I removed privatehs for a moment, because it is causing issues
Z <- cbind(data$icfes_female, data$icfes_age, data$icfes_urm, data$icfes_stratum,
           data$icfes_famsize)
colnames(Z) <- c("icfes_female", "icfes_age", "icfes_urm", "icfes_stratum",
                 "icfes_famsize")

#------------------------------------------#
# Figure 8 (Figure 3.2 in arXiv pre-print) #
# rdplot of the first stage                #
#------------------------------------------#
out <- rdplot(D, X1, title = "", x.label = "Distance to SISBEN cutoff", 
       y.label = "SPP recipient")
plot <- out$rdplot + theme(axis.text.x = element_text(size = 16), 
                           axis.text.y = element_text(size = 16), 
                           axis.title.y = element_text(size = 16), 
                           axis.title.x = element_text(size = 16), 
                           axis.text=element_text(size = 16))
plot

#--------------------------------------------#
# Snippet 8 (Snippet 3.1 in arXiv pre-print) #
# rdrobust of the first stage                #
#--------------------------------------------#
out <- rdrobust(D, X1)
summary(out)

#--------------------------------------------#
# Snippet 9 (Snippet 3.2 in arXiv pre-print) #
# rdrobust of the reduced form               #
#--------------------------------------------#
out <- rdrobust(Y, X1)
summary(out)

#------------------------------------------#
# Figure 9 (Figure 3.3 in arXiv pre-print) #
# rdplot of the reduced form               #
#------------------------------------------#
out <- rdplot(Y, X1, p = 3, title = "", x.label = "Distance to SISBEN cutoff", 
              y.label = "Immediate access in any HEI")
plot <- out$rdplot + theme(axis.text.x = element_text(size = 16), 
                           axis.text.y = element_text(size = 16), 
                           axis.title.y = element_text(size = 16), 
                           axis.title.x = element_text(size = 16), 
                           axis.text=element_text(size = 16))
plot

#---------------------------------------------#
# Snippet 10 (Snippet 3.3 in arXiv pre-print) #
# Fuzzy RD with rdrobust                      #
#---------------------------------------------#
out <- rdrobust(Y, X1, fuzzy = D)
summary(out)

#----------------------------------------------------#
# Snippet 11 (Snippet 3.4 in arXiv pre-print)        #
# Selecting a window with rdwinselect and covariates #
#----------------------------------------------------#
Z <- data[, c("icfes_female", "icfes_age", "icfes_urm", "icfes_stratum",
              "icfes_famsize")]
out <- rdwinselect(X1, Z)

#---------------------------------------------#
# Snippet 12 (Snippet 3.5 in arXiv pre-print) #
# First stage with rdrandinf                  #
#---------------------------------------------#
out <- rdrandinf(D, X1, wl = -0.13000107, wr = 0.13000107)

#---------------------------------------------#
# Snippet 13 (Snippet 3.6 in arXiv pre-print) #
# Reduced form with rdrandinf                 #
#---------------------------------------------#
out <- rdrandinf(Y, X1, wl = -0.13000107, wr = 0.13000107)

#---------------------------------------------#
# Snippet 14 (Snippet 3.7 in arXiv pre-print) #
# Fuzzy RD with rdrandinf                     #
#---------------------------------------------#
out <- rdrandinf(Y, X1, wl = -0.13000107, wr = 0.13000107, fuzzy = c(D,"tsls"))

#---------------------------------------------#
# Snippet 15 (Snippet 3.8 in arXiv pre-print) #
# Manipulation test with rddensity            #
#---------------------------------------------#
out <- rddensity(X1, binoW = 0.13000107, binoNW = 1)
summary(out)

#---------------------------------------------#
# Snippet 16 (Snippet 3.9 in arXiv pre-print) #
# Reduced form on a covariate with rdrobust   #
#---------------------------------------------#
out <- rdrobust(data$icfes_female, X1, bwselect = 'cerrd')
summary(out)

#----------------------------------------------#
# Snippet 17 (Snippet 3.10 in arXiv pre-print) #
# Reduced form on a covariate with rdrandinf   #
#----------------------------------------------#
out <- rdrandinf(data$icfes_female, X1, wl = -0.13000107, wr = 0.13000107)

