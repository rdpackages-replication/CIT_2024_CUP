#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# A Practical Introduction to Regression Discontinuity Designs: Extensions
# Authors: Matias D. Cattaneo, Nicolás Idrobo and Rocío Titiunik
# Last update: 2023-10-05
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
# install.packages('TeachingDemos')
# install.packages('geosphere')
# install.packages('tidyverse')
# install.packages('sf')
# install.packages('USAboundaries')
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
library(TeachingDemos)
library(rdmulti)
library(geosphere)
library(tidyverse)
library(sf)
library(USAboundaries)

#------------------#
# Loading the data #
#------------------#
data <- read.dta("CIT_2023_CUP_fuzzy.dta")
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

#---------------------------#
# Figure 3.2                #
# rdplot of the first stage #
#---------------------------#
out <- rdplot(D, X1, title = "", x.label = "Distance to SISBEN cutoff", 
       y.label = "SPP recipient")
plot <- out$rdplot + theme(axis.text.x = element_text(size = 16), 
                           axis.text.y = element_text(size = 16), 
                           axis.title.y = element_text(size = 16), 
                           axis.title.x = element_text(size = 16), 
                           axis.text=element_text(size = 16))
plot

#-----------------------------#
# Snippet 3.1                 #
# rdrobust of the first stage #
#-----------------------------#
out <- rdrobust(D, X1)
summary(out)

#------------------------------#
# Snippet 3.2                  #
# rdrobust of the reduced form #
#------------------------------#
out <- rdrobust(Y, X1)
summary(out)

#----------------------------#
# Figure 3.3                 #
# rdplot of the reduced form #
#----------------------------#
out <- rdplot(Y, X1, p = 3, title = "", x.label = "Distance to SISBEN cutoff", 
              y.label = "Immediate access in any HEI")
plot <- out$rdplot + theme(axis.text.x = element_text(size = 16), 
                           axis.text.y = element_text(size = 16), 
                           axis.title.y = element_text(size = 16), 
                           axis.title.x = element_text(size = 16), 
                           axis.text=element_text(size = 16))
plot

#------------------------#
# Snippet 3.3            #
# Fuzzy RD with rdrobust #
#------------------------#
out <- rdrobust(Y, X1, fuzzy = D)
summary(out)

#----------------------------------------------------#
# Snippet 3.4                                        #
# Selecting a window with rdwinselect and covariates #
#----------------------------------------------------#
Z <- data[, c("icfes_female", "icfes_age", "icfes_urm", "icfes_stratum",
              "icfes_famsize")]
out <- rdwinselect(X1, Z)

#----------------------------#
# Snippet 3.5                #
# First stage with rdrandinf #
#----------------------------#
out <- rdrandinf(D, X1, wl = -0.13000107, wr = 0.13000107)

#-----------------------------#
# Snippet 3.6                 #
# Reduced form with rdrandinf #
#-----------------------------#
out <- rdrandinf(Y, X1, wl = -0.13000107, wr = 0.13000107)

#-------------------------#
# Snippet 3.7             #
# Fuzzy RD with rdrandinf #
#-------------------------#
out <- rdrandinf(Y, X1, wl = -0.13000107, wr = 0.13000107, fuzzy = c(D,"tsls"))

#----------------------------------#
# Snippet 3.8                      #
# Manipulation test with rddensity #
#----------------------------------#
out <- rddensity(X1, binoW = 0.13000107, binoNW = 1)
summary(out)

#-------------------------------------------#
# Snippet 3.9                               #
# Reduced form on a covariate with rdrobust #
#-------------------------------------------#
out <- rdrobust(data$icfes_female, X1, bwselect = 'cerrd')
summary(out)

#--------------------------------------------#
# Snippet 3.10                               #
# Reduced form on a covariate with rdrandinf #
#--------------------------------------------#
out <- rdrandinf(data$icfes_female, X1, wl = -0.13000107, wr = 0.13000107)

