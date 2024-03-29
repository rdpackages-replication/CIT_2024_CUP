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

###########################################################
###########################################################
###########################################################
######### Section 5: Multi-Dimensional RD Designs #########
######### Non-Geographic Empirical Application    #########
###########################################################
###########################################################
###########################################################

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
data <- read.dta("CIT_2024_CUP_multiscore-nongeo.dta")

#---------------------------------------------#
# Snippet 33 (Snippet 5.6 in arXiv pre-print) #
# Using rdms on three boundary points         #
#---------------------------------------------#
cvec <- c(0, 30, 0)
cvec2 <- c(0, 0, 50)
Y <- data$spadies_any
X <- data$running_sisben
X2 <- data$running_saber11
Zvar <- data$tr
out <- rdms(Y = Y, X = X, X2 = X2, zvar = Zvar, C = cvec, C2 = cvec2)

#---------------------------------------------#
# Snippet 34 (Snippet 5.7 in arXiv pre-print) #
# Using rdrobust to illustrate what rdms does #
#---------------------------------------------#
pdim1 <- 30
pdim2 <- 0
data$dist <- sqrt( (data$running_sisben - pdim1)^2 + 
                     (data$running_saber11 - pdim2)^2 )
data$dist <- data$dist * (2 * data$tr - 1)
out <- rdrobust(data$spadies_any, data$dist)
summary(out)

#--------------------------------------------------------------#
# Snippet 35 (Snippet 5.8 in arXiv pre-print)                  #
# Creating the perpendicular distance to the boundary (step 1) #
#--------------------------------------------------------------#
data2 <- data[!is.na(data$running_sisben) & !is.na(data$running_saber11),]
#---#
data2$aux1 <- abs(data2$running_sisben)
data2$aux2 <- abs(data2$running_saber11)
#---#
data2$r1 <- data2$running_sisben
data2$r2 <- data2$running_saber11
#---#
data2$c <- NA
data2$c[data2$r1 >= 0 & data2$r2 >= 0] <- 1
data2$c[data2$r1 <= 0 & data2$r2 >= 0] <- 2
data2$c[data2$r1 >= 0 & data2$r2 <= 0] <- 3
data2$c[data2$r1 <= 0 & data2$r2 <= 0] <- 4
#---#
data2$xnorm <- NA
data2$xnorm[data2$c == 1] <- apply(data2[data2$c == 1, c("aux1","aux2")], 1, FUN = min)
data2$xnorm[data2$c == 2] <- data2$aux1[data2$c == 2]
data2$xnorm[data2$c == 3] <- data2$aux2[data2$c == 3]
data2$xnorm[data2$c == 4] <- sqrt(data2$aux1[data2$c == 4]^2 + data2$aux2[data2$c == 4]^2)

#--------------------------------------------------------------#
# Snippet 36 (Snippet 5.9 in arXiv pre-print)                  #
# Creating the perpendicular distance to the boundary (step 2) #
#--------------------------------------------------------------#
data2$xnorm <- data2$xnorm * (2 * data2$tr - 1) 

#----------------------------------------------#
# Snippet 37 (Snippet 5.10 in arXiv pre-print) #
# rdrobust using the perpendicular distance    #
#----------------------------------------------#
out <- rdrobust(data2$spadies_any, data2$xnorm)
summary(out)

#----------------------------------------------#
# Snippet 38 (Snippet 5.11 in arXiv pre-print) #
# rdms using the perpendicular distance        #
#----------------------------------------------#
out <- rdms(Y = data2$spadies_any, X = data2$running_sisben, 
            X2 = data2$running_saber11, zvar = data2$tr, C = cvec, 
            C2 = cvec2, xnorm = data2$xnorm)
