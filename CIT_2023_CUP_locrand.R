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

##############################################################################
##############################################################################
##############################################################################
######### Section 2: The Local Randomization Approach to RD Analysis #########
##############################################################################
##############################################################################
##############################################################################

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
data <- read.dta("CIT_2023_CUP_locrand.dta")
Y <- data$Y
X <- data$X
T <- data$T

#----------------------------------------------------#
# Figure 2.3                                         #
# Mimicking variance RD plot with evenly-spaced bins #
#----------------------------------------------------#
out <- rdplot(Y, X,  p = 3, binselect = 'esmv', x.label = 'Score', 
              y.label = 'Outcome', title = '')
plot <- out$rdplot + theme(axis.text.x = element_text(size = 16), 
                           axis.text.y = element_text(size = 16), 
                           axis.title.y = element_text(size = 16), 
                           axis.title.x = element_text(size = 16),
                           axis.text=element_text(size = 16))
plot

#-------------------------------#
# Snippet 2.1                   #
# rdrobust with default options #
#-------------------------------#
out <- rdrobust(Y, X, kernel = 'triangular',  p = 1, bwselect = 'mserd')
summary(out)

#----------------------------#
# Snippet 2.2                #
# rdrandinf in ad-hoc window #
#----------------------------#
out <- rdrandinf(Y, X, wl = -2.5, wr = 2.5, seed = 50)

#-------------------------------#
# Snippet 2.3                   #
# Binomial test using rdrandinf #
#-------------------------------#
bern_prob <- numeric(length(X))
bern_prob[abs(X)>2.5] <- NA
bern_prob[abs(X)<=2.5] <- 1/2
out <- rdrandinf(Y, X, wl = -2.5, wr = 2.5, seed = 50, bernoulli = bern_prob)

#-------------------------------#
# Snippet 2.4                   #
# Fisherian confidence interval #
#-------------------------------#
ci_vec <- c(0.05, seq(from = -20, to = 20, by = 0.10))
out <- rdrandinf(Y, X, wl = -2.5, wr = 2.5, seed = 50, ci = ci_vec)

#----------------------------------#
# Snippet 2.5                      #
# Window selection with covariates #
#----------------------------------#
Z <- data[, 4:11]
out <- rdwinselect(X, Z, seed = 50, wobs = 2)

#----------------------#
# Figure 2.5           #
# Windows vs. p-values #
#----------------------#
out <- rdwinselect(X, Z, seed = 50, wobs = 2, nwindows = 200, 
                   plot = TRUE)

#---------------------------------------------------------------#
# Snippet 2.6                                                   #
# Confidence interval with optimal window and power calculation #
#---------------------------------------------------------------#
ci_vec <- c(0.05, seq(from = -20, to = 20, by = 0.10))
out <- rdrandinf(Y, X, wl = -0.7652, wr = 0.7652, seed = 50, 
                 ci = ci_vec, d = 7.414)

#--------------------------------------------------------#
# Snippet                                                #
# Falsification: rdrandinf with one particular covariate #
#--------------------------------------------------------#
out <- rdrandinf(data$presdemvoteshlag1, X, seed = 50, wl = -0.7652, wr = 0.7652)

#---------------------------------------------#
# Table 2.2                                   #
# Falsification: rdrandinf for all covariates #
#---------------------------------------------#
out <- rdrandinf(data$presdemvoteshlag1, X, seed = 50,
                 wl = -0.7652, wr = 0.7652)
out <- rdrandinf(data$demvoteshlag1, X, seed = 50,
                 wl = -0.7652, wr = 0.7652)
out <- rdrandinf(data$demvoteshlag2, X, seed = 50,
                 wl = -0.7652, wr = 0.7652)
out <- rdrandinf(data$demwinprv1, X, seed = 50,
                 wl = -0.7652, wr = 0.7652)
out <- rdrandinf(data$demwinprv2, X, seed = 50,
                 wl = -0.7652, wr = 0.7652)
out <- rdrandinf(data$dmidterm, X, seed = 50,
                 wl = -0.7652, wr = 0.7652)
out <- rdrandinf(data$dpresdem, X, seed = 50,
                 wl = -0.7652, wr = 0.7652)
out <- rdrandinf(data$dopen, X, seed = 50,
                 wl = -0.7652, wr = 0.7652)

#------------------------------#
# Snippet                      #
# Density test using rdrandinf #
#------------------------------#
out <- rdwinselect(X, wmin = 0.7652, nwindows = 1)

#-----------------------#
# Snippet               #
# Binomial test by hand #
#-----------------------#
binom.test(25, 41, 1/2)

#-----------------------#
# Snippet               #
# Placebo cutoff at c=1 #
#-----------------------#
out <- rdrandinf(Y, X, cutoff = 1, wl = 0.2348, wr = 1.7652, seed = 50)

#---------------------------------------------#
# Table 2.3                                   #
# Falsification analysis with placebo cutoffs #
#---------------------------------------------#
out <- rdrandinf(Y, X, seed = 50, cutoff = -1, wl = -1.7652, wr = -0.2348)
out <- rdrandinf(Y, X, seed = 50, cutoff = 1, wl = 0.2348, wr = 1.7652)

#------------------------------#
# Snippet 2.7                  #
# Sensitivity to window choice #
#------------------------------#
out <- rdrandinf(Y, X, wl = -0.6934, wr = 0.6934, seed = 50)
