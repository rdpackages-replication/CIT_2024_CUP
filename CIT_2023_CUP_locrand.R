#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# A Practical Introduction to Regression Discontinuity Designs: Extensions
# Authors: Matias D. Cattaneo, Nicolás Idrobo and Rocío Titiunik
# Last update: 2023-01-21
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
pdf("outputs/Vol-2-R_senate_rdplot_esmv.pdf")
  rdplot(Y, X,  p = 3, binselect = 'esmv', x.label = 'Score', y.label = 'Outcome', 
         title = '')
dev.off()

#-------------------------------#
# Snippet 2.1                   #
# rdrobust with default options #
#-------------------------------#
txtStart("outputs/Vol-2-R_senate_rdrobust_triangular_mserd_p1_rhofree_regterm1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
  out <- rdrobust(Y, X, kernel = 'triangular',  p = 1, bwselect = 'mserd')
  summary(out)
txtStop()

#----------------------------#
# Snippet 2.2                #
# rdrandinf in ad-hoc window #
#----------------------------#
txtStart("outputs/Vol-2-R_senate_rdrandinf_adhoc_p0.txt", commands = TRUE, 
         results = TRUE, append = FALSE, visible.only = TRUE)
  out <- rdrandinf(Y, X, wl = -2.5, wr = 2.5, seed = 50)
txtStop()

#-------------------------------#
# Snippet 2.3                   #
# Binomial test using rdrandinf #
#-------------------------------#
txtStart("outputs/Vol-2-R_senate_rdrandinf_adhoc_p0_bernoulli.txt", 
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
  bern_prob <- numeric(length(X))
  bern_prob[abs(X)>2.5] <- NA
  bern_prob[abs(X)<=2.5] <- 1/2
  out <- rdrandinf(Y, X, wl = -2.5, wr = 2.5, seed = 50, bernoulli = bern_prob)
txtStop()

#-------------------------------#
# Snippet 2.4                   #
# Fisherian confidence interval #
#-------------------------------#
txtStart("outputs/Vol-2-R_senate_rdrandinf_adhoc_p0_ci.txt", commands = TRUE, 
         results = TRUE, append = FALSE, visible.only = TRUE)
  ci_vec <- c(0.05, seq(from = -20, to = 20, by = 0.10))
  out <- rdrandinf(Y, X, wl = -2.5, wr = 2.5, seed = 50, reps = 1000, ci = ci_vec)
txtStop()

#----------------------------------#
# Snippet 2.5                      #
# Window selection with covariates #
#----------------------------------#
txtStart("outputs/Vol-2-R_senate_rdwinselect_automatic_p0_wobs2_secondtry.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
  Z <- cbind(data$presdemvoteshlag1, data$demvoteshlag1, data$demvoteshlag2, 
             data$demwinprv1, data$demwinprv2, data$dmidterm, data$dpresdem, 
             data$dopen)
  colnames(Z) <- c("presdemvoteshlag1", "demvoteshlag1", "demvoteshlag2", 
                   "demwinprv1", "demwinprv2", "dmidterm", "dpresdem", "dopen")
  out <- rdwinselect(X, Z, seed = 50, reps = 1000, wobs = 2)
txtStop()

#----------------------#
# Figure 2.5           #
# Windows vs. p-values #
#----------------------#
pdf("outputs/Vol-2-R_senate_rdwinselect_automatic_p0_wobs2_secondtry_manywindows.pdf")
  out <- rdwinselect(X, Z, seed = 50, reps = 1000, wobs = 2, nwindows = 200, 
                     plot = TRUE)
dev.off()

#---------------------------------------------------------------#
# Snippet 2.6                                                   #
# Confidence interval with optimal window and power calculation #
#---------------------------------------------------------------#
txtStart("outputs/Vol-2-R_senate_rdrandinf_specific1.txt", commands = TRUE, 
         results = TRUE, append = FALSE, visible.only = TRUE)
  ci_vec <- c(0.05, seq(from = -20, to = 20, by = 0.10))
  out <- rdrandinf(Y, X, wl = -0.7652, wr = 0.7652, seed = 50, reps = 1000, 
                   ci = ci_vec, d = 7.414)
txtStop()

#--------------------------------------------------------#
# Snippet 2.7                                            #
# Falsification: rdrandinf with one particular covariate #
#--------------------------------------------------------#
txtStart("outputs/Vol-2-R_senate_falsification_rdrandinf_presdemvoteshlag1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
  out <- rdrandinf(data$presdemvoteshlag1, X, seed = 50, reps = 1000, 
                   wl = -0.7652, wr = 0.7652)
txtStop()

#---------------------------------------------#
# Table 2.4 (no output)                       #
# Falsification: rdrandinf for all covariates #
#---------------------------------------------#
out <- rdrandinf(data$presdemvoteshlag1, X, seed = 50, reps = 1000, 
                 wl = -0.7652, wr = 0.7652)
out <- rdrandinf(data$demvoteshlag1, X, seed = 50, reps = 1000, 
                 wl = -0.7652, wr = 0.7652)
out <- rdrandinf(data$demvoteshlag2, X, seed = 50, reps = 1000, 
                 wl = -0.7652, wr = 0.7652)
out <- rdrandinf(data$demwinprv1, X, seed = 50, reps = 1000, 
                 wl = -0.7652, wr = 0.7652)
out <- rdrandinf(data$demwinprv2, X, seed = 50, reps = 1000, 
                 wl = -0.7652, wr = 0.7652)
out <- rdrandinf(data$dmidterm, X, seed = 50, reps = 1000, 
                 wl = -0.7652, wr = 0.7652)
out <- rdrandinf(data$dpresdem, X, seed = 50, reps = 1000, 
                 wl = -0.7652, wr = 0.7652)
out <- rdrandinf(data$dopen, X, seed = 50, reps = 1000, 
                 wl = -0.7652, wr = 0.7652)

#------------------------------#
# Snippet 2.8                  #
# Density test using rdrandinf #
#------------------------------#
txtStart("outputs/Vol-2-R_senate_falsification_binomial.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
  out <- rdwinselect(X, wmin = 0.7652, nwindows = 1)
txtStop()

#-----------------------#
# Code snippet 2.9      #
# Binomial test by hand #
#-----------------------#
txtStart("outputs/Vol-2-R_senate_falsification_binomial_byhand.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
  binom.test(25, 41, 1/2)
txtStop()

#-----------------------#
# Snippet 2.10          #
# Placebo cutoff at c=1 #
#-----------------------#
txtStart("outputs/Vol-2-R_senate_falsification_placebocutoff1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
  out <- rdrandinf(Y, X, cutoff = 1, wl = 0.2348, wr = 1.7652, seed = 50)
txtStop()

#---------------------------------------------#
# Table 2.5 (no output)                       #
# Falsification analysis with placebo cutoffs #
#---------------------------------------------#
out <- rdrandinf(Y, X, seed = 50, cutoff = -1, wl = -1.7652, wr = -0.2348)
out <- rdrandinf(Y, X, seed = 50, cutoff = 1, wl = 0.2348, wr = 1.7652)

#------------------------------#
# Snippet 2.11                 #
# Sensitivity to window choice #
#------------------------------#
txtStart("outputs/Vol-2-R_senate_windowsensitivity.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
  out <- rdrandinf(Y, X, wl = -0.6934, wr = 0.6934, seed = 50)
txtStop()
