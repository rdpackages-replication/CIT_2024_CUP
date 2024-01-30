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

#########################################################################
#########################################################################
#########################################################################
######### Section 4: RD Designs with Discrete Running Variables #########
#########################################################################
#########################################################################
#########################################################################

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
data <- read.dta("CIT_2024_CUP_discrete.dta")
nextGPA <- data$nextGPA
X <- data$X
T <- data$T

#----------------------------------------------------#
# Figure 10 (Figure 4.1 in arXiv pre-print)          #
# Histogram and scatter plot of the running variable #
#----------------------------------------------------#
# Figure 10a (Figure 4.1a in arXiv pre-print): Histogram
tempdata <- as.data.frame(X); colnames(tempdata) <- c("v1");
p <- ggplot(data=tempdata, aes(v1))+
  geom_histogram(breaks=seq(-2.8, 0, by = 0.1), col="black", fill="blue", alpha = 1)+
  geom_histogram(breaks=seq(0, 1.6, by = 0.1), col="black", fill="red", alpha = 1)+
  labs(x="Score", y="Number of Observations")+geom_vline(xintercept=0, color="black")+
  theme_bw() +
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16), 
        axis.text=element_text(size = 16))
p

# Figure 10b (Figure 4.1b in arXiv pre-print): Scatter plot
p <- ggplot(data[abs(data$X) <= 0.25,], aes(x = X, y = nextGPA)) + 
  geom_point() +
  theme_bw() +
  labs(x = "Score", y = "Next Term GPA (normalized)") +
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16), 
        axis.text=element_text(size = 16))
p

#-------------------------------------------------------------------#
# Snippet 18 (Snippet 4.1 in arXiv pre-print)                       #
# Counting the number of observations with X different from missing #
#-------------------------------------------------------------------#
length(X[!is.na(X)])

#---------------------------------------------#
# Snippet 19 (Snippet 4.2 in arXiv pre-print) #
# Counting the unique values of X             #
#---------------------------------------------#
length(unique(X))

#---------------------------------------------#
# Snippet 20 (Snippet 4.3 in arXiv pre-print) #
# Using rddensity                             #
#---------------------------------------------#
out <- rddensity(X, bino = FALSE)
summary(out)

#----------------------------------------------------------#
# Additional analysis (output not reported in publication) #
# Using rdrobust on a covariate                            #
#----------------------------------------------------------#
out <- rdrobust(data$hsgrade_pct, X, bwselect = "cerrd")
summary(out)

#----------------------------------------------------------#
# Additional analysis (output not reported in publication) #
# Using rdplot on a covariate                              #
#----------------------------------------------------------#
rdplot(data$hsgrade_pct, X, x.label = "Score", y.label = "", title="")

#----------------------------------------#
# Table 7 (Table 4.2 in arXiv pre-print) #
# RD effects on predetermined covariates #
#----------------------------------------#
summary(rdrobust(data$hsgrade_pct, X, bwselect = "cerrd"))
summary(rdrobust(data$totcredits_year1, X, bwselect = "cerrd"))
summary(rdrobust(data$age_at_entry, X, bwselect = "cerrd"))
summary(rdrobust(data$male, X, bwselect = "cerrd"))
summary(rdrobust(data$bpl_north_america, X, bwselect = "cerrd"))

#----------------------------------------------------------#
# Additional analysis (output not reported in publication) #
# Using rdplot on the outcome                              #
#----------------------------------------------------------#
out <- rdplot(nextGPA, X,  binselect = 'esmv', x.label = 'Score', 
              y.label = 'Outcome', title = '')
summary(out)

#-------------------------------------------#
# Figure 11 (Figure 4.2 in arXiv pre-print) #
# rdplot for the outcome                    #
#-------------------------------------------#
out <- rdplot(nextGPA, X,  binselect = 'esmv', x.label = 'Score', 
              y.label = 'Outcome', title = '')
plot <- out$rdplot + theme(axis.text.x = element_text(size = 16), 
                           axis.text.y = element_text(size = 16), 
                           axis.title.y = element_text(size = 16), 
                           axis.title.x = element_text(size = 16), 
                           axis.text=element_text(size = 16))
plot

#---------------------------------------------#
# Snippet 21 (Snippet 4.4 in arXiv pre-print) #
# Using rdrobust on the outcome               #
#---------------------------------------------#
out <- rdrobust(nextGPA, X, kernel = 'triangular',  p = 1, 
                bwselect = 'mserd')
summary(out)

#---------------------------------------------#
# Snippet 22 (Snippet 4.5 in arXiv pre-print) #
# Using rdrobust and showing its outputs      #
#---------------------------------------------#
rdout <- rdrobust(nextGPA, X, kernel = 'triangular', p = 1, 
                  bwselect = 'mserd')
print(names(rdout))
print(rdout$beta_Y_p_r)
print(rdout$beta_Y_p_l)

#-----------------------------------------------#
# Snippet 23 (Snippet 4.6 in arXiv pre-print)   #
# Using rdrobust with clustered standard errors #
#-----------------------------------------------#
clustervar <- X
out <- rdrobust(nextGPA, X, vce = 'hc0', cluster = clustervar)
summary(out)

#------------------------------------------------------#
# Snippet 24 (Snippet 4.7 in arXiv pre-print)          #
# Using rdrobust on the collapsed data (first outcome) #
#------------------------------------------------------#
data2 <- data.frame(nextGPA, X)
dim(data2)
collapsed <- aggregate(nextGPA ~ X, data = data2, mean)
dim(collapsed)
out <- rdrobust(collapsed$nextGPA, collapsed$X)
summary(out)

#----------------------------------------------------------#
# Additional analysis (output not reported in publication) #
# Binomial test with rdwinselect                           #
#----------------------------------------------------------#
out <- rdwinselect(X, wmin = 0.01, nwindows = 1, cutoff = 5.00000000000e-06)

#---------------------------------------------#
# Snippet 25 (Snippet 4.8 in arXiv pre-print) #
# Binomial test by hand                       #
#---------------------------------------------#
binom.test(67, 275, 1/2)

#----------------------------------------------------------#
# Additional analysis (output not reported in publication) #
# Using rdrandinf on a covariate                           #
#----------------------------------------------------------#
out <- rdrandinf(data$hsgrade_pct, X, wl = -0.005, wr = 0.01, seed = 50)

#----------------------------------------#
# Table 8 (Table 4.3 in arXiv pre-print) #
# RD effects on predetermined covariates #
#----------------------------------------#
out <- rdrandinf(data$hsgrade_pct, X, wl = -0.005, wr = 0.01, seed = 50)

out <- rdrandinf(data$totcredits_year1, X, wl = -0.005, wr = 0.01, seed = 50)

out <- rdrandinf(data$age_at_entry, X, wl = -0.005, wr = 0.01, seed = 50)

out <- rdrandinf(data$male, X, wl = -0.005, wr = 0.01, seed = 50)

out <- rdrandinf(data$bpl_north_america, X, wl = -0.005, wr = 0.01, seed = 50)

#---------------------------------------------------------------#
# Snippet 26 (Snippet 4.9 in arXiv pre-print)                   #
# Using rdwinselect with covariates to determine optimal window #
#---------------------------------------------------------------#
Z <- data[, c("hsgrade_pct", "totcredits_year1", "age_at_entry", "male",
              "bpl_north_america")]
out <- rdwinselect(X, Z, seed = 50, wmin = 0.01, wstep = 0.01, 
                   cutoff = 5.00000000000e-06, level = 0.135)

#----------------------------------------------#
# Snippet 27 (Snippet 4.10 in arXiv pre-print) #
# Using rdrandinf on the Outcome               #
#----------------------------------------------#
out <- rdrandinf(nextGPA, X, wl = -0.005, wr = 0.01, seed = 50)

#----------------------------------------------------------#
# Additional analysis (output not reported in publication) #
# rdplots for predetermined covariates                     #
#----------------------------------------------------------#
rdplot(data$hsgrade_pct, X, x.label = "Score", y.label = "", title="")

rdplot(data$totcredits_year1, X, x.label = "Score", y.label = "", title="")

rdplot(data$age_at_entry, X, x.label = "Score", y.label = "", title="")

rdplot(data$male, X, x.label = "Score", y.label = "", title="")

rdplot(data$bpl_north_america, X, x.label = "Score", y.label = "", title="")
