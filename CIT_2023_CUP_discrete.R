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
library(TeachingDemos)
library(rdmulti)
library(geosphere)
library(tidyverse)
library(sf)
library(USAboundaries)

#------------------#
# Loading the data #
#------------------#
data <- read.dta("CIT_2023_CUP_discrete.dta")
left_school <- data$left_school
nextGPA <- data$nextGPA
X <- data$X
T <- data$T
T_X <- T*X

#----------------------------------------------------#
# Figure 4.1                                         #
# Histogram and scatter plot of the running variable #
#----------------------------------------------------#
# Figure 4.1a: Histogram
tempdata <- as.data.frame(X); colnames(tempdata) <- c("v1");
p <- ggplot(data=tempdata, aes(v1))+
  geom_histogram(breaks=seq(-2.8, 0, by = 0.1), col="black", fill="blue", alpha = 1)+
  geom_histogram(breaks=seq(0, 1.6, by = 0.1), col="black", fill="red", alpha = 1)+
  labs(x="Score", y="Number of Observations")+geom_vline(xintercept=0, color="black")+
  theme_bw()
p
ggsave("outputs/Vol-2-R_histogram.pdf", plot = p, width = 6, height = 5, units = "in")

# Figure 4.1b: Scatter plot
p <- ggplot(data[abs(data$X) <= 0.25,], aes(x = X, y = nextGPA)) + 
  geom_point() +
  theme_bw() +
  labs(x = "Score", y = "Next Term GPA (normalized)")
p
ggsave("outputs/Vol-2-R_lso_scatterplot_discreterv.pdf", plot = p, width = 6, height = 5, units = "in")

#-------------------------------------------------------------------#
# Snippet 4.1                                                       #
# Counting the number of observations with X different from missing #
#-------------------------------------------------------------------#
txtStart("outputs/Vol-2-R_lso_countX.txt", commands=TRUE, results=TRUE, 
         append=FALSE, visible.only=TRUE)
  length(X[!is.na(X)])
txtStop()

#---------------------------------#
# Snippet 4.2                     #
# Counting the unique values of X #
#---------------------------------#
txtStart("outputs/Vol-2-R_lso_uniqueX.txt", commands=TRUE, results=TRUE, 
         append=FALSE, visible.only=TRUE)
  length(unique(X))
txtStop()

#-----------------#
# Snippet 4.3     #
# Using rddensity #
#-----------------#
txtStart("outputs/Vol-2-R_lso_falsification_rddensity.txt", commands=TRUE, 
         results=TRUE, append=FALSE, visible.only=TRUE)
  out <- rddensity(X, bino = FALSE)
  summary(out)
txtStop()

#-------------------------------#
# Snippet 4.4                   #
# Using rdrobust on a covariate #
#-------------------------------#
txtStart("outputs/Vol-2-R_lso_falsification_rdrobust_hsgrade_pct.txt", 
         commands=TRUE, results=TRUE, append=FALSE, visible.only=TRUE)
  out <- rdrobust(data$hsgrade_pct, X, bwselect = "cerrd")
  summary(out)
txtStop()

#-----------------------------#
# Snippet 4.5                 #
# Using rdplot on a covariate #
#-----------------------------#
txtStart("outputs/Vol-2-R_lso_falsification_rdplot_hsgrade_pct-COMMAND-ONLY.txt",
         commands=TRUE, results=FALSE, append=FALSE, visible.only=TRUE)
  rdplot(data$hsgrade_pct, X, x.label = "Score", y.label = "", title="")
txtStop()

#----------------------------------------------------#
# Table 4.3                                          #
# RD effects on predetermined covariates (no output) #
#----------------------------------------------------#
summary(rdrobust(data$hsgrade_pct, X, bwselect = "cerrd"))
summary(rdrobust(data$totcredits_year1, X, bwselect = "cerrd"))
summary(rdrobust(data$age_at_entry, X, bwselect = "cerrd"))
summary(rdrobust(data$male, X, bwselect = "cerrd"))
summary(rdrobust(data$bpl_north_america, X, bwselect = "cerrd"))

#-----------------------------#
# Snippet 4.6                 #
# Using rdplot on the outcome #
#-----------------------------#
txtStart("outputs/Vol-2-R_lso3_rdplot_esmv.txt", commands=TRUE, results=TRUE, 
         append=FALSE, visible.only=TRUE)
  out <- rdplot(nextGPA, X,  binselect = 'esmv')
  summary(out)
txtStop()

#-------------------------------#
# Snippet 4.7                   #
# Using rdrobust on the outcome #
#-------------------------------#
txtStart("outputs/Vol-2-R_lso3_rdrobust_triangular_mserd_p1_rhofree_regterm1.txt",
         commands=TRUE, results=TRUE, append=FALSE, visible.only=TRUE)
  out <- rdrobust(nextGPA, X, kernel = 'triangular',  p = 1, 
                  bwselect = 'mserd')
  summary(out)
txtStop()

#----------------------------------------#
# Snippet 4.8                            #
# Using rdrobust and showing its outputs #
#----------------------------------------#
txtStart("outputs/Vol-2-R_lso3_rdrobust_triangular_mserd_p1_rhofree_regterm1_namescoefsout_all.txt",
         commands=TRUE, results=TRUE, append=FALSE, visible.only=TRUE)
  rdout <- rdrobust(nextGPA, X, kernel = 'triangular', p = 1, 
                    bwselect = 'mserd')
  print(names(rdout))
  print(rdout$beta_Y_p_r)
  print(rdout$beta_Y_p_l)
txtStop()

#-----------------------------------------------#
# Snippet 4.9                                   #
# Using rdrobust with clustered standard errors #
#-----------------------------------------------#
txtStart("outputs/Vol-2-R_lso3_rdrobust_triangular_mserd_p1_rhofree_regterm1_cluster.txt",
         commands=TRUE, results=TRUE, append=FALSE, visible.only=TRUE)
  clustervar <- X
  out <- rdrobust(nextGPA, X, kernel = 'triangular', p = 1, 
                  bwselect = 'mserd', vce = 'hc0', cluster = clustervar)
  summary(out)
txtStop()

#------------------------------------------------------#
# Snippet 4.10                                         #
# Using rdrobust on the collapsed data (first outcome) #
#------------------------------------------------------#
txtStart("outputs/Vol-2-R_lso3_rdrobust_collapsed.txt", commands=TRUE, 
         results=TRUE, append=FALSE, visible.only=TRUE)
  data2 <- data.frame(nextGPA, X)
  dim(data2)
  collapsed <- aggregate(nextGPA ~ X, data = data2, mean)
  dim(collapsed)
  out <- rdrobust(collapsed$nextGPA, collapsed$X)
  summary(out)
txtStop()

#--------------------------------#
# Snippet 4.11                   #
# Binomial test with rdwinselect #
#--------------------------------#
txtStart("outputs/Vol-2-R_lso_falsification_binomial.txt", commands=TRUE, 
         results=TRUE, append=FALSE, visible.only=TRUE)
  out <- rdwinselect(X, wmin = 0.01, nwindows = 1, cutoff = 5.00000000000e-06)
txtStop()

#-----------------------#
# Snippet 4.12          #
# Binomial test by hand #
#-----------------------#
txtStart("outputs/Vol-2-R_lso_falsification_binomial_byhand.txt", commands=TRUE, 
         results=TRUE, append=FALSE, visible.only=TRUE)
  binom.test(67, 275, 1/2)
txtStop()

#--------------------------------#
# Snippet 4.13                   #
# Using rdrandinf on a covariate #
#--------------------------------#
txtStart("outputs/Vol-2-R_lso_falsification_rdrandinf_hsgrade_pct.txt", 
         commands=TRUE, results=TRUE, append=FALSE, visible.only=TRUE)
  out <- rdrandinf(data$hsgrade_pct, X, wl = -0.005, wr = 0.01, seed = 50)
txtStop()

#----------------------------------------#
# Table 4.4                              #
# RD effects on predetermined covariates #
#----------------------------------------#
out <- rdrandinf(data$hsgrade_pct, X, wl = -0.005, wr = 0.01, seed = 50)
table <- cbind(out$sumstats[3,1], out$sumstats[3,2], out$obs.stat, out$p.value, 
               out$sumstats[2,1] + out$sumstats[2,2])

out <- rdrandinf(data$totcredits_year1, X, wl = -0.005, wr = 0.01, seed = 50)
table <- rbind(table, cbind(out$sumstats[3,1], out$sumstats[3,2], out$obs.stat, 
                            out$p.value, out$sumstats[2,1] + out$sumstats[2,2]))

out <- rdrandinf(data$age_at_entry, X, wl = -0.005, wr = 0.01, seed = 50)
table <- rbind(table, cbind(out$sumstats[3,1], out$sumstats[3,2], out$obs.stat, 
                            out$p.value, out$sumstats[2,1] + out$sumstats[2,2]))

out <- rdrandinf(data$male, X, wl = -0.005, wr = 0.01, seed = 50)
table <- rbind(table, cbind(out$sumstats[3,1], out$sumstats[3,2], out$obs.stat, 
                            out$p.value, out$sumstats[2,1] + out$sumstats[2,2]))

out <- rdrandinf(data$bpl_north_america, X, wl = -0.005, wr = 0.01, seed = 50)
table <- rbind(table, cbind(out$sumstats[3,1], out$sumstats[3,2], out$obs.stat, 
                            out$p.value, out$sumstats[2,1] + out$sumstats[2,2]))

write.dta(as.data.frame(table), "outputs/Vol-2-R_lso_falsification_rdrandinf_allcovariates.dta")

#---------------------------------------------------------------#
# Snippet 4.14                                                  #
# Using rdwinselect with covariates to determine optimal window #
#---------------------------------------------------------------#
txtStart("outputs/Vol-2-R_lso_rdwinselect_consecutive_windows.txt", 
         commands=TRUE, results=TRUE, append=FALSE, visible.only=TRUE)
  Z <- cbind(data$hsgrade_pct, data$totcredits_year1, data$age_at_entry, 
             data$male, data$bpl_north_america)
  colnames(Z) <- c("hsgrade_pct", "totcredits_year1", "age_at_entry", "male",
                   "bpl_north_america")
  out <- rdwinselect(X, Z, seed = 50, wmin = 0.01, wstep = 0.01, 
                     cutoff = 5.00000000000e-06, level = 0.135)
txtStop()

#--------------------------------#
# Snippet 4.15                   #
# Using rdrandinf on the Outcome #
#--------------------------------#
txtStart("outputs/Vol-2-R_lso3_rdrandinf_adhocsmall_p0.txt", commands=TRUE, 
         results=TRUE, append=FALSE, visible.only=TRUE)
  out <- rdrandinf(nextGPA, X, wl = -0.005, wr = 0.01, seed = 50)
txtStop()

#--------------------------------------#
# Figure 4.2                           #
# rdplots for predetermined covariates #
#--------------------------------------#
pdf("outputs/Vol-2-R_lso_falsification_rdplot_hsgrade_pct.pdf")
  rdplot(data$hsgrade_pct, X, x.label = "Score", y.label = "", title="")
dev.off()

pdf("outputs/Vol-2-R_lso_falsification_rdplot_totcredits_year1.pdf")
  rdplot(data$totcredits_year1, X, x.label = "Score", y.label = "", title="")
dev.off()

pdf("outputs/Vol-2-R_lso_falsification_rdplot_age_at_entry.pdf")
  rdplot(data$age_at_entry, X, x.label = "Score", y.label = "", title="")
dev.off()

pdf("outputs/Vol-2-R_lso_falsification_rdplot_male.pdf")
  rdplot(data$male, X, x.label = "Score", y.label = "", title="")
dev.off()

pdf("outputs/Vol-2-R_lso_falsification_rdplot_bpl_north_america.pdf")
  rdplot(data$bpl_north_america, X, x.label = "Score", y.label = "", title="")
dev.off()

#------------------------#
# Figure 4.3             #
# rdplot for the outcome #
#------------------------#
pdf("outputs/Vol-2-R_lso3_rdplot_esmv.pdf")
  rdplot(nextGPA, X,  binselect = 'esmv', x.label = 'Score', 
         y.label = 'Outcome', title = '')
dev.off()
