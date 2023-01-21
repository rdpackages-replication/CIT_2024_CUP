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

###########################################################
###########################################################
###########################################################
######### Section 5: Multi-Dimensional RD Designs #########
######### Geographic Empirical Application        #########
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
library(TeachingDemos)
library(rdmulti)
library(geosphere)
library(tidyverse)
library(sf)
library(USAboundaries)

#------------------#
# Loading the data #
#------------------#
data <- read.dta("CIT_2023_CUP_multiscore-geo.dta")

data$dist1[data$treated == 0] <- data$dist1[data$treated == 0] * (-1)
data$dist2[data$treated == 0] <- data$dist2[data$treated == 0] * (-1)
data$dist3[data$treated == 0] <- data$dist3[data$treated == 0] * (-1)
data$perp_dist[data$treated == 0] <- data$perp_dist[data$treated == 0] * (-1)

#--------------------------------------------------------#
# Figure 5.8                                             #
# Histograms of chordal distance for control and treated #
#--------------------------------------------------------#
data$abschord.d2 <- abs(data$dist2)

# Panel a: Treated observations
h1 <- ggplot(data = data[data$treated == 1,], aes(abschord.d2))+
  labs(x = expression(paste("Chordal distance to ",b[2])), y = "Frequency") +
  geom_histogram(col="black", fill="deepskyblue", alpha = 0.5)+
  theme_bw()
h1
ggsave("outputs/Vol-2-kt-HistDist-Tr.pdf", plot = h1, width = 6, height = 5, 
       units = "in")

# Panel b: Control observations
h2 <- ggplot(data = data[data$treated == 0,], aes(abschord.d2))+
  labs(x = expression(paste("Chordal distance to ",b[2])), y = "Frequency") +
  geom_histogram(col="black", fill="deepskyblue", alpha = 0.5)+
  theme_bw()
h2
ggsave("outputs/Vol-2-kt-HistDist-Co.pdf", plot = h2, width = 6, height = 5, 
       units = "in")

#-----------------------------------#
# Snippet 5.12                      #
# Using rdrobust with respect to b2 #
#-----------------------------------#
txtStart("outputs/Vol-2-R_kt_rdrobust_cutoff2.txt", 
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
  out <- rdrobust(data$e2008g, data$dist2)
  summary(out)
txtStop()

#------------------------------------------#
# Snippet 5.13                             #
# Using rdms and the three boundary points #
#------------------------------------------#
txtStart("outputs/Vol-2-R_kt_rdms_basic.txt", 
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
  out <- rdms(data$e2008g, data$latitude, data$lat_cutoff[1:3], data$longitude, 
              data$treat, data$long_cutoff[1:3])
txtStop()

#-------------------------------------------#
# Snippet 5.14                              #
# Using rdms and the perpendicular distance #
#-------------------------------------------#
txtStart("outputs/Vol-2-R_kt_rdms_perpdist.txt", 
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
  out <- rdms(data$e2008g, data$latitude, data$lat_cutoff[1:3], data$longitude, 
              data$treat, data$long_cutoff[1:3], xnorm = data$perp_dist)
txtStop()

#-----------------------------------------------#
# Snippet 5.15                                  #
# Using rdrobust and the perpendicular distance #
#-----------------------------------------------#
txtStart("outputs/Vol-2-R_kt_rdrobust_perpdist.txt", 
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
  out <- rdrobust(data$e2008g, data$perp_dist)
  summary(out)
txtStop()
