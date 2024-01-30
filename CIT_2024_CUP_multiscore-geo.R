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
library(rdmulti)
library(geosphere)
library(tidyverse)
library(sf)

#------------------#
# Loading the data #
#------------------#
data <- read.dta("CIT_2024_CUP_multiscore-geo.dta")

data$dist1[data$treated == 0] <- data$dist1[data$treated == 0] * (-1)
data$dist2[data$treated == 0] <- data$dist2[data$treated == 0] * (-1)
data$dist3[data$treated == 0] <- data$dist3[data$treated == 0] * (-1)
data$perp_dist[data$treated == 0] <- data$perp_dist[data$treated == 0] * (-1)

#--------------------------------------------------------#
# Figure 19 (Figure 5.8 in arXiv pre-print)              #
# Histograms of chordal distance for control and treated #
#--------------------------------------------------------#
data$abschord.d2 <- abs(data$dist2)

# Panel a: Treated observations
h1 <- ggplot(data = data[data$treated == 1,], aes(abschord.d2))+
  labs(x = expression(paste("Chordal distance to ",b[2])), y = "Frequency") +
  geom_histogram(col="black", fill="deepskyblue", alpha = 0.5)+
  theme_bw() + theme(axis.text.x = element_text(size = 16), 
                     axis.text.y = element_text(size = 16), 
                     axis.title.y = element_text(size = 16), 
                     axis.title.x = element_text(size = 16), 
                     axis.text=element_text(size = 16))
h1

# Panel b: Control observations
h2 <- ggplot(data = data[data$treated == 0,], aes(abschord.d2))+
  labs(x = expression(paste("Chordal distance to ",b[2])), y = "Frequency") +
  geom_histogram(col="black", fill="deepskyblue", alpha = 0.5)+
  theme_bw() + theme(axis.text.x = element_text(size = 16), 
                     axis.text.y = element_text(size = 16), 
                     axis.title.y = element_text(size = 16), 
                     axis.title.x = element_text(size = 16), 
                     axis.text=element_text(size = 16))
h2

#----------------------------------------------#
# Snippet 39 (Snippet 5.12 in arXiv pre-print) #
# Using rdrobust with respect to b2            #
#----------------------------------------------#
out <- rdrobust(data$e2008g, data$dist2)
summary(out)

#----------------------------------------------#
# Snippet 40 (Snippet 5.13 in arXiv pre-print) #
# Using rdms and the three boundary points     #
#----------------------------------------------#
lat <- data$lat_cutoff[1:3]
lon <- data$long_cutoff[1:3]
out <- rdms(data$e2008g, data$latitude, lat, data$longitude, data$treat, lon)

#-----------------------------------------------#
# Snippet 41 (Snippet 5.14 in arXiv pre-print)  #
# Using rdrobust and the perpendicular distance #
#-----------------------------------------------#
out <- rdrobust(data$e2008g, data$perp_dist)
summary(out)
