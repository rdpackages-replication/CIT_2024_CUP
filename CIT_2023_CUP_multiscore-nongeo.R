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
library(TeachingDemos)
library(rdmulti)
library(geosphere)
library(tidyverse)
library(sf)
library(USAboundaries)

#------------------#
# Loading the data #
#------------------#
data <- read.dta("CIT_2023_CUP_multiscore-nongeo.dta")

#-----------------------------------------------#
# Figure 5.5b                                   #
# Multi-Score RD Designs: Geographic Assignment #
#-----------------------------------------------#
# Get the county data
counties <- us_counties()

counties.PA <- counties[counties$stusps=="PA",]
counties.NJ.NY <- counties[counties$stusps=="NJ" | counties$stusps=="NY",]

borders <- st_intersection(st_buffer(counties.PA, 2000), 
                           st_buffer(counties.NJ.NY, 2000))

# Plot the data
map <- ggplot() + 
  labs(x = expression(Longitude~(X[1])), y = expression(Latitude~(X[2]))) +
  geom_sf(data = counties.PA, color = "black", fill = "lavender") +
  geom_sf(data = counties.NJ.NY, color = "black", fill = "pink") +
  geom_sf(data = borders, color = "purple", fill = "purple") +
  theme_minimal() + theme(legend.position = 'none') +
  theme(axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        axis.text = element_text(size = 16)) +
  annotate("text", label = "Control Area",   x = -77.8, y = 39, size = 5, colour="black") +
  annotate("segment", x = -77.8, xend = -77.8, y = 39.1, yend=39.7, 
           colour="black", linetype = 1, 
           arrow = arrow(length = unit(0.25, "cm"))) +
  annotate("text", label = "Treated Area", x = -77.8, y = 44.5, size = 5, colour="black") +
  annotate("segment", x = -77.8, xend = -77.8, y = 44.3, yend=43.4, 
           colour="black", linetype = 1, 
           arrow = arrow(length = unit(0.25, "cm")))
map
ggsave("outputs/Vol-2-R_multiscore_geographic.pdf", plot = map, width = 7, 
       height = (13/16)*7, units = "in")

#-------------------------------------#
# Snippet 5.6                         #
# Using rdms on three boundary points #
#-------------------------------------#
txtStart("outputs/Vol-2-R_LRS_rdms_threepoints.txt", commands = TRUE, 
         results = TRUE, append = FALSE, visible.only = TRUE)
  cvec <- c(0, 30, 0)
  cvec2 <- c(0, 0, 50)
  out <- rdms(Y = data$spadies_any, X = data$running_sisben, 
              X2 = data$running_saber11, zvar = data$tr, C = cvec, C2 = cvec2)
txtStop()

#---------------------------------------------#
# Snippet 5.7                                 #
# Using rdrobust to illustrate what rdms does #
#---------------------------------------------#
txtStart("outputs/Vol-2-R_LRS_rdms_vs_rdrobust.txt", commands = TRUE, 
         results = TRUE, append = FALSE, visible.only = TRUE)
  pdim1 <- 30
  pdim2 <- 0
  data$dist <- sqrt( (data$running_sisben - pdim1)^2 + 
                       (data$running_saber11 - pdim2)^2 )
  data$dist <- data$dist * (2 * data$tr - 1)
  out <- rdrobust(data$spadies_any, data$dist)
  summary(out)
txtStop()

#--------------------------------------------------------------#
# Snippet 5.8                                                  #
# Creating the perpendicular distance to the boundary (step 1) #
#--------------------------------------------------------------#
txtStart("outputs/Vol-2-R_LRS_rdms_perpendicular_dist_step1.txt", commands = TRUE, 
         results = TRUE, append = FALSE, visible.only = TRUE)
  data2 <- data[is.na(data$running_sisben) == FALSE & 
                  is.na(data$running_saber11) == FALSE,]
  #---#
  data2$aux1 <- abs(data2$running_sisben)
  data2$aux2 <- abs(data2$running_saber11)
  #---#
  data2$case <- NA
  data2$case[data2$running_sisben >= 0 & data2$running_saber11 >= 0] <- 1
  data2$case[data2$running_sisben <= 0 & data2$running_saber11 >= 0] <- 2
  data2$case[data2$running_sisben >= 0 & data2$running_saber11 <= 0] <- 3
  data2$case[data2$running_sisben <= 0 & data2$running_saber11 <= 0] <- 4
  #---#
  data2$xnorm <- NA
  data2$xnorm[data2$case == 1] <- apply(data2[data2$case == 1, c("aux1","aux2")], 
                                        1, FUN = min)
  data2$xnorm[data2$case == 2] <- data2$aux1[data2$case == 2]
  data2$xnorm[data2$case == 3] <- data2$aux2[data2$case == 3]
  data2$xnorm[data2$case == 4] <- sqrt(data2$aux1[data2$case == 4]^2 + 
                                         data2$aux2[data2$case == 4]^2)
txtStop()

#--------------------------------------------------------------#
# Snippet 5.9                                                  #
# Creating the perpendicular distance to the boundary (step 2) #
#--------------------------------------------------------------#
txtStart("outputs/Vol-2-R_LRS_rdms_perpendicular_dist_step2.txt", commands = TRUE, 
         results = TRUE, append = FALSE, visible.only = TRUE)
  data2$xnorm <- data2$xnorm * (2 * data2$tr - 1) 
txtStop()

#-------------------------------------------#
# Snippet 5.10                              #
# rdrobust using the perpendicular distance #
#-------------------------------------------#
txtStart("outputs/Vol-2-R_LRS_rdms_perpendicular_dist_rdrobust.txt", 
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
  out <- rdrobust(data2$spadies_any, data2$xnorm)
  summary(out)
txtStop()

#---------------------------------------#
# Snippet 5.11                          #
# rdms using the perpendicular distance #
#---------------------------------------#
txtStart("outputs/Vol-2-R_LRS_rdms_perpendicular_dist.txt", commands = TRUE, 
         results = TRUE, append = FALSE, visible.only = TRUE)
  out <- rdms(Y = data2$spadies_any, X = data2$running_sisben, 
              X2 = data2$running_saber11, zvar = data2$tr, C = cvec, 
              C2 = cvec2, xnorm = data2$xnorm)
txtStop()
