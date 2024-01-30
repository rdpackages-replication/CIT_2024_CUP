#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# A Practical Introduction to Regression Discontinuity Designs: Extensions
# Authors: Matias D. Cattaneo, Nicolás Idrobo and Rocío Titiunik
#-----------------------------------------------------------------------------#
# SOFTWARE WEBSITE: https://rdpackages.github.io/
#-----------------------------------------------------------------------------#
# TO INSTALL/DOWNLOAD PYTHON PACKAGES/FUNCTIONS:
# pip install rdrobust
# pip install rdlocrand
# pip install rddensity
# pip install rdmulti
#-----------------------------------------------------------------------------#

##################################################
##################################################
##################################################
######### Section 3: The Fuzzy RD Design #########
##################################################
##################################################
##################################################

# Loading packages
from rdrobust import rdrobust, rdplot
from rdlocrand import rdrandinf, rdwinselect
import rddensity
import pandas as pd

#------------------#
# Loading the data #
#------------------#
data = pd.read_csv("CIT_2024_CUP_fuzzy.csv")

# Defining a structure with the covariates to be used below
selected_columns = ["icfes_female", "icfes_age", "icfes_urm", "icfes_stratum", 
                    "icfes_famsize"]
Z = data[selected_columns].copy()

#------------------------------------------#
# Figure 8 (Figure 3.2 in arXiv pre-print) #
# rdplot of the first stage                #
#------------------------------------------#
out = rdplot(y = data.D, x = data.X1)

#--------------------------------------------#
# Snippet 8 (Snippet 3.1 in arXiv pre-print) #
# rdrobust of the first stage                #
#--------------------------------------------#
out = rdrobust(data.D, data.X1)
print(out)

#--------------------------------------------#
# Snippet 9 (Snippet 3.2 in arXiv pre-print) #
# rdrobust of the reduced form               #
#--------------------------------------------#
out = rdrobust(data.Y, data.X1)
print(out)

#------------------------------------------#
# Figure 9 (Figure 3.3 in arXiv pre-print) #
# rdplot of the reduced form               #
#------------------------------------------#
out = rdplot(data.Y, data.X1, p = 3, x_label = "Distance to SISBEN cutoff", 
             y_label = "Immediate access in any HEI", title = "")

#---------------------------------------------#
# Snippet 10 (Snippet 3.3 in arXiv pre-print) #
# Fuzzy RD with rdrobust                      #
#---------------------------------------------#
out = rdrobust(data.Y, data.X1, fuzzy = data.D)
print(out)

#----------------------------------------------------#
# Snippet 11 (Snippet 3.4 in arXiv pre-print)        #
# Selecting a window with rdwinselect and covariates #
#----------------------------------------------------#
out = rdwinselect(data.X1, Z)

#---------------------------------------------#
# Snippet 12 (Snippet 3.5 in arXiv pre-print) #
# First stage with rdrandinf                  #
#---------------------------------------------#
out = rdrandinf(data.D, data.X1, wl = -0.13000107, wr = 0.13000107)

#---------------------------------------------#
# Snippet 13 (Snippet 3.6 in arXiv pre-print) #
# Reduced form with rdrandinf                 #
#---------------------------------------------#
out = rdrandinf(data.Y, data.X1, wl = -0.13000107, wr = 0.13000107)

#---------------------------------------------#
# Snippet 14 (Snippet 3.7 in arXiv pre-print) #
# Fuzzy RD with rdrandinf                     #
#---------------------------------------------#
out = rdrandinf(data.Y, data.X1, wl = -0.13000107, wr = 0.13000107, 
                fuzzy = [data.D,"tsls"])

#---------------------------------------------#
# Snippet 15 (Snippet 3.8 in arXiv pre-print) #
# Manipulation test with rddensity            #
#---------------------------------------------#
rddensity.rddensity(data.X1, binoW = 0.13000107, binoNW = 1)

#---------------------------------------------#
# Snippet 16 (Snippet 3.9 in arXiv pre-print) #
# Reduced form on a covariate with rdrobust   #
#---------------------------------------------#
out = rdrobust(data.icfes_female, data.X1, bwselect = 'cerrd')
print(out)

#----------------------------------------------#
# Snippet 17 (Snippet 3.10 in arXiv pre-print) #
# Reduced form on a covariate with rdrandinf   #
#----------------------------------------------#
out = rdrandinf(data.icfes_female, data.X1, wl = -0.13000107, wr = 0.13000107)