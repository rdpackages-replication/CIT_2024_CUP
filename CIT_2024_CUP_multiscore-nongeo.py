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

###########################################################
###########################################################
###########################################################
######### Section 5: Multi-Dimensional RD Designs #########
######### Non-Geographic Empirical Application    #########
###########################################################
###########################################################
###########################################################

# Loading packages
from rdrobust import rdrobust
from rdmulti import rdms
import pandas as pd
import numpy as np

#------------------#
# Loading the data #
#------------------#
data = pd.read_csv("CIT_2024_CUP_multiscore-nongeo.csv")

#---------------------------------------------#
# Snippet 33 (Snippet 5.6 in arXiv pre-print) #
# Using rdms on three boundary points         #
#---------------------------------------------#
cvec = [0, 30, 0]
cvec2 = [0, 0, 50]
out = rdms(Y = data.spadies_any, X = data.running_sisben, 
           X2 = data.running_saber11, zvar = data.tr, C = cvec, C2 = cvec2)

#---------------------------------------------#
# Snippet 34 (Snippet 5.7 in arXiv pre-print) #
# Using rdrobust to illustrate what rdms does #
#---------------------------------------------#
pdim1 = 30
pdim2 = 0
data.insert(4,"dist",np.sqrt( (data.running_sisben - pdim1)**2 + (data.running_saber11 - pdim2)**2 ))
data.dist = data.dist * (2 * data.tr - 1)
out = rdrobust(data.spadies_any, data.dist)
print(out)

#--------------------------------------------------------------#
# Snippet 35 (Snippet 5.8 in arXiv pre-print)                  #
# Creating the perpendicular distance to the boundary (step 1) #
#--------------------------------------------------------------#
data2 = data.dropna(subset=['running_sisben', 'running_saber11']).copy()
#---#
data2['aux1'] = np.abs(data2['running_sisben'])
data2['aux2'] = np.abs(data2['running_saber11'])
#---#
data2['r1'] = data2['running_sisben']
data2['r2'] = data2['running_saber11']
#---#
data2['c'] = np.nan
#---#
data2.loc[(data2['r1'] >= 0) & (data2['r2'] >= 0), 'c'] = 1
data2.loc[(data2['r1'] <= 0) & (data2['r2'] >= 0), 'c'] = 2
data2.loc[(data2['r1'] >= 0) & (data2['r2'] <= 0), 'c'] = 3
data2.loc[(data2['r1'] <= 0) & (data2['r2'] <= 0), 'c'] = 4
#---#
data2['xnorm'] = np.nan
data2.loc[data2['c'] == 1, 'xnorm'] = data2[data2['c'] == 1][['aux1', 'aux2']].apply(lambda row: min(row), axis=1)
data2.loc[data2['c'] == 2, 'xnorm'] = data2['aux1'][data2['c'] == 2]
data2.loc[data2['c'] == 3, 'xnorm'] = data2['aux2'][data2['c'] == 3]
data2.loc[data2['c'] == 4, 'xnorm'] = np.sqrt(data2['aux1'][data2['c'] == 4]**2 + 
                                              data2['aux2'][data2['c'] == 4]**2)

#--------------------------------------------------------------#
# Snippet 36 (Snippet 5.9 in arXiv pre-print)                  #
# Creating the perpendicular distance to the boundary (step 2) #
#--------------------------------------------------------------#
data2.xnorm = data2.xnorm * (2 * data2.tr - 1) 

#----------------------------------------------#
# Snippet 37 (Snippet 5.10 in arXiv pre-print) #
# rdrobust using the perpendicular distance    #
#----------------------------------------------#
out = rdrobust(data2.spadies_any, data2.xnorm)
print(out)

#----------------------------------------------#
# Snippet 38 (Snippet 5.11 in arXiv pre-print) #
# rdms using the perpendicular distance        #
#----------------------------------------------#
out = rdms(Y = data2.spadies_any, X = data2.running_sisben, 
            X2 = data2.running_saber11, zvar = data2.tr, C = cvec, 
            C2 = cvec2, xnorm = data2.xnorm)