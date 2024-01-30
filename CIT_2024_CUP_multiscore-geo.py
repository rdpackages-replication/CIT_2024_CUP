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
######### Geographic Empirical Application        #########
###########################################################
###########################################################
###########################################################

# Loading packages
from rdrobust import rdrobust
from rdmulti import rdms
import matplotlib.pyplot as plt
import pandas as pd

#------------------#
# Loading the data #
#------------------#
data = pd.read_csv("CIT_2024_CUP_multiscore-geo.csv")

data.loc[data['treated'] == 0, 'dist1'] *= -1
data.loc[data['treated'] == 0, 'dist2'] *= -1
data.loc[data['treated'] == 0, 'dist3'] *= -1
data.loc[data['treated'] == 0, 'perp_dist'] *= -1

#--------------------------------------------------------#
# Figure 19 (Figure 5.8 in arXiv pre-print)              #
# Histograms of chordal distance for control and treated #
#--------------------------------------------------------#
data['abschord_d2'] = abs(data['dist2'])

# Panel a: Treated observations
filtered_data = data[data['treated'] == 1]
plt.hist(filtered_data['abschord_d2'], bins=16, edgecolor='black')
plt.title('')
plt.xlabel('Chordal distance to $b_2$')
plt.ylabel('Frequency')
plt.show()

# Panel b: Control observations
filtered_data = data[data['treated'] == 0]
plt.hist(filtered_data['abschord_d2'], bins=16, edgecolor='black')
plt.title('')
plt.xlabel('Chordal distance to $b_2$')
plt.ylabel('Frequency')
plt.show()

#----------------------------------------------#
# Snippet 39 (Snippet 5.12 in arXiv pre-print) #
# Using rdrobust with respect to b2            #
#----------------------------------------------#
out = rdrobust(data.e2008g, data.dist2)
print(out)

#----------------------------------------------#
# Snippet 40 (Snippet 5.13 in arXiv pre-print) #
# Using rdms and the three boundary points     #
#----------------------------------------------#
lat = data['lat_cutoff'].iloc[0:3]
lon = data['long_cutoff'].iloc[0:3]
out = rdms(data.e2008g, data.latitude, lat, data.longitude, data.treated, lon)

#-----------------------------------------------#
# Snippet 41 (Snippet 5.14 in arXiv pre-print)  #
# Using rdrobust and the perpendicular distance #
#-----------------------------------------------#
out = rdrobust(data.e2008g, data.perp_dist)
print(out)