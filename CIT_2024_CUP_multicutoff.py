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
from rdrobust import rdrobust, rdplot
from rdmulti import rdmc, rdmcplot
from scipy.stats import norm
import pandas as pd
import numpy as np
import math

#------------------#
# Loading the data #
#------------------#
data = pd.read_csv("CIT_2024_CUP_multicutoff.csv")

#-------------------------------------------#
# Figure 15 (Figure 5.4 in arXiv pre-print) #
# Panel a: rdplot on one cutoff             #
# Panel b: rdmcplot on the three cutoffs    #
#-------------------------------------------#
# Panel a
data_cut1 = data[data['cutoff'] == -57.21][['spadies_any', 'sisben_score']]

out = rdplot(data_cut1.spadies_any, data_cut1.sisben_score, c = -57.21, p = 1, 
             title = "", x_label = "Distance to SISBEN cutoff", 
             y_label = "Immediate access in any HEI")

#-----#
# Panel b

# Creating smaller data frames, one for each cutoff
data_cut1 = data[data['cutoff'] == -57.21][['spadies_any', 'sisben_score']]
data_cut2 = data[data['cutoff'] == -56.32][['spadies_any', 'sisben_score']]
data_cut3 = data[data['cutoff'] == -40.75][['spadies_any', 'sisben_score']]

# Calling rdplot on each cutoff and extracting the optimal number of bins
out = rdplot(data_cut1.spadies_any, data_cut1.sisben_score, 
              p = 1, c = -57.21, binselect = "esmv")
bins_cut1 = np.ceil(np.array(out.J).ravel() / 2)

out = rdplot(data_cut2.spadies_any, data_cut2.sisben_score, 
              p = 1, c = -56.32, binselect = "esmv")
bins_cut2 = np.ceil(np.array(out.J).ravel() / 2)

out = rdplot(data_cut3.spadies_any, data_cut3.sisben_score, 
              p = 1, c = -40.75, binselect = "esmv")
bins_cut3 = np.ceil(np.array(out.J).ravel() / 2)

# Calling rdmcplot and using the number of bins defined above
aux = rdmcplot(data.spadies_any, data.sisben_score, data.cutoff, 
         pvec = [1,1,1], binselectvec = ['esmv','esmv','esmv'], 
         nbinsmat = np.vstack((bins_cut1, bins_cut2, bins_cut3)))

#---------------------------------------------#
# Snippet 28 (Snippet 5.1 in arXiv pre-print) #
# rdrobust using cutoff 1                     #
#---------------------------------------------#
data_cut1 = data[data['cutoff'] == -57.21][['spadies_any', 'sisben_score']]

out = rdrobust(data_cut1.spadies_any, data_cut1.sisben_score, c = -57.21)
print(out)

#---------------------------------------------#
# Snippet 29 (Snippet 5.2 in arXiv pre-print) #
# Using rdmc and the three cutoffs            #
#---------------------------------------------#
out = rdmc(data.spadies_any, data.sisben_score, data.cutoff)

#---------------------------------------------#
# Snippet 30 (Snippet 5.3 in arXiv pre-print) #
# Using rdrobust with a normalized score      #
#---------------------------------------------#
data['area'] = 0
data.loc[data['sisben_area'] == 'Main metro area', 'area'] = 1
data.loc[data['sisben_area'] == 'Other urban area', 'area'] = 2
data.loc[data['sisben_area'] == 'Rural area', 'area'] = 3
#---#
data['xnorm'] = 0
data['sisben_score'] = data['sisben_score'].astype(np.float64) 
data['xnorm'] = data['xnorm'].astype(np.float64)
data.loc[data['area'] == 1, 'xnorm'] = data.loc[data['area'] == 1, 'sisben_score'] + 57.21
data.loc[data['area'] == 2, 'xnorm'] = data.loc[data['area'] == 2, 'sisben_score'] + 56.32
data.loc[data['area'] == 3, 'xnorm'] = data.loc[data['area'] == 3, 'sisben_score'] + 40.75
#---#
out = rdrobust(data.spadies_any, data.xnorm, c = 0)
print(out)

#---------------------------------------------#
# Snippet 31 (Snippet 5.4 in arXiv pre-print) #
# Using rdmc and understanding its outputs    #
#---------------------------------------------#
out = rdmc(data.spadies_any, data.sisben_score, data.cutoff)
Coefs = out.Coefs
Coefs = pd.DataFrame(Coefs[0])
W = out.W
W = pd.DataFrame(W[0][0]).T
print(Coefs)
print(W)

W.columns = Coefs.columns[:3]
result = Coefs.iloc[:, :3] * W
result = result.sum().sum()
print(result)

#--------------------------------------------------------------------#
# Snippet 32 (Snippet 5.5 in arXiv pre-print)                        #
# Formally testing the difference between the effects at the cutoffs #
#--------------------------------------------------------------------#
out = rdmc(data.spadies_any, data.sisben_score, data.cutoff)
B = out.B
B = B[0]
V = out.V
V = V[0]

dif_notrounded = (B['1'] - B['2']).iloc[0]
dif = round(dif_notrounded, 3)
print(dif)

se_notrounded = math.sqrt((V['1'] + V['2']).iloc[0])
se = round(se_notrounded, 3)
print(se)

tstat_notrounded = dif_notrounded / se_notrounded
tstat = round(tstat_notrounded, 3)
print(tstat)

pval_notrounded = 2 * norm.cdf(-abs(tstat_notrounded))
pval = round(pval_notrounded, 3)
print(pval)