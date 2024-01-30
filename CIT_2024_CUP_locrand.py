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

##############################################################################
##############################################################################
##############################################################################
######### Section 2: The Local Randomization Approach to RD Analysis #########
##############################################################################
##############################################################################
##############################################################################

# Loading packages
from rdrobust import rdrobust, rdplot
from rdlocrand import rdrandinf, rdwinselect
from scipy import stats
import pandas as pd
import numpy as np

# Loading the data and defining the main variables
data = pd.read_csv("CIT_2024_CUP_locrand.csv")

#----------------------------------------------------#
# Figure 4 (Figure 2.3 in arXiv pre-print)           #
# Mimicking variance RD plot with evenly-spaced bins #
#----------------------------------------------------#
out = rdplot(y = data.Y, x = data.X, p = 3, x_label = "Score", 
             y_label = "Outcome", title = "")

#--------------------------------------------#
# Snippet 1 (Snippet 2.1 in arXiv pre-print) #
# rdrobust with default options              #
#--------------------------------------------#
out = rdrobust(data.Y, data.X, kernel = "triangular", p = 1, 
               bwselect = "mserd")
print(out)

#--------------------------------------------#
# Snippet 2 (Snippet 2.2 in arXiv pre-print) #
# rdrandinf in ad-hoc window                 #
#--------------------------------------------#
out = rdrandinf(data.Y, data.X, wl = -2.5, wr = 2.5, seed = 50)

#--------------------------------------------#
# Snippet 3 (Snippet 2.3 in arXiv pre-print) #
# Binomial test using rdrandinf              #
#--------------------------------------------#
bern_prob = np.zeros(len(data.X))
bern_prob[np.abs(data.X) > 2.5] = np.nan
bern_prob[np.abs(data.X) <= 2.5] = 1/2

out = rdrandinf(data.Y, data.X, wl = -2.5, wr = 2.5, seed = 50, 
                bernoulli = bern_prob)

#--------------------------------------------#
# Snippet 4 (Snippet 2.4 in arXiv pre-print) #
# Fisherian confidence interval              #
#--------------------------------------------#
ci_vec = np.concatenate(([0.05], np.arange(-20, 21, 0.10)))
out = rdrandinf(data.Y, data.X, wl = -2.5, wr = 2.5, seed = 50, ci = ci_vec)

#--------------------------------------------#
# Snippet 5 (Snippet 2.5 in arXiv pre-print) #
# Window selection with covariates           #
#--------------------------------------------#
Z = data[["presdemvoteshlag1", "demvoteshlag1", "demvoteshlag2", 
          "demwinprv1",	"demwinprv2", "dmidterm", "dpresdem", "dopen"]]
colnames_Z = ["DemPres Vote", "DemSen Vote t-1", "DemSen Vote t-2",
              "DemSen Win t-1", "DemSen Win t-2", "Midterm", "DemPres", "Open"]
Z.columns = colnames_Z

out = rdwinselect(data.X, Z, seed = 50, wobs = 2)

#------------------------------------------#
# Figure 6 (Figure 2.5 in arXiv pre-print) #
# Windows vs. p-values                     #
#------------------------------------------#
out = rdwinselect(data.X, Z, seed = 50, wobs = 2, nwindows = 200, plot = True)

#---------------------------------------------------------------#
# Snippet 6 (Snippet 2.6 in arXiv pre-print)                    #
# Confidence interval with optimal window and power calculation #
#---------------------------------------------------------------#
ci_vec = np.concatenate(([0.05], np.arange(-20, 21, 0.10)))
out = rdrandinf(data.Y, data.X, wl = -0.7652, wr = 0.7652, seed = 50, 
                ci = ci_vec, d = 7.414)

#----------------------------------------------------------#
# Additional analysis (output not reported in publication) #
# Falsification: rdrandinf with one particular covariate   #
#----------------------------------------------------------#
out = rdrandinf(data.presdemvoteshlag1, data.X, seed = 50, 
                wl = -0.7652, wr = 0.7652)

#---------------------------------------------#
# Table 2 (Table 2.2 in arXiv pre-print)      #
# Falsification: rdrandinf for all covariates #
#---------------------------------------------#
out = rdrandinf(data.presdemvoteshlag1, data.X, seed = 50,
                 wl = -0.7652, wr = 0.7652)
out = rdrandinf(data.demvoteshlag1, data.X, seed = 50,
                 wl = -0.7652, wr = 0.7652)
out = rdrandinf(data.demvoteshlag2, data.X, seed = 50,
                 wl = -0.7652, wr = 0.7652)
out = rdrandinf(data.demwinprv1, data.X, seed = 50,
                 wl = -0.7652, wr = 0.7652)
out = rdrandinf(data.demwinprv2, data.X, seed = 50,
                 wl = -0.7652, wr = 0.7652)
out = rdrandinf(data.dmidterm, data.X, seed = 50,
                 wl = -0.7652, wr = 0.7652)
out = rdrandinf(data.dpresdem, data.X, seed = 50,
                 wl = -0.7652, wr = 0.7652)
out = rdrandinf(data.dopen, data.X, seed = 50,
                 wl = -0.7652, wr = 0.7652)

#----------------------------------------------------------#
# Additional analysis (output not reported in publication) #
# Density test using rdrandinf                             #
#----------------------------------------------------------#
out = rdwinselect(data.X, wmin = 0.7652, nwindows = 1)

#----------------------------------------------------------#
# Additional analysis (output not reported in publication) #
# Binomial test by hand                                    #
#----------------------------------------------------------#
result = stats.binomtest(25, 41, p=0.5)
print(result.pvalue)

#----------------------------------------------------------#
# Additional analysis (output not reported in publication) #
# Placebo cutoff at c=1                                    #
#----------------------------------------------------------#
out = rdrandinf(data.Y, data.X, cutoff = 1, wl = 0.2348, wr = 1.7652, 
                seed = 50)

#---------------------------------------------#
# Table 3 (Table 2.3 in arXiv pre-print)      #
# Falsification analysis with placebo cutoffs #
#---------------------------------------------#
out = rdrandinf(data.Y, data.X, seed = 50, cutoff = -1, wl = -1.7652, 
                wr = -0.2348)
out = rdrandinf(data.Y, data.X, seed = 50, cutoff = 1, wl = 0.2348, 
                wr = 1.7652)

#--------------------------------------------#
# Snippet 7 (Snippet 2.7 in arXiv pre-print) #
# Sensitivity to window choice               #
#--------------------------------------------#
out = rdrandinf(data.Y, data.X, wl = -0.6934, wr = 0.6934, seed = 50)
