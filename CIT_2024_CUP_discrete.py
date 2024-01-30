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

#########################################################################
#########################################################################
#########################################################################
######### Section 4: RD Designs with Discrete Running Variables #########
#########################################################################
#########################################################################
#########################################################################

# Loading packages
from rdrobust import rdrobust, rdplot
from rdlocrand import rdrandinf, rdwinselect
from scipy import stats
import rddensity
import pandas as pd
import matplotlib.pyplot as plt

#------------------#
# Loading the data #
#------------------#
data = pd.read_csv("CIT_2024_CUP_discrete.csv")

#----------------------------------------------------#
# Figure 10 (Figure 4.1 in arXiv pre-print)          #
# Histogram and scatter plot of the running variable #
#----------------------------------------------------#
# Figure 10a (Figure 4.1a in arXiv pre-print): Histogram
plt.hist(data[data.X < 0].X, bins=25, edgecolor='black', alpha=0.7, color='blue')
plt.hist(data[data.X >= 0].X, bins=14, edgecolor='black', alpha=0.7, color='red')
plt.xlabel('Score')
plt.ylabel('Number of observations')
plt.title('')
plt.show()

# Figure 10b (Figure 4.1b in arXiv pre-print): Scatter plot
plt.scatter(data[data['X'].abs() <= 0.25]['X'], 
            data[data['X'].abs() <= 0.25]['nextGPA'],
            color='black',
            s=10
            )
plt.xlabel('Score')
plt.ylabel('Next Term GPA (normalized)')
plt.show()

#-------------------------------------------------------------------#
# Snippet 18 (Snippet 4.1 in arXiv pre-print)                       #
# Counting the number of observations with X different from missing #
#-------------------------------------------------------------------#
count_non_missing = data['X'].count()
print(count_non_missing)

#---------------------------------------------#
# Snippet 19 (Snippet 4.2 in arXiv pre-print) #
# Counting the unique values of X             #
#---------------------------------------------#
unique_value_count = data['X'].nunique()
print(unique_value_count)

#---------------------------------------------#
# Snippet 20 (Snippet 4.3 in arXiv pre-print) #
# Using rddensity                             #
#---------------------------------------------#
rddensity.rddensity(data.X, bino_flag = False)

#----------------------------------------------------------#
# Additional analysis (output not reported in publication) #
# Using rdrobust on a covariate                            #
#----------------------------------------------------------#
out = rdrobust(data.hsgrade_pct, data.X, bwselect = "cerrd")
print(out)

#----------------------------------------------------------#
# Additional analysis (output not reported in publication) #
# Using rdplot on a covariate                              #
#----------------------------------------------------------#
rdplot(data.hsgrade_pct, data.X)

#----------------------------------------#
# Table 7 (Table 4.2 in arXiv pre-print) #
# RD effects on predetermined covariates #
#----------------------------------------#
print(rdrobust(data.hsgrade_pct, data.X, bwselect = "cerrd"))
print(rdrobust(data.totcredits_year1, data.X, bwselect = "cerrd"))
print(rdrobust(data.age_at_entry, data.X, bwselect = "cerrd"))
print(rdrobust(data.male, data.X, bwselect = "cerrd"))
print(rdrobust(data.bpl_north_america, data.X, bwselect = "cerrd"))

#----------------------------------------------------------#
# Additional analysis (output not reported in publication) #
# Using rdplot on the outcome                              #
#----------------------------------------------------------#
out = rdplot(data.nextGPA, data.X, binselect = 'esmv', x_label = "Score", 
             y_label = "Outcome", title = "")
print(out)

#-------------------------------------------#
# Figure 11 (Figure 4.2 in arXiv pre-print) #
# rdplot for the outcome                    #
#-------------------------------------------#
rdplot(data.nextGPA, data.X, binselect = 'esmv', x_label = "Score", 
             y_label = "Outcome", title = "")

#---------------------------------------------#
# Snippet 21 (Snippet 4.4 in arXiv pre-print) #
# Using rdrobust on the outcome               #
#---------------------------------------------#
out = rdrobust(data.nextGPA, data.X, kernel = 'triangular',  p = 1, 
               bwselect = 'mserd')
print(out)

#---------------------------------------------#
# Snippet 22 (Snippet 4.5 in arXiv pre-print) #
# Using rdrobust and showing its outputs      #
#---------------------------------------------#
rdout = rdrobust(data.nextGPA, data.X, kernel = 'triangular', p = 1, 
                 bwselect = 'mserd')
dir(rdout)
print(rdout.beta_p_r)
print(rdout.beta_p_l)

#-----------------------------------------------#
# Snippet 23 (Snippet 4.6 in arXiv pre-print)   #
# Using rdrobust with clustered standard errors #
#-----------------------------------------------#
clustervar = data.X
out = rdrobust(data.nextGPA, data.X, vce = 'hc0', cluster = clustervar)
print(out)

#------------------------------------------------------#
# Snippet 24 (Snippet 4.7 in arXiv pre-print)          #
# Using rdrobust on the collapsed data (first outcome) #
#------------------------------------------------------#
data2 = pd.DataFrame({'nextGPA': data.nextGPA, 'X': data.X})
print(data2.shape)
collapsed = data2.groupby('X')['nextGPA'].mean().reset_index()
print(collapsed.shape)
out = rdrobust(collapsed.nextGPA, collapsed.X)
print(out)

#----------------------------------------------------------#
# Additional analysis (output not reported in publication) #
# Binomial test with rdwinselect                           #
#----------------------------------------------------------#
out = rdwinselect(data.X, wmin = 0.01, nwindows = 1, cutoff = 5.00000000000e-06)

#---------------------------------------------#
# Snippet 25 (Snippet 4.8 in arXiv pre-print) #
# Binomial test by hand                       #
#---------------------------------------------#
result = stats.binomtest(67, 275, 0.5)
print(result)

#----------------------------------------------------------#
# Additional analysis (output not reported in publication) #
# Using rdrandinf on a covariate                           #
#----------------------------------------------------------#
out = rdrandinf(data.hsgrade_pct, data.X, wl = -0.005, wr = 0.01, seed = 50)

#----------------------------------------#
# Table 8 (Table 4.3 in arXiv pre-print) #
# RD effects on predetermined covariates #
#----------------------------------------#
out = rdrandinf(data.hsgrade_pct, data.X, wl = -0.005, wr = 0.01, seed = 50)

out = rdrandinf(data.totcredits_year1, data.X, wl = -0.005, wr = 0.01, seed = 50)

out = rdrandinf(data.age_at_entry, data.X, wl = -0.005, wr = 0.01, seed = 50)

out = rdrandinf(data.male, data.X, wl = -0.005, wr = 0.01, seed = 50)

out = rdrandinf(data.bpl_north_america, data.X, wl = -0.005, wr = 0.01, seed = 50)

#---------------------------------------------------------------#
# Snippet 26 (Snippet 4.9 in arXiv pre-print)                   #
# Using rdwinselect with covariates to determine optimal window #
#---------------------------------------------------------------#
selected_variables =['hsgrade_pct', 'totcredits_year1', 'age_at_entry', 
                     'male', 'bpl_north_america']
Z = data[selected_variables]
out = rdwinselect(data.X, Z, seed = 50, wmin = 0.01, wstep = 0.01, 
                  cutoff = 5.00000000000e-06, level = 0.135)

#----------------------------------------------#
# Snippet 27 (Snippet 4.10 in arXiv pre-print) #
# Using rdrandinf on the Outcome               #
#----------------------------------------------#
out = rdrandinf(data.nextGPA, data.X, wl = -0.005, wr = 0.01, seed = 50)

#----------------------------------------------------------#
# Additional analysis (output not reported in publication) #
# rdplots for predetermined covariates                     #
#----------------------------------------------------------#
rdplot(data.hsgrade_pct, data.X, x_label = "Score", y_label = "", title="")

rdplot(data.totcredits_year1, data.X, x_label = "Score", y_label = "", title="")

rdplot(data.age_at_entry, data.X, x_label = "Score", y_label = "", title="")

rdplot(data.male, data.X, x_label = "Score", y_label = "", title="")

rdplot(data.bpl_north_america, data.X, x_label = "Score", y_label = "", title="")