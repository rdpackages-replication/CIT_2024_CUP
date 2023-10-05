**----------------------------------------------------------------------------**
** A Practical Introduction to Regression Discontinuity Designs: Extensions
** Authors: Matias D. Cattaneo, Nicolás Idrobo and Rocío Titiunik
** Last update: 2023-10-05
**----------------------------------------------------------------------------**
** SOFTWARE WEBSITE: https://rdpackages.github.io/
**----------------------------------------------------------------------------**
** TO INSTALL STATA PACKAGES:
** net install rdrobust, ///
**	from("https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata") replace
**
** net install rdlocrand, ///
**	from("https://raw.githubusercontent.com/rdpackages/rdlocrand/master/stata") replace
**
** net install rddensity, ///
**	from("https://raw.githubusercontent.com/rdpackages/rddensity/master/stata") replace
**
** net install rdmulti, ///
**	from("https://raw.githubusercontent.com/rdpackages/rdmulti/master/stata") replace
**
** net install rdpower, ///
**	from("https://raw.githubusercontent.com/rdpackages/rdpower/master/stata") replace
**
** net install lpdensity, ///
**		from("https://raw.githubusercontent.com/nppackages/lpdensity/master/stata") replace
**
** ssc install mmerge
**----------------------------------------------------------------------------**

clear
clear all
clear matrix
cap log close
set more off
set matsize 11000

**************************************************
**************************************************
**************************************************
********* Section 3: The Fuzzy RD Design *********
**************************************************
**************************************************
**************************************************

**------------------**
** Loading the data **
**------------------**
use "CIT_2023_CUP_fuzzy.dta", clear

* Defining list of covariates
# delimit ;
	global covariates "icfes_female icfes_age icfes_urm icfes_stratum
		icfes_famsize";
# delimit cr

**------------------------**
** Table                  **
** Descriptive statistics **
**------------------------**
global sumstats "X1 T D Y $covariates"
foreach x of global sumstats {
	summarize `x', detail
}

**---------------------------**
** Figure 3.2                **
** rdplot of the first stage **
**---------------------------**
rdplot D X1

**-----------------------------**
** Snippet 3.1                 **
** rdrobust of the first stage **
**-----------------------------**
rdrobust D X1

**------------------------------**
** Snippet 3.2                  **
** rdrobust of the reduced form **
**------------------------------**
rdrobust Y X1

**----------------------------**
** Figure 3.3                 **
** rdplot of the reduced form **
**----------------------------**
rdplot Y X1, p(3)

**------------------------**
** Snippet 3.3            **
** Fuzzy RD with rdrobust **
**------------------------**
rdrobust Y X1, fuzzy(D)

**----------------------------------------------------**
** Snippet 3.4                                        **
** Selecting a window with rdwinselect and covariates **
**----------------------------------------------------**
# delimit ;
	global covariates "icfes_female icfes_age icfes_urm icfes_stratum
		icfes_famsize";
# delimit cr

rdwinselect X1 $covariates

**----------------------------**
** Snippet 3.5                **
** First stage with rdrandinf **
**----------------------------**
rdrandinf D X1, wl(-0.13000107) wr(0.13000107)

**-----------------------------**
** Snippet 3.6                 **
** Reduced form with rdrandinf **
**-----------------------------**
rdrandinf Y X1, wl(-0.13000107) wr(0.13000107)

**-------------------------**
** Snippet 3.7             **
** Fuzzy RD with rdrandinf **
**-------------------------**
rdrandinf Y X1, wl(-0.13000107) wr(0.13000107) fuzzy(D tsls)

**----------------------------------**
** Snippet 3.8                      **
** Manipulation test with rddensity **
**----------------------------------**
rddensity X1, bino_w(0.13000107) bino_nw(1)

**--------------------------------------------**
** Snippet 3.9                                **
** Reduced form on a covariate using rdrobust **
**--------------------------------------------**
rdrobust icfes_female X1, bwselect(cerrd)

**---------------------------------------------**
** Snippet 3.10                                **
** Reduced form on a covariate using rdrandinf **
**---------------------------------------------**
rdrandinf icfes_female X1, wl(-0.13000107) wr(0.13000107)

**-------------------------------------------------------**
** Table 3.1                                             **
** Reduced form regressions on covariates using rdrobust **
**-------------------------------------------------------**
foreach y of global covariates {
	rdrobust `y' X, all bwselect(cerrd)
}

**--------------------------------------------------------**
** Table 3.2                                              **
** Reduced form regressions on covariates using rdrandinf **
**--------------------------------------------------------**
local window = 0.13000107

foreach y of global covariates {
	ttest `y' if abs(X) <= `window', by(T)
	
	rdrandinf `y' X, wl(-`window') wr(`window')
}

*------------------------------------------------------------------------------*
clear all
