**----------------------------------------------------------------------------**
** A Practical Introduction to Regression Discontinuity Designs: Extensions
** Authors: Matias D. Cattaneo, Nicolás Idrobo and Rocío Titiunik
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
use "CIT_2024_CUP_fuzzy.dta", clear

* Defining list of covariates
# delimit ;
	global covariates "icfes_female icfes_age icfes_urm icfes_stratum
		icfes_famsize";
# delimit cr

**----------------------------------------------------------**
** Additional analysis (output not reported in publication) **
** Descriptive statistics                                   **
**----------------------------------------------------------**
global sumstats "X1 T D Y $covariates"
foreach x of global sumstats {
	summarize `x', detail
}

**------------------------------------------**
** Figure 8 (Figure 3.2 in arXiv pre-print) **
** rdplot of the first stage                **
**------------------------------------------**
rdplot D X1

**--------------------------------------------**
** Snippet 8 (Snippet 3.1 in arXiv pre-print) **
** rdrobust of the first stage                **
**--------------------------------------------**
rdrobust D X1

**--------------------------------------------**
** Snippet 9 (Snippet 3.2 in arXiv pre-print) **
** rdrobust of the reduced form               **
**--------------------------------------------**
rdrobust Y X1

**------------------------------------------**
** Figure 9 (Figure 3.3 in arXiv pre-print) **
** rdplot of the reduced form               **
**------------------------------------------**
rdplot Y X1, p(3)

**---------------------------------------------**
** Snippet 10 (Snippet 3.3 in arXiv pre-print) **
** Fuzzy RD with rdrobust                      **
**---------------------------------------------**
rdrobust Y X1, fuzzy(D)

**----------------------------------------------------**
** Snippet 11 (Snippet 3.4 in arXiv pre-print)        **
** Selecting a window with rdwinselect and covariates **
**----------------------------------------------------**
# delimit ;
	global covariates "icfes_female icfes_age icfes_urm icfes_stratum
		icfes_famsize";
# delimit cr

rdwinselect X1 $covariates

**---------------------------------------------**
** Snippet 12 (Snippet 3.5 in arXiv pre-print) **
** First stage with rdrandinf                  **
**---------------------------------------------**
rdrandinf D X1, wl(-0.13000107) wr(0.13000107)

**---------------------------------------------**
** Snippet 13 (Snippet 3.6 in arXiv pre-print) **
** Reduced form with rdrandinf                 **
**---------------------------------------------**
rdrandinf Y X1, wl(-0.13000107) wr(0.13000107)

**---------------------------------------------**
** Snippet 14 (Snippet 3.7 in arXiv pre-print) **
** Fuzzy RD with rdrandinf                     **
**---------------------------------------------**
rdrandinf Y X1, wl(-0.13000107) wr(0.13000107) fuzzy(D tsls)

**---------------------------------------------**
** Snippet 15 (Snippet 3.8 in arXiv pre-print) **
** Manipulation test with rddensity            **
**---------------------------------------------**
rddensity X1, bino_w(0.13000107) bino_nw(1)

**---------------------------------------------**
** Snippet 16 (Snippet 3.9 in arXiv pre-print) **
** Reduced form on a covariate using rdrobust  **
**---------------------------------------------**
rdrobust icfes_female X1, bwselect(cerrd)

**----------------------------------------------**
** Snippet 17 (Snippet 3.10 in arXiv pre-print) **
** Reduced form on a covariate using rdrandinf  **
**----------------------------------------------**
rdrandinf icfes_female X1, wl(-0.13000107) wr(0.13000107)

**-------------------------------------------------------**
** Table 4 (Table 3.1 in arXiv pre-print)                **
** Reduced form regressions on covariates using rdrobust **
**-------------------------------------------------------**
foreach y of global covariates {
	rdrobust `y' X, all bwselect(cerrd)
}

**--------------------------------------------------------**
** Table 5 (Table 3.2 in arXiv pre-print)                 **
** Reduced form regressions on covariates using rdrandinf **
**--------------------------------------------------------**
local window = 0.13000107

foreach y of global covariates {
	ttest `y' if abs(X) <= `window', by(T)
	
	rdrandinf `y' X, wl(-`window') wr(`window')
}

*------------------------------------------------------------------------------*
clear all
