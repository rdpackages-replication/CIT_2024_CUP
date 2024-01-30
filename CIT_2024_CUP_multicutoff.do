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

***********************************************************
***********************************************************
***********************************************************
********* Section 5: Multi-Dimensional RD Designs *********
********* Non-Geographic Empirical Application    *********
***********************************************************
***********************************************************
***********************************************************

**------------------**
** Loading the data **
**------------------**
use "CIT_2024_CUP_multicutoff.dta", clear

**-------------------------------------------**
** Figure 15 (Figure 5.4 in arXiv pre-print) **
** Panel a: rdplot on one cutoff             **
** Panel b: rdmcplot on the three cutoffs    **
**-------------------------------------------**
* Panel a:
rdplot spadies_any sisben_score if cutoff == -57.21, c(-57.21) p(1)

* Panel b:
* Calling rdplot on each cutoff and extracting the optimal number of bins
rdplot spadies_any sisben_score if cutoff == -57.21, c(-57.21) p(1) hide
gen nb_l=ceil(e(J_star_l)/2) if _n==1
gen nb_r=ceil(e(J_star_r)/2) if _n==1

rdplot spadies_any sisben_score if cutoff == -56.32, c(-56.32) p(1) hide
replace nb_l=ceil(e(J_star_l)/2) if _n==2
replace nb_r=ceil(e(J_star_r)/2) if _n==2

rdplot spadies_any sisben_score if cutoff == -40.75, c(-40.75) p(1) hide
replace nb_l=ceil(e(J_star_l)/2) if _n==3
replace nb_r=ceil(e(J_star_r)/2) if _n==3

* Defining the polynomial degree and bin selection method for each cutoff
gen p = 1 if _n <= 3
gen b = "esmv" if _n <=3

* Calling rdmcplot and using the number of bins defined above
rdmcplot spadies_any sisben_score, ///
	c(cutoff) pvar(p) binselectvar(b) nbinsvar(nb_l) nbinsrightvar(nb_r) ///
	genvars

drop p b nb*

* The rest of the code in this figure illustrates how to create the
* plot by hand, using the outputs from rdmcplot. This is useful in case
* the user wants to customize the plot even further.

twoway (scatter rdmcplot_mean_y_1 rdmcplot_mean_x_1, mcolor(blue) msize(small)) ///
	(line rdmcplot_hat_y_1 rdmcplot_mean_x_1 if ///
		cutoff==-57.21 & sisben_score<-57.21, sort lcolor(blue)) ///
	(line rdmcplot_hat_y_1 rdmcplot_mean_x_1 if ///
		cutoff==-57.21 & sisben_score>=-57.21, sort lcolor(blue)) ///
	(scatter rdmcplot_mean_y_2 rdmcplot_mean_x_2, msymbol(Sh) mcolor(cranberry) msize(small)) ///
	(line rdmcplot_hat_y_2 rdmcplot_mean_x_2 if ///
		cutoff==-56.32 & sisben_score<-56.32, sort lcolor(cranberry)) ///
	(line rdmcplot_hat_y_2 rdmcplot_mean_x_2 if ///
		cutoff==-56.32 & sisben_score>=-56.32, sort lcolor(cranberry)) ///
	(scatter rdmcplot_mean_y_3 rdmcplot_mean_x_3, msymbol(Th) mcolor(dkgreen) msize(small)) ///
	(line rdmcplot_hat_y_3 rdmcplot_mean_x_3 if ///
		cutoff==-40.75 & sisben_score<-40.75, sort lcolor(dkgreen)) ///
	(line rdmcplot_hat_y_3 rdmcplot_mean_x_3 if ///
		cutoff==-40.75 & sisben_score>=-40.75, sort lcolor(dkgreen)), ///
	legend(off) graphregion(color(white)) ylabel(,angle(0) nogrid) xtitle("Running Variable") ///
	xline(-57.21, lcolor(blue) lpattern(dash)) ///
	xline(-56.32, lcolor(cranberry) lpattern(dash)) ///
	xline(-40.75, lcolor(dkgreen) lpattern(dash))

drop rdmcplot*

**---------------------------------------------**
** Snippet 28 (Snippet 5.1 in arXiv pre-print) **
** rdrobust using cutoff 1                     **
**---------------------------------------------**
rdrobust spadies_any sisben_score if cutoff == -57.21, c(-57.21)

**---------------------------------------------**
** Snippet 29 (Snippet 5.2 in arXiv pre-print) **
** Using rdmc and the three cutoffs            **
**---------------------------------------------**
rdmc spadies_any sisben_score, c(cutoff)

**---------------------------------------------**
** Snippet 30 (Snippet 5.3 in arXiv pre-print) **
** Using rdrobust with a normalized score      **
**---------------------------------------------**
gen double xnorm = .
replace xnorm = sisben_score + 57.21 if sisben_area == 1
replace xnorm = sisben_score + 56.32 if sisben_area == 2
replace xnorm = sisben_score + 40.75 if sisben_area == 3
rdrobust spadies_any xnorm, c(0)

**---------------------------------------------**
** Snippet 31 (Snippet 5.4 in arXiv pre-print) **
** Using rdmc and understanding its outputs    **
**---------------------------------------------**
rdmc spadies_any sisben_score, c(cutoff) 
ereturn list
mat list e(coefs)
mat list e(weights)
display e(coefs)[1,1] * e(weights)[1,1] + ///
	e(coefs)[1,2] * e(weights)[1,2] + ///
	e(coefs)[1,3] * e(weights)[1,3]

**--------------------------------------------------------------------**
** Snippet 32 (Snippet 5.5 in arXiv pre-print)                        **
** Formally testing the difference between the effects at the cutoffs **
**--------------------------------------------------------------------**
rdmc spadies_any sisben_score, c(cutoff) 
matrix b = e(b)
matrix V = e(V)
local dif = b[1,1] - b[1,2]
local dif_se = sqrt( V[1,1] + V[2,2] )
local zstat = `dif' / `dif_se'
local pval = 2 * normal( -abs(`zstat') )

*------------------------------------------------------------------------------*
clear all
