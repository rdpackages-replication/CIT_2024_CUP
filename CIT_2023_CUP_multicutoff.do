**----------------------------------------------------------------------------**
** A Practical Introduction to Regression Discontinuity Designs: Extensions
** Authors: Matias D. Cattaneo, Nicolás Idrobo and Rocío Titiunik
** Last update: 2023-01-21
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
use "CIT_2023_CUP_multicutoff.dta", clear

**----------------------------------------**
** Figure 5.4 (no output)                 **
** Panel a: rdplot on one cutoff          **
** Panel b: rdmcplot on the three cutoffs **
**----------------------------------------**
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

* The rest of the code in this Figure 5.4b illustrates how to create the
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

**-------------------------**
** Snippet 5.1             **
** rdrobust using cutoff 1 **
**-------------------------**
sjlog using "outputs/Vol-2-STATA_LRS_rdrobust_cutoff1", replace
	rdrobust spadies_any sisben_score if cutoff == -57.21, c(-57.21)
sjlog close, replace logfile smclfile

**----------------------------------**
** Snippet 5.2                      **
** Using rdmc and the three cutoffs **
**----------------------------------**
sjlog using "outputs/Vol-2-STATA_LRS_rdmc", replace
	rdmc spadies_any sisben_score, c(cutoff)
sjlog close, replace logfile smclfile

**----------------------------------------**
** Snippet 5.3                            **
** Using rdrobust with a normalized score **
**----------------------------------------**
sjlog using "outputs/Vol-2-STATA_LRS_rdrobust_pooled_xnorm", replace
	gen double xnorm = .
	replace xnorm = sisben_score + 57.21 if sisben_area == 1
	replace xnorm = sisben_score + 56.32 if sisben_area == 2
	replace xnorm = sisben_score + 40.75 if sisben_area == 3
	rdrobust spadies_any xnorm, c(0)
sjlog close, replace logfile smclfile

**------------------------------------------**
** Snippet 5.4                              **
** Using rdmc and understanding its outputs **
**------------------------------------------**
sjlog using "outputs/Vol-2-STATA_LRS_rdmc_row_weightedresults", replace
	rdmc spadies_any sisben_score, c(cutoff) 
	ereturn list
	mat list e(coefs)
	mat list e(weights)
	display e(coefs)[1,1] * e(weights)[1,1] + ///
		e(coefs)[1,2] * e(weights)[1,2] + ///
		e(coefs)[1,3] * e(weights)[1,3]
sjlog close, replace logfile smclfile

**--------------------------------------------------------------------**
** Snippet 5.5                                                        **
** Formally testing the difference between the effects at the cutoffs **
**--------------------------------------------------------------------**
sjlog using "outputs/Vol-2-STATA_LRS_rdmc_comparing_effects", replace
	rdmc spadies_any sisben_score, c(cutoff) 
	matrix b = e(b)
	matrix V = e(V)
	local dif = b[1,1] - b[1,2]
	local dif_se = sqrt( V[1,1] + V[2,2] )
	local zstat = `dif' / `dif_se'
	local pval = 2 * normal( -abs(`zstat') )
sjlog close, replace logfile smclfile

*------------------------------------------------------------------------------*
clear all
