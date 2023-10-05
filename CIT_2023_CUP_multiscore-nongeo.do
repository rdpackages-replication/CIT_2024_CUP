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

***********************************************************
***********************************************************
***********************************************************
********* Section 5: Multi-Dimensional RD Designs *********
********* Non-Geographic Empirical Application    *********
***********************************************************
***********************************************************
***********************************************************

**-------------------**
** Loading the data **
**-------------------**
use "CIT_2023_CUP_multiscore-nongeo.dta", clear

**-----------------------------------------------------**
** Figure 5.6                                          **
** Example of treatment assignment in bidimensional RD **
**-----------------------------------------------------**
preserve
	sample 20

	generate xaux = 0 in 1/60
	generate yaux = _n in 1/60
	twoway (scatter running_sisben running_saber11 if tr ==0, ///
				msize(tiny) mfcolor(white) msymbol(X) ytitle("Sisben score") ///
			xtitle("Saber 11 score") mcolor(ebblue%60)) ///
	       (scatter running_sisben running_saber11 if tr ==1, ///
	       		msize(tiny) mfcolor(white) msymbol(T) mcolor(cranberry%60)) ///
			(function y = 0, range(0 200) lcolor(black) lwidth(thick))   ///
			(line yaux xaux, lcolor(black) lwidth(thick)) ///
			(scatteri 0 0, msize(large) mcolor(black)) ///
			(scatteri 30 0, msize(large) mcolor(black)) ///
			(scatteri 0 50, msize(large) mcolor(black)), ///
			text(20 120 "Treated", size(vlarge)) text(0 -200 "Control", ///
			size(vlarge)) legend(off) graphregion(color(white)) ///
			ylabel(,angle(0) nogrid) xlabel(,nogrid) ///
			text(-5 2.5 "b{subscript:1}" ///
				-5 50 "b{subscript:3}" ///
				30 -15 "b{subscript:2}", size(large))
restore

**-------------------------------------**
** Snippet 5.6                         **
** Using rdms on three boundary points **
**-------------------------------------**
gen p1 = .
gen p2 = .
replace p1 = 0  in 1
replace p2 = 0  in 1
replace p1 = 30 in 2
replace p2 = 0  in 2
replace p1 = 0  in 3
replace p2 = 50 in 3
list p1 p2 in 1/3
rdms spadies_any running_sisben running_saber11 tr, cvar(p1 p2)

**---------------------------------------------**
** Snippet 5.7                                 **
** Using rdrobust to illustrate what rdms does **
**---------------------------------------------**
global pdim1 = 30
global pdim2 = 0
gen double dist = sqrt( (running_sisben - $pdim1)^2 + ///
	(running_saber11 - $pdim2)^2 )
replace dist = dist*(2*tr-1)
rdrobust spadies_any dist

**--------------------------------------------------------------**
** Snippet 5.8                                                  **
** Creating the perpendicular distance to the boundary (step 1) **
**--------------------------------------------------------------**
gen aux1 = abs(running_sisben)
gen aux2 = abs(running_saber11)
egen double xnorm = rowmin(aux1 aux2) if ///
	running_sisben >= 0 & running_saber11 >= 0
replace xnorm = aux1 if ///
	running_sisben <= 0 & running_saber11 >= 0
replace xnorm = aux2 if ///
	running_sisben >= 0 & running_saber11 <= 0
replace xnorm = sqrt(running_sisben^2 + running_saber11^2) if ///
	running_sisben<=0  & running_saber11<=0

**--------------------------------------------------------------**
** Snippet 5.9                                                  **
** Creating the perpendicular distance to the boundary (step 2) **
**--------------------------------------------------------------**
replace xnorm = xnorm*(2*tr-1) 
replace xnorm = . if running_sisben == . | running_saber11 == .

**-------------------------------------------**
** Snippet 5.10                              **
** rdrobust using the perpendicular distance **
**-------------------------------------------**
rdrobust spadies_any xnorm

**---------------------------------------**
** Snippet 5.11                          **
** rdms using the perpendicular distance **
**---------------------------------------**
rdms spadies_any running_sisben running_saber11 tr, cvar(p1 p2) xnorm(xnorm)

*------------------------------------------------------------------------------*
clear all
