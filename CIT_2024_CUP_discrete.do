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

*************************************************************************
*************************************************************************
*************************************************************************
********* Section 4: RD Designs with Discrete Running Variables *********
*************************************************************************
*************************************************************************
*************************************************************************

**------------------**
** Loading the data **
**------------------**
use "CIT_2024_CUP_discrete.dta", clear

**------------------------------------------------------------------**
** Additional analysis (output not reported in publication)         **
** Descriptive statistics for Lindo, Sanders, and Oreopoulos (2010) **
**------------------------------------------------------------------**
# delimit ;
	global vars "nextGPA X T 
	hsgrade_pct totcredits_year1 age_at_entry male bpl_north_america";
# delimit cr

foreach x of global vars {
	sum `x', detail
}

**----------------------------------------------------**
** Figure 10 (Figure 4.1 in arXiv pre-print)          **
** Histogram and scatter plot of the running variable **
**----------------------------------------------------**
** Figure 10a (Figure 4.1a in arXiv pre-print): Histogram
twoway (histogram X if X<0, width(0.1) freq color(blue) xline(0)) ///
	(histogram X if X>=0, width(0.1) freq color(red)), ///
	graphregion(color(white)) legend(off) ///
	xtitle(Score) ytitle(Number of Observations)

** Figure 10b (Figure 4.1b in arXiv pre-print): Scatter plot
twoway (scatter nextGPA X if abs(X)<=0.25, mcolor(black) msize(vsmall)), ///
	graphregion(color(white)) ylabel(,angle(0) nogrid) xlabel(-0.25(0.05)0.25) ///
	xtitle("Running Variable")

**-------------------------------------------------------------------**
** Snippet 18 (Snippet 4.1 in arXiv pre-print)                       **
** Counting the number of observations with X different from missing **
**-------------------------------------------------------------------**
count if X != .

**---------------------------------------------**
** Snippet 19 (Snippet 4.2 in arXiv pre-print) **
** Counting the unique values of X             **
**---------------------------------------------**
codebook X

**----------------------------------------**
** Table 6 (Table 4.1 in arXiv pre-print) **
** Observations at closest mass points    **
**----------------------------------------**
forvalues k=1/3 {
	preserve
		gen obs=1
		collapse (sum) obs, by(X T loc_campus`k')
		reshape wide obs, i(X T) j(loc_campus`k')
		drop obs0
		rename obs1 obs1_campus`k'
		keep if abs(X)<=0.03
		drop if X==0.03
	restore
}

**---------------------------------------------**
** Snippet 20 (Snippet 4.3 in arXiv pre-print) **
** Using rddensity                             **
**---------------------------------------------**
rddensity X, nobinomial

**----------------------------------------------------------**
** Additional analysis (output not reported in publication) **
** Using rdrobust on a covariate                            **
**----------------------------------------------------------**
rdrobust hsgrade_pct X, bwselect(cerrd)

**----------------------------------------------------------**
** Additional analysis (output not reported in publication) **
** Using rdplot on a covariate                              **
**----------------------------------------------------------**
rdplot hsgrade_pct X 

**----------------------------------------**
** Table 7 (Table 4.2 in arXiv pre-print) **
** RD effects on predetermined covariates **
**----------------------------------------**
# delimit ;
	global covariates "hsgrade_pct totcredits_year1 age_at_entry male 
		bpl_north_america";
# delimit cr

foreach y of global covariates {
	rdrobust `y' X, all bwselect(cerrd)
}

**----------------------------------------------------------**
** Additional analysis (output not reported in publication) **
** Using rdplot on the outcome                              **
**----------------------------------------------------------**
rdplot nextGPA X, binselect(esmv) ///
	graph_options(graphregion(color(white)) ///
	xtitle(Score) ytitle(Outcome))

**-------------------------------------------**
** Figure 11 (Figure 4.2 in arXiv pre-print) **
** rdplot for the outcome                    **
**-------------------------------------------**
rdplot nextGPA X

**---------------------------------------------**
** Snippet 21 (Snippet 4.4 in arXiv pre-print) **
** Using rdrobust on the outcome               **
**---------------------------------------------**
rdrobust nextGPA X, kernel(triangular) p(1) bwselect(mserd)  

**---------------------------------------------**
** Snippet 22 (Snippet 4.5 in arXiv pre-print) **
** Using rdrobust and showing its outputs      **
**---------------------------------------------**
rdrobust nextGPA X
ereturn list

**-----------------------------------------------**
** Snippet 23 (Snippet 4.6 in arXiv pre-print)   **
** Using rdrobust with clustered standard errors **
**-----------------------------------------------**
cap drop clustervar
gen clustervar=X
rdrobust nextGPA X, kernel(triangular) p(1) bwselect(mserd) ///
	vce(cluster clustervar)

**------------------------------------------------------**
** Snippet 24 (Snippet 4.7 in arXiv pre-print)          **
** Using rdrobust on the collapsed data (first outcome) **
**------------------------------------------------------**
preserve
	collapse (mean) nextGPA, by(X)
	rdrobust nextGPA X
restore

**----------------------------------------------------------**
** Additional analysis (output not reported in publication) **
** Binomial test with rdwinselect                           **
**----------------------------------------------------------**
rdwinselect X, wmin(0.01) nwindows(1) cutoff(0.000005)

**---------------------------------------------**
** Snippet 25 (Snippet 4.8 in arXiv pre-print) **
** Binomial test by hand                       **
**---------------------------------------------**
bitesti 275 67 1/2

**----------------------------------------------------------**
** Additional analysis (output not reported in publication) **
** Using rdrandinf on a covariate                           **
**----------------------------------------------------------**
rdrandinf hsgrade_pct X, seed(50) wl(-.005) wr(.01)

**----------------------------------------**
** Table 8 (Table 4.3 in arXiv pre-print) **
** RD effects on predetermined covariates **
**----------------------------------------**
# delimit ;
	global covariates "hsgrade_pct totcredits_year1 age_at_entry male 
		bpl_north_america";
# delimit cr

foreach y of global covariates {
	rdrandinf `y' X, wl(-0.005) wr(0.01) seed(50)
}

**---------------------------------------------------------------**
** Snippet 26 (Snippet 4.9 in arXiv pre-print)                   **
** Using rdwinselect with covariates to determine optimal window **
**---------------------------------------------------------------**
# delimit ;
	global covariates "hsgrade_pct totcredits_year1 age_at_entry male 
		bpl_north_america";
# delimit cr

rdwinselect X $covariates, cutoff(0.00005) wmin(0.01) wstep(0.01) ///
	seed(50) level(0.135)

**----------------------------------------------**
** Snippet 27 (Snippet 4.10 in arXiv pre-print) **
** Using rdrandinf on the outcome               **
**----------------------------------------------**
rdrandinf nextGPA X, seed(50) wl(-0.005) wr(0.01) 

**----------------------------------------------------------**
** Additional analysis (output not reported in publication) **
** rdplots for predetermined covariates                     **
**----------------------------------------------------------**
foreach y of global covariates {
	rdplot `y' X
}

*------------------------------------------------------------------------------*
clear all
