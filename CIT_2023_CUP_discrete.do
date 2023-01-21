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
use "CIT_2023_CUP_discrete.dta", clear

**------------------------------------------------------------------**
** Table 4.1                                                        **
** Descriptive statistics for Lindo, Sanders, and Oreopoulos (2010) **
**------------------------------------------------------------------**
# delimit ;
	global vars "nextGPA X T 
	hsgrade_pct totcredits_year1 age_at_entry male bpl_north_america";
# delimit cr

matrix define R=J(8,6,.)
local k=1
foreach x of global vars {
	quietly sum `x', detail
	local label_`k': variable label `x'
	local varname_`k'=" (`x')"
	matrix R[`k',1]=r(mean)
	matrix R[`k',2]=r(p50)
	matrix R[`k',3]=r(sd)
	matrix R[`k',4]=r(min)
	matrix R[`k',5]=r(max)
	matrix R[`k',6]=r(N)
	
	local k=`k'+1
}

preserve
	clear
	local t=`k'-1
	svmat R
	gen R0=""
	forvalues k=1/`t' {
		replace R0="`label_`k''"+"`varname_`k''" if _n==`k'
	}
	order R0
	save "outputs/Vol-2-descriptive_stats_block1.dta", replace
restore

**----------------------------------------------------**
** Figure 4.1                                         **
** Histogram and scatter plot of the running variable **
**----------------------------------------------------**
** Figure 4.1a: Histogram
twoway (histogram X if X<0, width(0.1) freq color(blue) xline(0)) ///
	(histogram X if X>=0, width(0.1) freq color(red)), ///
	graphregion(color(white)) legend(off) ///
	xtitle(Score) ytitle(Number of Observations)

** Figure 4.1b: Scatter plot
twoway (scatter nextGPA X if abs(X)<=0.25, mcolor(black) msize(vsmall)), ///
	graphregion(color(white)) ylabel(,angle(0) nogrid) xlabel(-0.25(0.05)0.25) ///
	xtitle("Running Variable")

**-------------------------------------------------------------------**
** Snippet 4.1                                                       **
** Counting the number of observations with X different from missing **
**-------------------------------------------------------------------**
sjlog using "outputs/Vol-2-STATA_lso_countX", replace
	count if X != .
sjlog close, replace logfile smclfile

**---------------------------------**
** Snippet 4.2                     **
** Counting the unique values of X **
**---------------------------------**
sjlog using "outputs/Vol-2-STATA_lso_uniqueX", replace
	codebook X
sjlog close, replace logfile smclfile

**-------------------------------------**
** Table 4.2                           **
** Observations at closest mass points **
**-------------------------------------**
forvalues k=1/3 {
	preserve
		gen obs=1
		collapse (sum) obs, by(X T loc_campus`k')
		reshape wide obs, i(X T) j(loc_campus`k')
		drop obs0
		rename obs1 obs1_campus`k'
		keep if abs(X)<=0.03
		drop if X==0.03
		save "temp`k'.dta", replace
	restore
}

preserve
	gen obs=1
	collapse (sum) obs, by(X T)
	keep if abs(X)<=0.03
	drop if X==0.03

	forvalues k=1/3 {
		mmerge X T using "temp`k'.dta"
		drop _merge
		erase "temp`k'.dta"
	}
	
	tostring X, force gen(Xstr) format(%12.2f)
	replace Xstr="-0.000005" if Xstr=="-0.00"

	gen X_old=X*(-1)
	tostring X_old, force gen(X_oldstr) format(%12.2f)
	drop X X_old

	order X_oldstr Xstr

	drop if _n==1
	
	gen treatment="Control" if T==0
	replace treatment="Treated" if T==1
	drop T
	order X_oldstr Xstr treatment
	save "outputs/Vol-2-STATA_obs_near_cutoff.dta", replace
restore

**-----------------**
** Snippet 4.3     **
** Using rddensity **
**-----------------**
sjlog using "outputs/Vol-2-STATA_lso_falsification_rddensity", replace
	rddensity X
sjlog close, replace logfile smclfile

**-------------------------------**
** Snippet 4.4                   **
** Using rdrobust on a covariate **
**-------------------------------**
sjlog using "outputs/Vol-2-STATA_lso_falsification_rdrobust_hsgrade_pct", replace
	rdrobust hsgrade_pct X, bwselect(cerrd)
sjlog close, replace logfile smclfile

**-----------------------------**
** Snippet 4.5                 **
** Using rdplot on a covariate **
**-----------------------------**
sjlog using "outputs/Vol-2-STATA_lso_falsification_rdplot_hsgrade_pct", replace
	rdplot hsgrade_pct X 
sjlog close, replace logfile smclfile

**----------------------------------------**
** Table 4.3                              **
** RD effects on predetermined covariates **
**----------------------------------------**
# delimit ;
	global covariates "hsgrade_pct totcredits_year1 age_at_entry male 
		bpl_north_america";
# delimit cr
matrix define R=J(5,8,.)
local k=1
foreach y of global covariates {
	rdrobust `y' X, all bwselect(cerrd)
	local label_`k': variable label `y'
	matrix R[`k',1]=e(h_l)
	matrix R[`k',2]=e(tau_cl)
	matrix R[`k',3]=e(tau_bc)
	matrix R[`k',4]=e(se_tau_rb)
	matrix R[`k',5]=2*normal(-abs(R[`k',3]/R[`k',4]))
	matrix R[`k',6]=R[`k',3]-invnormal(0.975)*R[`k',4]
	matrix R[`k',7]=R[`k',3]+invnormal(0.975)*R[`k',4]
	matrix R[`k',8]=e(N_h_l)+e(N_h_r)
	
	local k=`k'+1
}

preserve
	clear
	local t=`k'-1
	svmat R
	gen R0=""
	forvalues k=1/`t' {
		replace R0="`label_`k''" if _n==`k'
	}
	order R0
	save "outputs/Vol-2-STATA_lso_falsification_rdrobust_allcovariates.dta", replace
restore

**-----------------------------**
** Code snippet 4.6            **
** Using rdplot on the outcome **
**-----------------------------**
sjlog using "outputs/Vol-2-STATA_lso3_rdplot_esmv", replace
	rdplot nextGPA X, binselect(esmv) ///
		graph_options(graphregion(color(white)) ///
		xtitle(Score) ytitle(Outcome))
sjlog close, replace logfile smclfile

**-------------------------------**
** Snippet 4.7                   **
** Using rdrobust on the outcome **
**-------------------------------**
sjlog using "outputs/Vol-2-STATA_lso3_rdrobust_triangular_mserd_p1_rhofree_regterm1", replace
	rdrobust nextGPA X, kernel(triangular) p(1) bwselect(mserd)  
sjlog close, replace logfile smclfile

**----------------------------------------**
** Snippet 4.8                            **
** Using rdrobust and showing its outputs **
**----------------------------------------**
sjlog using "outputs/Vol-2-STATA_lso3_rdrobust_triangular_mserd_p1_rhofree_regterm1_namescoefsout_all", replace
	rdrobust nextGPA X
	ereturn list
sjlog close, replace logfile smclfile

**-----------------------------------------------**
** Snippet 4.9                                   **
** Using rdrobust with clustered standard errors **
**-----------------------------------------------**
cap drop clustervar
sjlog using "outputs/Vol-2-STATA_lso3_rdrobust_triangular_mserd_p1_rhofree_regterm1_cluster", replace
	gen clustervar=X
	rdrobust nextGPA X, kernel(triangular) p(1) bwselect(mserd) ///
		vce(cluster clustervar)
sjlog close, replace logfile smclfile

**------------------------------------------------------**
** Snippet 4.10                                         **
** Using rdrobust on the collapsed data (first outcome) **
**------------------------------------------------------**
preserve
	sjlog using "outputs/Vol-2-STATA_lso3_rdrobust_collapsed", replace
		collapse (mean) nextGPA, by(X)
		rdrobust nextGPA X
	sjlog close, replace logfile smclfile
restore

**--------------------------------**
** Snippet 4.11                   **
** Binomial test with rdwinselect **
**--------------------------------**
sjlog using "outputs/Vol-2-STATA_lso_falsification_binomial", replace
	rdwinselect X, wmin(0.01) nwindows(1) cutoff(0.000005)
sjlog close, replace logfile smclfile

**-----------------------**
** Snippet 4.12          **
** Binomial test by hand **
**-----------------------**
sjlog using "outputs/Vol-2-STATA_lso_falsification_binomial_byhand", replace
	bitesti 275 67 1/2
sjlog close, replace logfile smclfile

**--------------------------------**
** Snippet 4.13                   **
** Using rdrandinf on a covariate **
**--------------------------------**
sjlog using "outputs/Vol-2-STATA_lso_falsification_rdrandinf_hsgrade_pct", replace
	rdrandinf hsgrade_pct X, seed(50) wl(-.005) wr(.01)
sjlog close, replace logfile smclfile

**----------------------------------------**
** Table 4.4 (no output)                  **
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
** Snippet 4.14                                                  **
** Using rdwinselect with covariates to determine optimal window **
**---------------------------------------------------------------**
# delimit ;
	global covariates "hsgrade_pct totcredits_year1 age_at_entry male 
		bpl_north_america";
# delimit cr
sjlog using "outputs/Vol-2-STATA_lso_rdwinselect_consecutive_windows", replace
	rdwinselect X $covariates, cutoff(0.00005) wmin(0.01) wstep(0.01) seed(50) level(0.135)
sjlog close, replace logfile smclfile

**--------------------------------**
** Snippet 4.15                   **
** Using rdrandinf on the outcome **
**--------------------------------**
sjlog using "outputs/Vol-2-STATA_lso3_rdrandinf_adhocsmall_p0", replace
	rdrandinf nextGPA X, seed(50) wl(-0.005) wr(0.01) 
sjlog close, replace logfile smclfile

**--------------------------------------**
** Figure 4.2 (no output)               **
** rdplots for predetermined covariates **
**--------------------------------------**
foreach y of global covariates {
	rdplot `y' X
}

**------------------------**
** Figure 4.3 (no output) **
** rdplot for the outcome **
**------------------------**
rdplot nextGPA X

*------------------------------------------------------------------------------*
clear all
