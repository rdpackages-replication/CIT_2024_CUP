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
set matsize 10000

******************************************************************************
******************************************************************************
******************************************************************************
********* Section 2: The Local Randomization Approach to RD Analysis *********
******************************************************************************
******************************************************************************
******************************************************************************

**------------------**
** Loading the data **
**------------------**
use "CIT_2023_CUP_locrand.dta", clear

# delimit ;
	global vars "X Y T presdemvoteshlag1 demvoteshlag1 demvoteshlag2 
		demwinprv1 demwinprv2 dmidterm dpresdem dopen";
# delimit cr

**------------------------**
** Table 2.1              **
** Descriptive statistics **
**------------------------**
matrix define R = J(11, 6, .)
local k = 1
foreach x of global vars {
	quietly summarize `x', detail
	local label_`k': variable label `x'
	matrix R[`k', 1] = r(mean)
	matrix R[`k', 2] = r(p50)
	matrix R[`k', 3] = r(sd)
	matrix R[`k', 4] = r(min)
	matrix R[`k', 5] = r(max)
	matrix R[`k', 6] = r(N)
	local k = `k' + 1
}

preserve
	clear
	local t = `k' - 1
	svmat R
	gen R0 = ""
	forvalues k = 1 / `t' {
		replace R0 = "`label_`k''" if _n == `k'
	}
	order R0
	replace R0 = subinstr(R0, "=1 if ", "", .)
	replace R0 = proper(R0)
	save "outputs/Vol-2-STATA_senate_descstats.dta", replace
restore

**----------------------------------------------------**
** Figure 2.3                                         **
** Mimicking variance RD plot with evenly-spaced bins **
**----------------------------------------------------**
rdplot Y X, p(3)

**-------------------------------**
** Snippet 2.1                   **
** rdrobust with default options **
**-------------------------------**
sjlog using "outputs/Vol-2-STATA_senate_rdrobust_triangular_mserd_p1_rhofree_regterm1", replace
	rdrobust Y X, kernel(triangular) p(1) bwselect(mserd)
sjlog close, replace logfile smclfile

**----------------------------**
** Snippet 2.2                **
** rdrandinf in ad-hoc window **
**----------------------------**
sjlog using "outputs/Vol-2-STATA_senate_rdrandinf_adhoc_p0", replace
	rdrandinf Y X, wl(-2.5) wr(2.5) seed(50)
sjlog close, replace logfile smclfile

**-------------------------------**
** Snippet 2.3                   **
** Binomial test using rdrandinf **
**-------------------------------**
sjlog using "outputs/Vol-2-STATA_senate_rdrandinf_adhoc_p0_bernoulli", replace
	gen bern_prob = 1/2 if abs(X) <= 2.5
	rdrandinf Y X, wl(-2.5) wr(2.5) seed(50) bernoulli(bern_prob)
sjlog close, replace logfile smclfile

**-------------------------------**
** Snippet 2.4                   **
** Fisherian confidence interval **
**-------------------------------**
sjlog using "outputs/Vol-2-STATA_senate_rdrandinf_adhoc_p0_ci", replace
	rdrandinf Y X, wl(-2.5) wr(2.5) seed(50) ci(0.05 -20(0.10)20)
sjlog close, replace logfile smclfile

**--------------------------------------------------------**
** Snippet 2.5                                            **
** Window selection with covariates                       **
** Note: we use the option "approx" to make things faster **
**--------------------------------------------------------**
sjlog using "outputs/Vol-2-STATA_senate_rdwinselect_automatic_p0_wobs2_secondtry", replace
	# delimit ;
		global covs "presdemvoteshlag1 demvoteshlag1 demvoteshlag2 demwinprv1
		demwinprv2 dmidterm dpresdem dopen";
	# delimit cr
	rdwinselect X $covs, seed(50) reps(1000) wobs(2) approx
sjlog close, replace logfile smclfile

**--------------------------------------------------------**
** Figure 2.5                                             **
** Windows vs. p-values                                   **
** Note: we use the option "approx" to make things faster **
**--------------------------------------------------------**
rdwinselect X $covs, seed(50) reps(1000) wobs(2) nwindows(200) plot approx

**---------------------------------------------------------------**
** Snippet 2.6                                                   **
** Confidence interval with optimal window and power calculation **
**---------------------------------------------------------------**
sjlog using "outputs/Vol-2-STATA_senate_rdrandinf_specific1", replace
	rdrandinf Y X, wl(-0.7652) wr(0.7652) seed(50) reps(1000) ///
		ci(0.05 -20(0.10)20) d(7.414)
sjlog close, replace logfile smclfile

**--------------------------------------------------------**
** Snippet 2.7                                            **
** Falsification: rdrandinf with one particular covariate **
**--------------------------------------------------------**
sjlog using "outputs/Vol-2-STATA_senate_falsification_rdrandinf_presdemvoteshlag1", replace
	rdrandinf presdemvoteshlag1 X, wl(-0.7652) wr(0.7652) seed(50) reps(1000)
sjlog close, replace logfile smclfile

**---------------------------------------------**
** Table 2.4                                   **
** Falsification: rdrandinf for all covariates **
**---------------------------------------------**
local window = 0.7652
matrix define R = J(8, 5, .)
local k = 1
foreach y of global covs {
	ttest `y' if abs(X) <= `window', by(T)
	matrix R[`k', 1] = r(mu_1)
	matrix R[`k', 2] = r(mu_2)

	rdrandinf `y' X, wl(-`window') wr(`window') seed(50)
	local label_`k': variable label `y'
	matrix R[`k', 3] = r(obs_stat)
	matrix R[`k', 4] = r(randpval)
	matrix R[`k', 5] = r(N)
	
	local k = `k' + 1
}

preserve
	clear
	local t = `k' - 1
	svmat R
	gen R0 = ""
	forvalues k = 1 / `t' {
		replace R0 = "`label_`k''" if _n == `k'
	}
	order R0
	save "outputs/Vol-2-STATA_senate_rdrandinf_allcovariates.dta", replace
restore

**------------------------------**
** Snippet 2.8                  **
** Density test using rdrandinf **
**------------------------------**
sjlog using "outputs/Vol-2-STATA_senate_falsification_binomial", replace
	rdwinselect X, wmin(0.7652) nwindows(1)
sjlog close, replace logfile smclfile

**-----------------------**
** Snippet 2.9           **
** Binomial test by hand **
**-----------------------**
sjlog using "outputs/Vol-2-STATA_senate_falsification_binomial_byhand", replace
	bitesti 41 25 1/2
sjlog close, replace logfile smclfile

**-----------------------**
** Snippet 2.10          **
** Placebo cutoff at c=1 **
**-----------------------**
sjlog using "outputs/Vol-2-STATA_senate_falsification_placebocutoff1", replace
	rdrandinf Y X, c(1) wl(0.2348) wr(1.7652) seed(50)	
sjlog close, replace logfile smclfile

**---------------------------------------------**
** Table 2.5                                   **
** Falsification analysis with placebo cutoffs **
**---------------------------------------------**
local window = 0.7652
matrix define R = J(2, 5, .)
local k = 1
foreach c in -1 1 {
	sum Y if X < `c' & X >= `c' - `window'
	matrix R[`k', 1] = r(mean)
	sum Y if X >= `c' & X < `c' + `window'
	matrix R[`k', 2] = r(mean)

	local left = `c' - `window'
	local right = `c' + `window'

	rdrandinf Y X, c(`c') wl(`left') wr(`right') seed(50)
	matrix R[`k', 3] = r(obs_stat)
	matrix R[`k', 4] = r(randpval)
	matrix R[`k', 5] = r(N)
	
	local k = `k' + 1
}

matrix list R

preserve
	clear
	local t = `k' - 1
	svmat R
	gen R0 = "Cutoff at -1" if _n == 1
	replace R0 = "Cutoff at 1" if _n == 2
	order R0
	save "outputs/Vol-2-STATA_senate_rdrandinf_placebocutoffs.dta", replace
restore

**------------------------------**
** Snippet 2.11                 **
** Sensitivity to window choice **
**------------------------------**
sjlog using "outputs/Vol-2-STATA_senate_windowsensitivity", replace
	rdrandinf Y X, wl(-0.6934) wr(0.6934) seed(50)	
sjlog close, replace logfile smclfile

*------------------------------------------------------------------------------*
clear all
