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
** Table 3.1              **
** Descriptive statistics **
**------------------------**
global sumstats "X1 T D Y $covariates"
matrix define R = J(10, 6, .)
local k = 1
foreach x of global sumstats {
	quietly summarize `x', detail
	local label_`k': variable label `x'
	local label_`k' "`label_`k'' (`x')"
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
	save "outputs/Vol-2-STATA_LRS_descstats.dta", replace
restore

**---------------------------**
** Figure 3.2                **
** rdplot of the first stage **
**---------------------------**
rdplot D X1

**-----------------------------**
** Snippet 3.1                 **
** rdrobust of the first stage **
**-----------------------------**
sjlog using "outputs/Vol-2-STATA_LRS_rdrobust_firststage", replace
	rdrobust D X1
sjlog close, replace logfile smclfile

**------------------------------**
** Snippet 3.2                  **
** rdrobust of the reduced form **
**------------------------------**
sjlog using "outputs/Vol-2-STATA_LRS_rdrobust_reducedform", replace
	rdrobust Y X1
sjlog close, replace logfile smclfile

**----------------------------**
** Figure 3.3                 **
** rdplot of the reduced form **
**----------------------------**
rdplot Y X1, p(3)

**------------------------**
** Snippet 3.3            **
** Fuzzy RD with rdrobust **
**------------------------**
sjlog using "outputs/Vol-2-STATA_LRS_rdrobust_fuzzy", replace
	rdrobust Y X1, fuzzy(D)
sjlog close, replace logfile smclfile

**----------------------------------------------------**
** Snippet 3.4                                        **
** Selecting a window with rdwinselect and covariates **
**----------------------------------------------------**
sjlog using "outputs/Vol-2-STATA_LRS_rdwinselect", replace
	# delimit ;
		global covariates "icfes_female icfes_age icfes_urm icfes_stratum
			icfes_famsize";
	# delimit cr

	rdwinselect X1 $covariates
sjlog close, replace logfile smclfile

**----------------------------**
** Snippet 3.5                **
** First stage with rdrandinf **
**----------------------------**
sjlog using "outputs/Vol-2-STATA_LRS_rdrandinf_firststage", replace
	rdrandinf D X1, wl(-0.13000107) wr(0.13000107)
sjlog close, replace logfile smclfile

**-----------------------------**
** Snippet 3.6                 **
** Reduced form with rdrandinf **
**-----------------------------**
sjlog using "outputs/Vol-2-STATA_LRS_rdrandinf_reducedform", replace
	rdrandinf Y X1, wl(-0.13000107) wr(0.13000107)
sjlog close, replace logfile smclfile

**-------------------------**
** Snippet 3.7             **
** Fuzzy RD with rdrandinf **
**-------------------------**
sjlog using "outputs/Vol-2-STATA_LRS_rdrandinf_TSLS", replace
	rdrandinf Y X1, wl(-0.13000107) wr(0.13000107) fuzzy(D tsls)
sjlog close, replace logfile smclfile

**----------------------------------**
** Snippet 3.8                      **
** Manipulation test with rddensity **
**----------------------------------**
sjlog using "outputs/Vol-2-STATA_LRS_rddensity", replace
	rddensity X1, bino_w(0.13000107) bino_nw(1)
sjlog close, replace logfile smclfile

**--------------------------------------------**
** Snippet 3.9                                **
** Reduced form on a covariate using rdrobust **
**--------------------------------------------**
sjlog using "outputs/Vol-2-STATA_LRS_rdrobust_reducedform_examplecovariate", replace
	rdrobust icfes_female X1, bwselect(cerrd)
sjlog close, replace logfile smclfile

**---------------------------------------------**
** Snippet 3.10                                **
** Reduced form on a covariate using rdrandinf **
**---------------------------------------------**
sjlog using "outputs/Vol-2-STATA_LRS_rdrandinf_ITT_examplecovariate", replace
	rdrandinf icfes_female X1, wl(-0.13000107) wr(0.13000107)
sjlog close, replace logfile smclfile

**-------------------------------------------------------**
** Table 3.2                                             **
** Reduced form regressions on covariates using rdrobust **
**-------------------------------------------------------**
matrix define R = J(6, 8, .)
local k = 1
foreach y of global covariates {
	rdrobust `y' X, all bwselect(cerrd)
	local label_`k': variable label `y'
	local label_`k' "`label_`k'' (`y')"
	matrix R[`k', 1] = e(h_l)
	matrix R[`k', 2] = e(tau_cl)
	matrix R[`k', 3] = e(tau_bc)
	matrix R[`k', 4] = e(se_tau_rb)
	matrix R[`k', 5] = 2 * normal(-abs(R[`k', 3] / R[`k', 4]))
	matrix R[`k', 6] = R[`k', 3] - invnormal(0.975) * R[`k', 4]
	matrix R[`k', 7] = R[`k', 3] + invnormal(0.975) * R[`k', 4]
	matrix R[`k', 8] = e(N_h_l) + e(N_h_r)

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
	save "outputs/Vol-2-STATA_LRS_falsification_rdrobust_reducedform_allcovariates.dta", replace
restore

**--------------------------------------------------------**
** Table 3.3                                              **
** Reduced form regressions on covariates using rdrandinf **
**--------------------------------------------------------**
local window = 0.13000107
matrix define R = J(6, 5, .)
local k = 1
foreach y of global covariates {
	ttest `y' if abs(X) <= `window', by(T)
	matrix R[`k', 1] = r(mu_1)
	matrix R[`k', 2] = r(mu_2)

	rdrandinf `y' X, wl(-`window') wr(`window')
	local label_`k': variable label `y'
	local label_`k' "`label_`k'' (`y')"
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
	save "outputs/Vol-2-STATA_LRS_rdrandinf_ITT_allcovariates.dta", replace
restore

*------------------------------------------------------------------------------*
clear all
