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
use "CIT_2024_CUP_locrand.dta", clear

# delimit ;
	global vars "X Y T presdemvoteshlag1 demvoteshlag1 demvoteshlag2 
		demwinprv1 demwinprv2 dmidterm dpresdem dopen";
# delimit cr

**----------------------------------------------------------**
** Additional analysis (output not reported in publication) **
** Descriptive statistics                                   **
**----------------------------------------------------------**
foreach x of global vars {
	summarize `x', detail
}

**----------------------------------------------------**
** Figure 4 (Figure 2.3 in arXiv pre-print)           **
** Mimicking variance RD plot with evenly-spaced bins **
**----------------------------------------------------**
rdplot Y X, p(3)

**--------------------------------------------**
** Snippet 1 (Snippet 2.1 in arXiv pre-print) **
** rdrobust with default options              **
**--------------------------------------------**
rdrobust Y X, kernel(triangular) p(1) bwselect(mserd)

**--------------------------------------------**
** Snippet 2 (Snippet 2.2 in arXiv pre-print) **
** rdrandinf in ad-hoc window                 **
**--------------------------------------------**
rdrandinf Y X, wl(-2.5) wr(2.5) seed(50)

**--------------------------------------------**
** Snippet 3 (Snippet 2.3 in arXiv pre-print) **
** Binomial test using rdrandinf              **
**--------------------------------------------**
gen bern_prob = 1/2 if abs(X) <= 2.5
rdrandinf Y X, wl(-2.5) wr(2.5) seed(50) bernoulli(bern_prob)

**--------------------------------------------**
** Snippet 4 (Snippet 2.4 in arXiv pre-print) **
** Fisherian confidence interval              **
**--------------------------------------------**
rdrandinf Y X, wl(-2.5) wr(2.5) seed(50) ci(0.05 -20(0.10)20)

**--------------------------------------------------------**
** Snippet 5 (Snippet 2.5 in arXiv pre-print)             **
** Window selection with covariates                       **
** Note: we use the option "approx" to make things faster **
**--------------------------------------------------------**
# delimit ;
	global covs "presdemvoteshlag1 demvoteshlag1 demvoteshlag2 demwinprv1
	demwinprv2 dmidterm dpresdem dopen";
# delimit cr

rdwinselect X $covs, seed(50) wobs(2) approx

**--------------------------------------------------------**
** Figure 6 (Figure 2.5 in arXiv pre-print)               **
** Windows vs. p-values                                   **
** Note: we use the option "approx" to make things faster **
**--------------------------------------------------------**
rdwinselect X $covs, seed(50) wobs(2) nwindows(200) plot approx

**---------------------------------------------------------------**
** Snippet 6 (Snippet 2.6 in arXiv pre-print)                    **
** Confidence interval with optimal window and power calculation **
**---------------------------------------------------------------**
rdrandinf Y X, wl(-0.7652) wr(0.7652) seed(50) ///
	ci(0.05 -20(0.10)20) d(7.414)

**----------------------------------------------------------**
** Additional analysis (output not reported in publication) **
** Falsification: rdrandinf with one particular covariate   **
**----------------------------------------------------------**
rdrandinf presdemvoteshlag1 X, wl(-0.7652) wr(0.7652) seed(50)

**---------------------------------------------**
** Table 2 (Table 2.2 in arXiv pre-print)      **
** Falsification: rdrandinf for all covariates **
**---------------------------------------------**
local window = 0.7652

foreach y of global covs {
	ttest `y' if abs(X) <= `window', by(T)

	rdrandinf `y' X, wl(-`window') wr(`window') seed(50)
}

**----------------------------------------------------------**
** Additional analysis (output not reported in publication) **
** Density test using rdrandinf                             **
**----------------------------------------------------------**
rdwinselect X, wmin(0.7652) nwindows(1)

**----------------------------------------------------------**
** Additional analysis (output not reported in publication) **
** Binomial test by hand                                    **
**----------------------------------------------------------**
bitesti 41 25 1/2

**----------------------------------------------------------**
** Additional analysis (output not reported in publication) **
** Placebo cutoff at c=1                                    **
**----------------------------------------------------------**
rdrandinf Y X, c(1) wl(0.2348) wr(1.7652) seed(50)	

**---------------------------------------------**
** Table 3 (Table 2.3 in arXiv pre-print)      **
** Falsification analysis with placebo cutoffs **
**---------------------------------------------**
local window = 0.7652

foreach c in -1 1 {
	sum Y if X < `c' & X >= `c' - `window'
	sum Y if X >= `c' & X < `c' + `window'

	local left = `c' - `window'
	local right = `c' + `window'

	rdrandinf Y X, c(`c') wl(`left') wr(`right') seed(50)
}

**--------------------------------------------**
** Snippet 7 (Snippet 2.7 in arXiv pre-print) **
** Sensitivity to window choice               **
**--------------------------------------------**
rdrandinf Y X, wl(-0.6934) wr(0.6934) seed(50)	

*------------------------------------------------------------------------------*
clear all
