********************************************************************************
* File: code_analysis_03_2SLS_OLS.do
* Purpose: 
	*Two-stage least squares (2SLS) instrumental variable analysis:///
		Estimate the causal effect of the exposure (number of mental health consultation days in general practice) ///
		on the outcome (days of contact with specialist and out-of-hours healthcare), ///
		using the instrument (GP’s proxied preference for mental health consultation days per patient)///
		as a source of exogenous variation in the exposure. 
	*Multivariable adjusted (aka ordinary least squared, OLS) regression:///
		Estimate the association between the exposure (number of mental health consultation days in general practice) ///
		and the outcome (days of contact with specialist and out-of-hours healthcare).
* Input: synthetic_analysis_dataset.dta (created in code_00_simulated_data.do file)
* Output: iv_2SLS_OLS_estimates.csv
* Requires: Stata 17+, packages reghdfe, ivreghdfe, ftools, estout (see requirements.txt)
/* Naming convention for stored estimates:
	ac_ = acute (specialist)
	nac_ = non-acute (specialist)
	ooh_ = out-of-hours
	_psych = mental healthcare
	_soma = somatic healthcare
	_in = inpatient
	_out = outpatient
	_<j> = value of rel_p (0, 1, 2), year relative to index date, P0
*/
********************************************************************************
/*Notes for this file:

	*The if days_in_year_end_only>364 argument is used to specify inclusion only of observations which contribute a full year to the dataset

See S1 Supplementary Methods file for more information on these and other variables
*/
********************************************************************************


* Startup -------------------------------------------------------------------
version 19.5
clear all
set more off

* -----------------------------------------------------------------------------

**# USER INPUT (replace with relevant names/paths) ----------------------------

local projdir path\to\projectdirectory
local resdir `projdir'\results
local datadir `projdir'\data
local datafile `datadir'\synthetic_analysis_dataset.dta
local 03_output_file `resdir'\iv_2SLS_OLS_estimates.csv

* -----------------------------------------------------------------------------

* Load dataset, and remove old versions of output files -----------------------

*load in dataset
use "`datafile'", clear

*OPTIONAL: Run this line to remove old versions of the output file to avoid duplicate rows
	*Because loop in output table line appends observations
capture erase "`03_output_file'"
* -----------------------------------------------------------------------------

*Outcomes
local outcomes ac_psych_in ac_soma_in nac_psych_in nac_soma_in nac_psych_out nac_soma_out ooh_psych ooh_soma

* -----------------------------------------------------------------------------

**# TWO-STAGE LEAST SQUARES INSTRUMENTAL VARIABLE ANALYSIS -------------------


*IV estimates
foreach l of local outcomes {
	forvalues j=0/2{
		ivreghdfe `l' (exposure_var=instrument_var) if rel_p==`j' & days_in_year_end_only>364, absorb(year_p0#new_gp_municipality patient_age_10y#patient_sex) cluster(person_id)
		eststo iv_`l'_`j'
		}
}

*Output table
foreach l of local outcomes {
	forvalues j=0/2{
		esttab iv_`l'_`j' using "`03_output_file'", cells("b t se p ci ci_l ci_u") plain append
		}
}

* -----------------------------------------------------------------------------

**# MULTIVARIABLE ADJUSTED REGRESSION ANALYSIS --------------------------------


*Regression of exposure on outcome
foreach l of local outcomes {
	forvalues j=0/2{
		reghdfe `l' exposure_var if rel_p==`j' & days_in_year_end_only>364, absorb(year_p0#new_gp_municipality patient_age_10y#patient_sex) cluster(person_id)
		eststo ols_`l'_`j'
	}
}

*Output table (NB: appended to file created in 2SLS analysis)
foreach l of local outcomes {
	forvalues j=0/2{
		esttab ols_`l'_`j' using "`03_output_file'", cells("b t se p ci ci_l ci_u") drop(_cons) plain append
		}
}

********************************************************************************

