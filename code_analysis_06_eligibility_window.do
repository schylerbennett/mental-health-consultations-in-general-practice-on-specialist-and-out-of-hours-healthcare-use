********************************************************************************
* File: code_analysis_06_eligibility_window.do
* Purpose: Sensitivity analysis, requested by reviewers. Reduce eligibility window ///
 for one, two, and three years and compare to four years.
* Input: synthetic_analysis_dataset.dta (created in code_00_simulated_data.do file)
* Output: sensitivity_analysis_eligibility_relevance_test.csv [1] ///
sensitivity_analysis_eligibility_iv_ols_estimates.csv [2]
* Requires: Stata 17+, packages reghdfe, ivreghdfe, ftools, estout (see requirements.txt)
* Naming convention for stored estimates:
	*elig_`yr'y = relevance (instrument-exposure association) results
	*iv`yr'y_outcomevar_year = two-stage least squares instrumental variable analysis
	*ols`yr'y_outcomevar_year = multivariable adjusted regression analysis
		/* where `yr' takes the values 1, 2, 3, and 4, representing the following eligibility windows:
			1 year = <366 days
			2 year = <731 days
			3 year = <1096 days
			4 year = <1461 days (equivalent to main analysis, where window is 4 years)
		*/
/*Outcome naming convention:
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

local 06_output_file1 `resdir'\sensitivity_analysis_eligibility_relevance_test.csv
local 06_output_file2 `resdir'\sensitivity_analysis_eligibility_iv_ols_estimates.csv

* -----------------------------------------------------------------------------

* Load dataset, and remove old versions of output files -----------------------

*load in dataset
use "`datafile'", clear

*OPTIONAL: Run this line to remove old versions of the output file to avoid duplicate rows
	*Because loop in output table line appends observations
capture erase "`06_output_file1'"
capture erase "`06_output_file2'"
* -----------------------------------------------------------------------------

*Outcomes
local outcomes ac_psych_in ac_soma_in nac_psych_in nac_soma_in nac_psych_out nac_soma_out ooh_psych ooh_soma


* -----------------------------------------------------------------------------

**# RELEVANCE -----------------------------------------------------------------

forvalues yr=1/4{
	local day = 365*`yr' + 1
	reghdfe exposure_var instrument_var if days_to_anyGP<`day', absorb(year_p0#new_gp_municipality patient_age_10y#patient_sex) cluster(person_id)
	eststo elig_`yr'y
}

*Output table
forvalues yr=1/4{
	esttab elig_`yr'y using "`06_output_file1'", cells("app_F t b se p") keep(instrument*) plain append
}

*Approximate F statistics are then estimated by squaring the t-value from this regression.

* -----------------------------------------------------------------------------

**# TWO-STAGE LEAST SQUARES INSTRUMENTAL VARIABLE ANALYSIS --------------------

foreach l of local outcomes {
	forvalues j=0/2{
		forvalues yr=1/4{
		local day = 365*`yr' + 1
		ivreghdfe `l' (exposure_var=instrument_var) if rel_p==`j' & days_in_year_end_only>364 & days_to_anyGP<`day', absorb(year_p0#new_gp_municipality patient_age_10y#patient_sex) cluster(person_id)
		eststo iv`yr'y_`l'_`j'
		}
	}
}

*Output table
foreach l of local outcomes {
	forvalues j=0/2{
		forvalues yr=1/4{
		esttab iv`yr'y_`l'_`j' using "`06_output_file2'", cells("b t se p ci ci_l ci_u") plain append
		}
	}
}


* -----------------------------------------------------------------------------

**# MULTIVARIABLE ADJUSTED REGRESSION ANALYSIS --------------------------------

foreach l of local outcomes {
	forvalues j=0/2{
		forvalues yr=1/4{
		local day = 365*`yr' + 1
		reghdfe `l' exposure_var if rel_p==`j' & days_in_year_end_only>364 & days_to_anyGP<`day', absorb(year_p0#new_gp_municipality patient_age_10y#patient_sex) cluster(person_id)
		eststo ols`yr'y_`l'_`j'
		}
	}
}

*Output table
foreach l of local outcomes {
	forvalues j=0/2{
		forvalues yr=1/4{
		esttab ols`yr'y_`l'_`j' using "`06_output_file2'", cells("b t se p ci ci_l ci_u") drop(_cons) plain append
		}
	}
}

********************************************************************************
