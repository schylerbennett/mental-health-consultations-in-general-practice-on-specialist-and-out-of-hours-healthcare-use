********************************************************************************
* File: code_analysis_04_relative_risk_scaled_mean_difference.do
* Purpose: Sensitivity analysis, requested by reviewers
	*Create relative risk estimates and scaled mean differences for findings reported in text,///
		both scaled by standard deviation change in exposure
* Input: synthetic_analysis_dataset.dta (created in code_00_simulated_data.do file)
* Output: sensitivity_analysis_relative_risk.csv [1] ///
	  sensitivity_analysis_scaled_mean_difference.csv [2]
* Requires: Stata 17+, packages reghdfe, ivreghdfe, ppmlhdfe, ftools, estout (see requirements.txt)
* Naming convention for stored estimates:
	*e1_outcomevar_year = effect estimate for instrument-outcome association
	*e2_sd_year = effect estimate for instrument-exposure association, scaled per one standard deviation change in the exposure
	*smd_outcomevar_year = scaled mean difference of two-stage least squares instrumental variable analysis, ///
		scaled per one standard deviation change in the exposure
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
/* Notes for this file:
	*Wald ratio used to create relative risk estimates.**
		*Wald ratio defined as: 
		exp(Effect estimate for instrument-outcome association / Effect estimate for instrument-exposure association)

			**See: Burgess, S., Small, D. S., & Thompson, S. G. (2017). 
			A review of instrumental variable estimators for Mendelian randomization. 
			Statistical Methods in Medical Research, 26(5), 2333–2355. 
			https://doi.org/10.1177/0962280215597579 

	*Relative risk/scaled mean difference calculated ONLY for POINT ESTIMATES REPORTED IN TEXT
		*(i.e. specialist mental healthcare use in the first year after MH diagnosis)
	
	*Relative risk and scaled mean differences are scaled by one standard deviation change in the exposure.

	*Final value of relative risk estimate (exponential of e1_outcome_var_year/e2_sd_year) is calculated in resulting spreadsheet


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

local 04_output_file1 `resdir'\sensitivity_analysis_relative_risk.csv
local 04_output_file2 `resdir'\sensitivity_analysis_scaled_mean_difference.csv

* -----------------------------------------------------------------------------

* Load dataset, and remove old versions of output files -----------------------

*load in dataset
use "`datafile'", clear

*OPTIONAL: Run this line to remove old versions of the output files to avoid duplicate rows
	*Because loop in output table line appends observations
capture erase "`04_output_file1'"
capture erase "`04_output_file2'"
* -----------------------------------------------------------------------------

*Outcomes
	*as mentioned in notes, estimates only calculated for values reported in manuscript text
local outcomes_rr ac_psych_in nac_psych_in nac_psych_out 


* -----------------------------------------------------------------------------


**# RELATIVE RISK CALCULATIONS ------------------------------------------------

* Generate instrument-outcome association (numerator)
foreach l of local outcomes_rr{
	forvalues j=0/0{
		ppmlhdfe `l' instrument_var if rel_p==`j' & days_in_year_end_only>364, absorb(year_p0#new_gp_municipality patient_age_10y#patient_sex) cluster(person_id)
		eststo e1_`l'_`j'
		}
}


* Generate instrument-exposure association, scaled for one standard deviation change in exposure (denominator)
forvalues j=0/0{
	reghdfe exposure_var_sd instrument_var if rel_p==`j' & days_in_year_end_only>364, absorb(year_p0#new_gp_municipality patient_age_10y#patient_sex) cluster(person_id)
	eststo e2_sd_`j'
	}



*Output table

foreach l of local outcomes_rr {
	forvalues j=0/0{
		esttab e1_`l'_`j' using "`04_output_file1'", cells("b") drop(_cons) plain append
		esttab e2_sd_`j' using "`04_output_file1'", cells("b") drop(_cons) plain append
		}
}



*NOTE: Need to finish calculating relative risk in excel file-- take exponential of e1_outcomevar_year/e2_sd_year
	*Use following formula: =EXP(XX/YY)
	*where XX = column and row of e1_outcomevar_year
	*and YY = column and row of e2_sd_year

* -----------------------------------------------------------------------------

**# SCALED MEAN DIFFERENCE CALCULATION ----------------------------------------

foreach l of local outcomes_rr {
	forvalues j=0/0{
		ivreghdfe `l' (exposure_var_sd=instrument_var) if rel_p==`j' & days_in_year_end_only>364, absorb(year_p0#new_gp_municipality patient_age_10y#patient_sex) cluster(person_id)
		eststo smd_`l'_`j'
		}
}

*Output table
foreach l of local outcomes_rr {
	forvalues j=0/0{
		esttab smd_`l'_`j' using "`04_output_file2'", cells("b t se p ci ci_l ci_u") plain append
		}
}

********************************************************************************
