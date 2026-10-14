********************************************************************************
* File: code_analysis_07_cons_days_prior_to_mh_diagnosis.do
* Purpose: Adjust main IV and OLS estimates by the number of consultation days ///
between assigned GP and index patient between T0 and P0 to see if GP practice style ///
for mental health diagnosis is driving results.
* Input: synthetic_analysis_dataset.dta (created in code_00_simulated_data.do file)
* Output: cons_days_between_first_cons_to_p0_table.docx [1] ///
	sensitivity_analysis_consdays_between_assigned.csv [2]
* Requires: Stata 17+, packages reghdfe, ivreghdfe, ftools, estout (see requirements.txt)
* Naming convention for stored estimates:
	*a_iv_outcomevar_year = two-stage least squares instrumental variable analysis, ///
			adjusted for consultation days prior to index date
	*a_ols_outcomevar_year = multivariable adjusted regression analysis, ///
			adjusted for consultation days prior to index date
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

	*consdays_between_assigned = Number of consultation days, regardless of diagnosis, ///
	between index patient and assigned GP between date of assignment (T0) and index date ///
	(date of mental health diagnosis, P0)

*/

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

local 07_output_file1 `resdir'\cons_days_between_first_cons_to_p0_table.docx
local 07_output_file2 `resdir'\sensitivity_analysis_consdays_between_assigned.csv

* -----------------------------------------------------------------------------

* Load dataset, and remove old versions of output files -----------------------

*load in dataset
use "`datafile'", clear

*OPTIONAL: Run this line to remove old versions of the output file to avoid duplicate rows
	*Because loop in output table line appends observations
capture erase "`07_output_file1'"
capture erase "`07_output_file2'"

* -----------------------------------------------------------------------------

*Outcomes
local outcomes ac_psych_in ac_soma_in nac_psych_in nac_soma_in nac_psych_out nac_soma_out ooh_psych ooh_soma

* -----------------------------------------------------------------------------

*Descriptive table of variable for number of consultation days between T0 and P0

collect clear
table (var) (result[mean sd median iqr max count]) if rel_p==0 & consdays_between_assigned>0, statistic(mean consdays_between_assigned) statistic(sd consdays_between_assigned) statistic(median consdays_between_assigned) statistic(iqr consdays_between_assigned) statistic(max consdays_between_assigned) statistic(count consdays_between_assigned) nformat(%16.2fc mean sd median iqr) sformat((%s) sd) nformat(%16.0fc max count) style(table)
collect preview

collect export "`07_output_file1'", replace
* -----------------------------------------------------------------------------

**# TWO-STAGE LEAST SQUARES INSTRUMENTAL VARIABLE ANALYSIS --------------------

foreach l of local outcomes {
	forvalues j=0/2{
		ivreghdfe `l' (exposure_var=instrument_var) c.consdays_between_assigned if rel_p==`j' & days_in_year_end_only>364, absorb(year_p0#new_gp_municipality patient_age_10y#patient_sex) cluster(person_id)
		eststo a_iv_`l'_`j'
		}
}

*Output table
foreach l of local outcomes {
	forvalues j=0/2{
		esttab a_iv_`l'_`j' using "`07_output_file2'", cells("b t se p ci ci_l ci_u") plain append
		}
}

* -----------------------------------------------------------------------------

**# MULTIVARIABLE ADJUSTED REGRESSION ANALYSIS --------------------------------

foreach l of local outcomes {
	forvalues j=0/2{
		reghdfe `l' exposure_var c.consdays_between_assigned if rel_p==`j' & days_in_year_end_only>364, absorb(year_p0#new_gp_municipality patient_age_10y#patient_sex) cluster(person_id)
		eststo a_ols_`l'_`j'
	}
}

*Output table
foreach l of local outcomes {
	forvalues j=0/2{
		esttab a_ols_`l'_`j' using "`07_output_file2'", cells("b t se p ci ci_l ci_u") drop(_cons) plain append
		}
}

********************************************************************************
