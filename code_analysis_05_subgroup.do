********************************************************************************
* File: code_analysis_05_subgroup.do
* Purpose: Sensitivity analysis which excludes patients with any specialist ///
or out-of-hours healthcare utilization in the year before assignment to see if ///
the results are driven by continuation of existing healthcare pathways.
* Input: synthetic_analysis_dataset.dta (created in code_00_simulated_data.do file)
* Output: subgroup_analysis.csv
* Requires: Stata 17+, packages reghdfe, ivreghdfe, ftools, estout (see requirements.txt)
* Naming convention for stored estimates:
 	*sg1_iv_outcomevar_year = Excluding sub-group, instrumental variable analysis
	*sg1_ols_outcomevar_year = Excluding sub-group, multivariable adjusted regression analysis
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

local 05_output_file `resdir'\subgroup_analysis.csv

* -----------------------------------------------------------------------------

* Load dataset, and remove old versions of output files -----------------------

*load in dataset
use "`datafile'", clear

*OPTIONAL: Run this line to remove old versions of the output file to avoid duplicate rows
	*Because loop in output table line appends observations
capture erase "`05_output_file'"

* -----------------------------------------------------------------------------

*Outcomes
local outcomes ac_psych_in ac_soma_in nac_psych_in nac_soma_in nac_psych_out nac_soma_out ooh_psych ooh_soma

* -----------------------------------------------------------------------------

**# TWO-STAGE LEAST SQUARES INSTRUMENTAL VARIABLE ANALYSIS --------------------

*Excluding subgroup
	foreach l of local outcomes {
		forvalues j=0/2{
			ivreghdfe `l' (exposure_var=instrument_var) if rel_p==`j' & days_in_year_end_only>364 & any_mh_use_T_minus_2==0, absorb(year_p0#new_gp_municipality patient_age_10y#patient_sex) cluster(person_id)
			eststo sg1_iv_`l'_`j'
		}
	}

*Output table
	foreach l of local outcomes {
		forvalues j=0/2{
			esttab sg1_iv_`l'_`j' using "`05_output_file'", cells("b t se p ci ci_l ci_u") plain append
		}
	}

* -----------------------------------------------------------------------------

**# MULTIVARIABLE ADJUSTED REGRESSION ANALYSIS --------------------------------

*Excluding subgroup
	foreach l of local outcomes {
		forvalues j=0/2{
			reghdfe `l' exposure_var if rel_p==`j' & days_in_year_end_only>364 & any_mh_use_T_minus_2==0, absorb(year_p0#new_gp_municipality patient_age_10y#patient_sex) cluster(person_id)
			eststo sg1_ols_`l'_`j'
		}
	}

*Output table
	foreach l of local outcomes {
		forvalues j=0/2{
			esttab sg1_ols_`l'_`j' using "`05_output_file'", cells("b t se p ci ci_l ci_u") drop(_cons) plain append
		}
	}


********************************************************************************
