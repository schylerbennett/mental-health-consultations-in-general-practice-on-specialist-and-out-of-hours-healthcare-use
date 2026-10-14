********************************************************************************
* File: code_analysis_02_balance.do
* Purpose: Estimate the association between the instrument and measured covariates,
	*including patient characteristics and prior healthcare utilization.
* Input: synthetic_analysis_dataset.dta (created in code_00_simulated_data.do file)
* Output: balance_test.csv
* Requires: Stata 17+, packages reghdfe, ftools, estout (see requirements.txt)
* Naming convention for stored estimates:
  * b_<outcome> 	IV-covariate association
********************************************************************************
/*
Notes for this file:
	*rel_p = year relative to index date, P0 (0 = first year after P0; 1 = second year after P0; 2 = third year after P0)
	*The if rel_p==0 argument is used here to specify that only one observation per patient is used in the analysis (as the variables are all fixed values)
	*patient_age is continuous form of variable, patient_age_10y is patient age in 10-year categories
	*edu_cat is dichotomous version of education variable
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
local 02_output_file `resdir'\balance_test.csv

* -----------------------------------------------------------------------------

* Load dataset, and remove old versions of output files -----------------------

*load in dataset
use "`datafile'", clear

*OPTIONAL: Run this line to remove old versions of the output file to avoid duplicate rows
	*Because loop in output table line appends observations
capture erase "`02_output_file'"


* -----------------------------------------------------------------------------
*Healthcare use variables
local hc_use ac_psych_in nac_psych_in nac_psych_out ooh_psych ac_soma_in nac_soma_in nac_soma_out ooh_soma


**# Balance tests -------------------------------------------------------------


*PATIENT CHARACTERISTICS

*Patient age

reghdfe patient_age instrument_var if rel_p==0, absorb(year_p0#new_gp_municipality patient_age_10y#patient_sex) cluster(person_id)
eststo b_pat_age

*Patient education

reghdfe edu_cat instrument_var if rel_p==0, absorb(year_p0#new_gp_municipality patient_age_10y#patient_sex) cluster(person_id) 
eststo b_edu

*HEALTHCARE USE (in the year two years prior to assignment, called T-2)

foreach l of local hc_use {
	reghdfe `l'_t_minus_2 instrument_var if rel_p==0, absorb(year_p0#new_gp_municipality patient_age_10y#patient_sex) cluster(person_id)
	eststo b_`l'2
	}

local balance b_pat_age b_edu b_ac_psych_in2 b_nac_psych_in2 b_nac_psych_out2 b_ooh_psych2 b_ac_soma_in2 b_nac_soma_in2 b_nac_soma_out2 b_ooh_soma2

foreach l of local balance {
	esttab `l' using "`02_output_file'", cells("b t se p ci ci_l ci_u") drop(_cons) plain append
}

********************************************************************************


