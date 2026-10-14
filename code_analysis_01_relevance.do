********************************************************************************
* File: code_analysis_01_relevance.do
* Purpose: Estimate the association between the instrument and the exposure (relevance assumption).
* Input: synthetic_analysis_dataset.dta (created in code_00_simulated_data.do file)
* Output: relevance_test.csv
* Requires: Stata 17+, packages reghdfe, ftools, estout (see requirements.txt)
* Naming convention for stored estimates:
  *i_first  Instrumental variable, first-stage results
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
local 01_output_file "`resdir'\relevance_test.csv"

* -----------------------------------------------------------------------------

* Load dataset, and remove old versions of output files -----------------------

*load in dataset
use "`datafile'", clear

*OPTIONAL: Run this line to remove old versions of the output file to avoid duplicate rows
	*Because loop in output table line appends observations
capture erase "`01_output_file'"
* -----------------------------------------------------------------------------

**# Relevance test -----------------------------------------------------------

*Relevance estimation
reghdfe exposure_var instrument_var, absorb(year_p0#new_gp_municipality patient_age_10y#patient_sex) cluster(person_id)
eststo i_first 


*Loop to save estimates to csv file

local first_stage i_first

foreach l of local first_stage {
	esttab `l' using "`01_output_file'", cells("app_F t b se p") keep(instrument*) plain append
}

*Approximate F statistics are then estimated by squaring the t-value from this regression.

********************************************************************************

