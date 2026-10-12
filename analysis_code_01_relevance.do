********************************************************************************
* File: code_analysis_01_relevance.do
* Purpose: Estimate the association between the instrument and the exposure (relevance assumption).
* Input: Analysis dataset (based on restricted registry dataset; TODO: make synthetic dataset?)
* Output: relevance_test.csv
* Requires: Stata 17+, packages reghdfe, ftools, estout
* Naming convention for stored estimates:
  *i_first  Instrumental variable, first-stage results
********************************************************************************


* Startup -------------------------------------------------------------------
version 19.5
clear all
set more off
* -----------------------------------------------------------------------------

**# USER INPUT (replace with relevant names/paths) ----------------------------

local datafile "projdir/data/synthetic_analysis_dataset.dta"
local resdir   "projdir/results"
local output_file "relevance_test.csv"
* -----------------------------------------------------------------------------

* Set wd, load dataset, and remove old versions of output files ---------------
  
*Set working directory to results/output folder
cd `resdir'

*load in dataset
use `datafile', clear

*Output files are appended to (because they use loops)
*Run this line to remove old versions of the file to avoid duplicate rows
capture erase `output_file'
* -----------------------------------------------------------------------------

**# Relevance test -----------------------------------------------------------

*Relevance estimation
reghdfe exposure_var instrument_var, absorb(year_p0#new_gp_municipality patient_age_10y#patient_sex) cluster(person_id)
eststo i_first 


*Loop to save estimates to csv file

local first_stage i_first

foreach l of local first_stage {
	esttab `l' using `output_file', cells("app_F t b se p") keep(instrument*) plain append
}

*Approximate F statistics are then estimated by squaring the t-value from this regression.

********************************************************************************

