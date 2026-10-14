********************************************************************************
* File: code_analysis_00_generic_reghdfe_ivreghdfe_ppmlhdfe_and_output_code.do
* Purpose: Illustrate generic version of all analysis and output commands
* Requires: Stata 17+, packages reghdfe, ivreghdfe, ppmlhdfe, ftools, estout 
	*(see requirements.txt)

********************************************************************************


* Startup -------------------------------------------------------------------
version 19.5
clear all
set more off

* -----------------------------------------------------------------------------

**# USER INPUT (replace with relevant names/paths) ----------------------------

*Path and file names
local projdir path\to\projectdirectory
local resdir `projdir'\results
local datadir `projdir'\data
local datafile `datadir'\synthetic_analysis_dataset.dta
local output_file `resdir'\generic_output.csv //other file formats allowed, see estout helpfile

*Variable names
local outcomevar varname1
local exposurevar varname2
local instrumentvar varname3
local covar1 varname4 //if adjusting for additional variables outside fixed effects
local covar2 varname5 //if adjusting for additional variables outside fixed effects
local fixedeffects varname6#varname7 varname8#varname9 // use # to denote ///
interacting variables or space to represent separate variables
local clustervars varname10 //multiple cluster variables allowed
local year_var varname11 //or other stratification variable

*To specify variables as continuous, use c.varname
*To specify variables as categorical, use i.varname
* -----------------------------------------------------------------------------

*General reghdfe format:
	reghdfe outcomevar exposurevar covar1 covar2, absorb(fixedeffects) cluster(clustervars)

*General ivreghdfe format:
	ivreghdfe outcomevar (exposurevar=instrumentvar) covar1 covar2, absorb(fixedeffects) cluster(clustervars)

*General ppmlhdfe format:
	ppmlhdfe outcomevar exposurevar covar1 covar2, absorb(fixedeffects) cluster(clustervars)

*General estimate storing format
	*BASED ON ESTIMATE CURRENTLY STORED IN MEMORY, NEEDS TO BE RUN IMMEDIATELY AFTER RELEVANT COMMAND
	eststo est_name

*General output format
	esttab est_name using "`output_file'", cells("b t se p ci ci_l ci_u") plain append //can specify other cells to include,///
see estout helpfile for more information

* -----------------------------------------------------------------------------

* -----------------------------------------------------------------------------
**# General loop format for multiple outcomes and multiple years

*STEP 1: Create local list of outcome variable names
local outcomes outcomevar1 outcomevar2 outcomevar3 outcomevar4

*STEP 2: Run analyses
	*Example using ivreghdfe (but applies to others as well)
	*Make sure to run eststo after each regression

foreach l of local outcomes {
	forvalues j=0/2{
		ivreghdfe `l' (exposure_var=instrumentvar) if year_var==`j',  absorb(fixedeffects) cluster(clustervars)
		eststo e_iv_`l'_`j'
		}
}

*STEP 3: Output table
foreach l of local outcomes {
	forvalues j=0/2{
		esttab e_iv_`l'_`j' using "`output_file'", cells("b t se p ci ci_l ci_u") plain append
		}
}

********************************************************************************
