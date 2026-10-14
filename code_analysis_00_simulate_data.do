********************************************************************************
* File: code_analysis_00_simulate_data.do
* One row per person_id x rel_p (rel_p = 0, 1, 2)
* Person-level variables are identical across all three observation rows,
* only the outcomes and days_in_year_end_only vary by row.
********************************************************************************

********************************************************************************
*EXTREMELY IMPORTANT NOTE!
*The data here is SYNTHETIC and does not fully replicate the data used in the study.
*Means, distributions, etc. are only vaguely approximately equal to what is seen in the study.
*Thus, results will differ from those found in the study.
*This data is provided solely for use with the reproducible code files.
********************************************************************************
* Startup -------------------------------------------------------------------

version 19.5
clear all
set more off

*------------------------------------------------------------------------------
**# 00. SET SEED NUMBER AND NAME LOCALS
*------------------------------------------------------------------------------

set seed 12345

local n_persons   300000
local end_dataset = mdy(12,31,2021)
local outcomes ac_psych_in ac_soma_in nac_psych_in nac_soma_in nac_psych_out nac_soma_out ooh_psych ooh_soma
* Mean and standard deviation use per outcome (same order as above)
local base_means 0.61 0.94 0.62 0.44 2.19 1.39 0.06 0.35
local sds        7.65 4.19 7.69 3.25 7.47 3.29 0.49 0.93
local pany 0.02 0.15 0.02 0.08 0.15 0.35 0.03 0.20
	*Mean and SD values are from registry dataset to simulate similar distributions

*------------------------------------------------------------------------------
**# 0. PROGRAM (to draw distributions for outcome variables in step 1 and 2)
*------------------------------------------------------------------------------
capture program drop draw_count
program define draw_count
    syntax newvarname, MEAN(real) SD(real) P(real)

    local muw   = `mean' / `p'
    local varw  = (`sd'^2 - `p'*(1-`p')*`muw'^2) / `p'
    local mz    = `muw' - 1

    if `varw' <= 0 | `mz' <= 0 {
        di as error "`varlist': p = `p' infeasible for mean `mean', sd `sd' (need p >= " ///
            %6.4f `mean'^2/(`sd'^2+`mean'^2) " and p <= mean)"
        exit 198
    }

    tempvar b lam
    gen byte `b' = runiform() < `p'

    if `varw' > `mz' {
        * negative binomial for Z: mean mz, variance varw
        local r = `mz'^2 / (`varw' - `mz')
        gen double `lam' = rgamma(`r', `mz'/`r')
    }
    else {
        gen double `lam' = `mz'     // underdispersed: plain Poisson
    }
    replace `lam' = min(max(`lam', 1e-9), 1e6)

    gen int `varlist' = `b' * (1 + rpoisson(`lam'))
    replace `varlist' = 366 if `varlist' > 366
end

*------------------------------------------------------------------------------
**# 1. PERSON-LEVEL DATA (one row per person)
*------------------------------------------------------------------------------
set obs `n_persons'

gen long person_id = _n


* p0: date between 1 Jan 2008 and 31 Dec 2021, roughly normal across years
gen yr = round(rnormal(2014.5, 3))
count if yr < 2008 | yr > 2021
while r(N) > 0 {
    replace yr = round(rnormal(2014.5, 3)) if yr < 2008 | yr > 2021
    count if yr < 2008 | yr > 2021
}
gen p0 = mdy(1,1,yr) + floor(runiform() * (mdy(12,31,yr) - mdy(1,1,yr) + 1))
format p0 %td
gen year_p0 = yofd(p0)


* GP municipality: 356 values, 0-355, displayed as 000, 001, ...
gen int new_gp_municipality = floor(runiform() * 356)
format new_gp_municipality %03.0f

* Patient sex, 0 = female, 1 = male
gen byte patient_sex = runiform() < .5

* Patient age
gen byte patient_age = 18 + 82 * rbeta(1.6, 2.2)
recode patient_age 0/9=0 10/19=1 20/29=2 30/39=3 40/49=4 50/59=5 60/69=6 70/79=7 80/89=8 90/129=9, gen(patient_age_10y)

*Patient education
gen byte edu = 0 + 9 * runiform()
recode edu 0/4 = 0 5/8 = 1 9/9 = ., gen(edu_cat)

* Right-skewed instrument and exposure (many zeros/low values)
gen instrument_var = rgamma(2.5, 0.8) * (runiform() > 0.25)
gen exposure_var = ceil(rgamma(0.13, 14)) * (runiform() > 0.20)

* Exposure variable scaled by one standard deviation, used in code_analysis_04_relative_risk_scaled_mean_difference
egen standard_deviation_exp = sd(exposure_var)
bysort person_id: gen exposure_var_sd = exposure_var/standard_deviation_exp

* Expected use per outcome in t-2 (2 years before assignment) values, used in code_analysis_05_subgroup
local i 0
foreach l of local outcomes {
    local ++i
    local m : word `i' of `base_means'
    local s : word `i' of `sds'
    local p : word `i' of `pany'
    draw_count `l'_t_minus_2, mean(`m') sd(`s') p(`p')
}

* Categorical version of T-2 variables    
foreach l of local outcomes {
	recode `l'_t_minus_2 0=0 1/366=1, gen(`l'_t_minus_2cat)
}    

* Categorical variable for any specialist or out-of-hours mental healthcare use in T-2
gen any_mh_use_T_minus_2=0
bysort person_id: replace any_mh_use_T_minus_2=1 if (ac_psych_in_t_minus_2cat==1 | nac_psych_in_t_minus_2cat==1 | nac_psych_out_t_minus_2cat==1 | ooh_psych_t_minus_2cat==1)

* Consultation days prior to P0 variable, used in code_analysis_06_eligibility window
gen days_to_anyGP = round(rgamma(1.5, 83) * (runiform()>0.25))
replace days_to_anyGP = min(days_to_anyGP, 1460)

* Consultation days prior to P0, used in code_analysis_07_cons_days_prior_to_mh_diagnosis
gen consdays_between_assigned = round(rgamma(0.3, 10) * (runiform()>0.25))
replace consdays_between_assigned = min(consdays_between_assigned, 220)

*------------------------------------------------------------------------------
**# 2. EXPAND TO THREE ROWS PER PERSON (rel_p = 0, 1, 2)
*------------------------------------------------------------------------------
expand 3
bysort person_id: gen byte rel_p = _n - 1

* Outcomes vary by row 
local i 0
foreach v of local outcomes {
    local ++i
    local m : word `i' of `base_means'
    local s : word `i' of `sds'
    local p : word `i' of `pany'
    draw_count `v', mean(`m') sd(`s') p(`p')
}

*------------------------------------------------------------------------------
**# 3. FOLLOW-UP DAYS (same construction as in the real data)
* Year window rel_p starts rel_p years after p0 and lasts one year
*------------------------------------------------------------------------------
bysort person_id: gen rel_year = year_p0 + rel_p
bysort person_id rel_p: gen date_p = birthday(p0, rel_year)
format date_p %td

gen p0_month = mofd(p0)
gen p0_day = day(p0)
gen next_year = rel_year + 1

gen yr_start = date_p
gen yr_end = mdy(p0_month, p0_day, next_year)-1

gen end_dataset = `end_dataset'
format yr_start yr_end end_dataset %td

bysort person_id rel_p: gen days_in_year_end_only = max(0, min(yr_end, end_dataset) - yr_start + 1)
format days_in_year_end_only %9.0g

*------------------------------------------------------------------------------
**# 4. CLEAN UP, CHECK, SAVE
*------------------------------------------------------------------------------
drop yr yr_start yr_end end_dataset rel_year date_p p0_month p0_day next_year

* Check: person-level variables must be constant within person_id
local personvars p0 year_p0 new_gp_municipality patient_sex patient_age patient_age_10y instrument_var exposure_var
foreach v of local outcomes {
    local personvars `personvars' `v'_t_minus_2
}
foreach v of local personvars {
    bysort person_id: assert `v' == `v'[1]
}
bysort person_id: assert _N == 3
isid person_id rel_p

order person_id p0 rel_p year_p0 patient_sex patient_age patient_age_10y instrument_var exposure_var days_in_year_end_only new_gp_municipality
sort person_id rel_p


compress
save "data/synthetic_analysis_dataset.dta", replace

********************************************************************************
