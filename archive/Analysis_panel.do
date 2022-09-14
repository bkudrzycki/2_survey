clear all
set more off

global path "/Users/kudrzycb/polybox/Youth Employment/1a Youth Survey"

// load data
do "$analysis/Do Files/panel_loader"

// drop cqps
drop if cqp
drop if missing(YS1_1)

// filter age
drop if baseline_age < 20 | baseline_age > 29

// new ids
egen id = group(IDYouth)

global id id
global t wave
global xtimeinvar covid lockdown 
global xlist age age2 yos bac 

// set as panel data
sort $id $t
xtset $id $t

// generate status lagged by one period
gen lagged_status = L1.status
label values lagged_status statuslabs

cd "/Users/kudrzycb/polybox/Youth Employment/Thesis/tables/panel/Stata"

// define var if youth transitioned to wage employment or work (self- or wage employment)
gen emp_trans = (status == 3 & (lagged_status == 1 | lagged_status == 2 | lagged_status == 5))
gen work_trans = ((status == 3 | status == 4) & (lagged_status == 1 | lagged_status == 2 | lagged_status == 5))

// transition matrix for entire survey
estpost tabulate lagged_status status 
esttab using transm.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("To") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("From")) replace




xtsum $id $t $ylist $xlist emp_trans work_trans

// fixed-effects ordered logit regression of status on life satisfaction

feologit life_sat $xlist i.status ib2.status, or baselevels

eststo: meologit life_sat age age2 sex i.status ib2.status || arrond:, or baselevels


xtset $id $t
xtologit life_sat age age2 sex i.status ib2.status, vce(cluster arrond)

meologit life_sat age age2 sex i.status ib2.status || arrond:, or baselevels

// pooled OLS
reg $ylist $xlist $controls

// population-averaged estimator
xtreg $ylist $xlist, pa

// between estimator
xtreg $ylist $xlist $controls, be

// fixed effects (within) estimator
xtreg $ylist $xlist, fe

// first-differences estimator
reg D.($ylist $xlist), noconstant

xtreg $ylist $xlist, re theta



// transition matrix for past seven years

clear all

import delimited "/Users/kudrzycb/polybox/Youth Employment/1a Youth Survey/Paper/Analysis/Data/past_activities.csv", encoding(ISO-8859-1)

drop if activity == "NA"


// inactive = 0
// school = 1:3


egen id = group(idyouth)
gen activite = real(activity)
drop activity

gen activity = 1 if activite == 1 | activite == 2 | activite == 3
replace activity = 2 if activite == 0 |  activite == 8 | activite == 99
replace activity = 3 if activite == 7
replace activity = 5 if activite == 4 | activite == 5
replace activity = 4 if activite == 6


label define act1 1 "School" 2 "NEET" 3 "Self-Employed" 4 "Wage Employed" 5 "Apprentice"
label values activity act1

global id id
global t year

sort $id $t
xtset $id $t

gen lagged_act = L1.activity
label values lagged_act act1

cd "/Users/kudrzycb/polybox/Youth Employment/Thesis/tables/panel/Stata"

estpost tabulate lagged_act activity 
esttab using pasttransm.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("To") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("From")) replace

preserve

drop if sex == 1
estpost tabulate lagged_act activity 
esttab using pasttransm_female.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("To") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("From")) replace

restore

preserve

drop if sex == 0
estpost tabulate lagged_act activity 
esttab using pasttransm_male.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("To") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("From")) replace

restore

// Transition propensity

tab activity lagged_act, matcell(x)
tab lagged_act activity, matcell(y)
matrix z = x + y
matrix r = J(5,5,0)


forvalues i = 1/5 {
	forvalues j = 1/5 {
		 matrix r[`i',`j']= x[`i',`j']/z[`i',`j']
	}
}

matrix r = r-(I(5)*.5)
matrix colnames r = School NEET Self-Employment "Wage Employment" Apprentice
matrix rownames r = School NEET Self-Employment "Wage Employment" Apprentice

matrix list r

esttab matrix(r) using propensity.tex, replace




