clear all
set more off

global path "/Users/kudrzycb/polybox/Youth Employment/1a Youth Survey"


***********************
****    Census    *****
***********************

use "$SourceData/ZD_sample_frame.dta"

gen activity = 1 if activite == "Ecolier, élève, ou étudiant" 
replace activity = 2 if activite == "Autre (cherche 1ère travail, ménagère, aide familiale, ou autre inactif)"
replace activity = 3 if activite == "Occupé indépendant"
replace activity = 4 if activite == "Occupé, salarié occasionnel" | activite == "Occupé, salarié permanent"
replace activity = 5 if activite == "Apprenti"

label define act1 1 "School" 2 "Other" 3 "Self-Employed" 4 "Wage Employed" 5 "Apprentice"
label values activity act1

cd "$latex/Tables"

estpost tabulate activity if age > 14 & age < 20
eststo m1
estpost tabulate activity if age > 19 & age < 30
eststo m2
estpost tabulate activity if age > 29
eststo m3
esttab m1 m2 m3 using census.tex, cell((b(fmt(g)) pct(fmt(2) par))) collabels(none) mtitle("Aged 15-19" "Aged 20-29" "Aged 30 and above") unstack noobs nonumber eqlabels(, lhs("Activity")) replace

***********************************************************************
***********************   General Analysis    *************************
***********************************************************************

clear all
do "$analysis/Do Files/data_loader"

cd "$latex/Tables"

********************   longitudinal analysis    ***********************

preserve
drop if cqp

cd "/Users/kudrzycb/polybox/Youth Employment/Thesis/tables/panel/Stata"

estpost tabulate status f1ustatus
esttab using transm01.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("Follow-Up 1") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Baseline")) replace

estpost tabulate f1ustatus f2ustatus
esttab using transm12.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("Follow-up 2") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Follow-up 1")) replace

estpost tabulate f2ustatus f3ustatus
esttab using transm23.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("Follow-up 3") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Follow-up 2")) replace

estpost tabulate f3ustatus f4ustatus
esttab using transm34.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("Endline") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Follow-up 3")) replace

estpost tabulate status f2ustatus
esttab using transm02.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("Endline") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Baseline")) replace

estpost tabulate status f3ustatus
esttab using transm03.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("Endline") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Baseline")) replace

estpost tabulate status f4ustatus
esttab using transm04.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("Endline") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Baseline")) replace

estpost tabulate f1ustatus f3ustatus
esttab using transm13.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("Endline") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Baseline")) replace

estpost tabulate f1ustatus f4ustatus
esttab using transm14.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("Endline") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Baseline")) replace

estpost tabulate f2ustatus f4ustatus
esttab using transm24.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("Endline") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Baseline")) replace

restore

cd "$latex/Tables"

//respondents by activity

label define gender 0 "Female" 1 "Male"
label values YS1_7 gender
estpost tabulate status YS1_7
esttab using activity.tex, cell((b(fmt(g)) colpct(fmt(2) par))) collabels(none) unstack noobs nonumber eqlabels(, lhs("Activity")) replace

// PAST

** childhood and religion **

gen bornc = (YS3_2_2 == 19 | YS3_2_2 == 57) //born in Abomey-Calavi (19) or Cotonou (57)

gen city = (YS3_3 == 4) // grew up in large city

gen christ = (YS3_5_1 == 1 | YS3_5_2 == 1 | YS3_5_3 == 1 | YS3_5_4 == 1) //christian

gen muslim = (YS3_5_5 == 1)

gen voudou = (YS3_5_6 == 1)

sort status
by status: eststo: quietly estpost summarize YS3_7 bornc city christ muslim voudou
esttab using demographics.tex, cells(mean(fmt(2))) collabels(none) nonumbers mtitle("School" "NEET" "Self" "Wage" "Apprentice")  unstack noobs alignment(c) rename(YS3_7 "Number of siblings" bornc "Born in city" city "Raised in city" christ "Christian" muslim "Muslim" voudou "Voudou") replace
eststo clear


//years of schooling (and apprenticeship, by respondent and parents) by activity

	//completed primary school
	
gen prim = (YS3_16 > 2 & YS3_16 != 10) //not "other"
replace prim = . if YS3_16 == 10

	//at least some secondary schooling

gen als = (YS3_16 > 3 & YS3_16 != 10) //not "other"
replace als = . if YS3_16 == 10

gen fals = (YS3_10 > 3 & YS3_10 != 10) //father
replace fals = . if YS3_10 == 10

gen mals = (YS3_12 > 3 & YS3_12 != 10) //mother
replace mals = . if YS3_12 == 10

	// has baccalauréat
gen bac = (YS3_17_6 == 1 )
replace bac = . if YS3_12 == 10

** education **

// to get relative frequencies, we have to do it variable-by-variable

cd "$latex/Tables/Custom/Temp"
foreach i of varlist YS3_13 prim als bac YS3_9 fals YS3_11 mals {
quietly estpost tabulate `i' status
esttab using var_`i'.tex, cell(colpct(fmt(2)) rowpct(fmt(2) par)) mtitle(Follow-Up) collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Baseline")) replace
}

sort status
by status: eststo: quietly estpost summarize YS3_13 prim als bac YS3_9 fals YS3_11 mals
esttab, cells(mean(fmt(2))) collabels(none) nonumbers mtitle("School" "NEET" "Self" "Wage" "Apprentice")  unstack noobs alignment(c) rename(YS3_13 "Completed apprenticeship" prim "Completed primary school" als "At least some secondary" bac "Completed secondary school" YS3_9 "Father completed apprenticeship " fals "Father at least some secondary" YS3_11 "Mother completed apprenticeship" mals "Mother at least some secondary") replace
eststo clear

by status: eststo: estpost summarize YS3_15
estpost tabstat YS3_15, by(status) stats(mean median) listwise
esttab using yos.tex, mtitle("School" "NEET" "Self" "Wage" "Apprentice") main(mean) aux(p50) nonumbers nostar unstack noobs nonote nodepvars rename(YS3_15 "_") replace
eststo clear

quietly estpost tabulate YS3_16 status, nototal
esttab using educ_by_status.tex, cell(colpct(fmt(2)) rowpct(fmt(2) par)) unstack label noobs varlabels(`e(labels)') collabels(none) nomtitles nonumbers replace
eststo clear


// PRESENT

** living conditions **

gen mar = (YS3_6 == 1)
gen livpar = (YS6_1 == 3)
gen hardfloor = (YS6_8_5 == 1 | YS6_8_6 == 1 | YS6_8_7 == 1 | YS6_8_9 == 1 | YS6_8_10 ==1)

// again we resort to relative frequencies

cd "$latex/Tables/Custom/Temp"
foreach i of varlist mar YS3_8 livpar hardfloor YS6_11_1 YS6_11_2 YS6_11_3 YS6_11_5 YS6_11_8 {
quietly estpost tabulate `i' status
esttab using var_`i'.tex, cell(colpct(fmt(2)) rowpct(fmt(2) par)) mtitle(Follow-Up) collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Baseline")) replace
}

sort status
by status: eststo: quietly estpost summarize baseline_age mar YS3_8 livpar hardfloor YS6_11_1 YS6_11_2 YS6_11_3 YS6_11_5 YS6_11_8
esttab, cells(mean(fmt(2))) collabels(none) nonumbers mtitle("School" "NEET" "Self" "Wage" "Apprentice")  unstack noobs alignment(c) rename(age "Mean age" mar "Married" YS3_8 "Number of children" livpar "Living with parents" hardfloor "Hard floor in home" YS6_11_1 "Own cell phone" YS6_11_2 "Own smartphone" YS6_11_3 "Own computer" YS6_11_5 "Own motorcycle" YS6_11_8 "Own TV") replace
eststo clear

** satisfaction **
gen sat = YS4_41 if status == 5
replace sat = YS7_18 if status == 1
replace sat = YS8_33 if status == 4
replace sat = YS9_34 if status == 3

sort status
by status: eststo: estpost summarize sat YS6_21
estpost tabstat YS6_21, by(status) statistics(mean)
esttab using lifesat.tex, mtitle("School" "NEET" "Self" "Wage" "Apprentice") main(mean) nonumbers nogaps nostar noobs wide nonote rename(sat "Satisfaction with activity" YS6_21 "Life Satisfaction") replace
eststo clear

// FUTURE

** plans after education/apprenticeship **

gen plans = 1 if YS4_53 == 1 | YS7_22 == 1 //look for a job
replace plans = 2 if YS4_53 == 2 | YS7_22 == 2 //start own business
replace plans = 3 if YS4_53 == 6 | YS7_22 == 4 //get (more) education
replace plans = 4 if YS4_53 == 7 | YS7_22 == 5 //get (another) apprenticeship
replace plans = 5 if YS4_53 == 3 //stay with the patron
replace plans = 6 if YS4_53 == 5 | YS4_53 == 8 | YS7_22 == 3 | YS7_22 == 8 | YS7_22 == 99 //other

label define planslbl 1 "Look for a job" 2 "Start own business" 3 "(More) education" 4 "(Another) apprenticeship" 5 "Stay with patron" 6 "Other"
label values plans planslbl

estpost tabulate plans status if status == 1 | status == 5
esttab using plans.tex, cell(b(fmt(g)) colpct(fmt(2) par)) collabels(none) unstack nonumbers nogaps nostar noobs nonote replace
eststo clear

** where do you see yourself in 5 years? **

gen fiveyrs = 0 if YS10_27 == 1
replace fiveyrs = 1 if YS8_36 == 1 //same employer
replace fiveyrs = 2 if YS8_36 == 2 | YS9_37 == 2 | YS10_27 == 3 //different/new employer
replace fiveyrs = 3 if YS8_36 == 3 | YS9_37 == 1 | YS10_27 == 2 //(still) self-employed
replace fiveyrs = 4 if YS8_36 == 5 | YS8_36 == 6 | YS9_37 == 4 | YS10_27 == 5 | YS10_27 == 6 //additional education or training
replace fiveyrs = 5 if YS8_36 == 7 | YS8_36 == 9 | YS9_37 == 6 | YS10_27 == 4 | YS10_27 == 7 //other

label define fiveyrslbl 0 "Still looking for work" 1 "Same employer" 2 "Different/new employer" 3 "(Still) self-employed" 4 "In education/training" 5 "Other"
label values fiveyrs fiveyrslbl

estpost tabulate fiveyrs status if status == 3 | status == 4 | status == 2
esttab using fiveyrs.tex, cell(b(fmt(g)) colpct(fmt(2) par)) collabels(none) unstack nonumbers nogaps nostar noobs nonote replace
eststo clear


// APPRENTICES

** trades **

label define trades 1 "Masonry" 2 "Carpentry" 3 "Plumbing" 4 "Metal construction" 5 "Electrical installation" 6 "Other"
label values YS4_3 trades
estpost tabulate YS4_3
esttab using trades.tex, cell(b(fmt(g)) rowpct(fmt(2) par)) collabels(none) wide noobs nonumber rename(YS4_3 "Number") replace

estpost summarize YS4_4 if YS4_4 < 51
esttab using numapps.tex, cells(mean(fmt(2))) collabels(none) unstack noobs nomtitle rename(YS4_4 "Number of apprentices") replace
eststo clear

estpost summarize YS4_5 if YS4_5 <51
esttab using appcompsize.tex, cells(mean(fmt(2))) collabels(none) unstack noobs nomtitle rename(YS4_5 "Firm size") replace
eststo clear

// IN SCHOOL

** subject of study **

gen subj = 1 if YS7_4_1 == 1
replace subj = 2 if YS7_4_2 == 1
replace subj = 3 if YS7_4_3 == 1
replace subj = 4 if YS7_4_4 == 1
replace subj = 5 if YS7_4_5 == 1
replace subj = 6 if YS7_4_6 == 1
replace subj = 7 if YS7_4_7 == 1
replace subj = 8 if YS7_4_8 == 1
replace subj = 2 if YS7_4_9 == 1

label define subjlbl 1 "Humanities" 2 "Agriculture" 3 "Education" 4 "Social Sciences" 5 "Medicine" 6 "Business and Econ" 7 "Science and Tech" 8 "Politics, Law, and Admin" 9 "Other"
label values subj subjlbl

estpost tabulate subj
esttab using subject.tex, cells((b(fmt(g)) pct(fmt(2) par))) collabels(none) mtitle("Students") wide noobs nonumber replace


// WAGE WORKERS

egen dayswked = rowtotal(YS8_17 YS8_18)
gen workweek = dayswked * YS8_19 if YS8_19 > 0 & YS8_19 < 16

estpost tabulate YS8_10 YS8_11
esttab using contracts.tex, cell(b(fmt(g)) rowpct(fmt(2) par)) collabels(none) mtitle("Contract Duraction") unstack nonumbers nogaps nostar noobs nonote eqlabels(, lhs("Contract Type"))  replace
eststo clear



// SELF EMPLOYED

estpost summarize YS9_29_1 YS9_29_2 YS9_29_3 YS9_29_4 YS9_29_5 YS9_29_6
esttab using obstacles.tex, cells(mean(fmt(2))) mtitle("Average Rank") collabels(none) nonumbers unstack noobs rename(YS9_29_1 "Poor access to electricity" YS9_29_2 "Competition" YS9_29_3 "Poor access to credit" YS9_29_4 "Lack of qualified labor" YS9_29_5 "Lack of motivated apprentices" YS9_29_6 "Government bureaucracy") replace
eststo clear


// NEET

** obstacles to finding work **

gen obst = 1 if YS10_14_1 == 1
replace obst = 2 if YS10_14_2 == 1
replace obst = 3 if YS10_14_3 == 1
replace obst = 4 if YS10_14_4 == 1
replace obst = 5 if YS10_14_5 == 1
replace obst = 6 if YS10_14_6 == 1
replace obst = 7 if YS10_14_7 == 1
replace obst = 8 if YS10_14_8 == 1
replace obst = 9 if YS10_14_9 == 1

label define obstlbl 1 "Lack of work experience" 2 "Lack of training" 3 "Too few jobs" 4 "Considered too young" 5 "Gender/Discrimination" 6 "Low wages" 7 "Poor working conditions" 8 "Don't know where to look" 9  "Other"
label values obst obstlbl

estpost tabulate obst
esttab using neetobst.tex, cell(b(fmt(g)) pct(fmt(2) par)) collabels(none) mtitle("Number of Responses") wide noobs nonumber replace

//tabulate characteristics of NEET vs others

replace city = (YS3_3==4) if !missing(YS3_3) //grew up in big city
gen Female = (YS1_7==0)
gen Siblings = (YS3_7<11) //change "more than 11 siblings" to missing
gen father_sec = (YS3_10>3) & (YS3_10<10) //father compleated at least some secondary education
gen mother_sec = (YS3_12>3) & (YS3_12<10) //mother compleated at least some secondary education
gen father_app = YS3_9
gen mother_app = YS3_11
gen NEET = (status==2)
gen school = (status==1)
gen wage = (status==3)
gen self = (status==4)
gen app = (status==5)


gen ethnicity = 1 if YS3_4_4 == 1 
replace ethnicity = 2 if YS3_4_8 == 1
replace ethnicity = 3 if (YS3_4_6==1) | (YS3_4_6==2) | (YS3_4_6==3) | (YS3_4_6==5) | (YS3_4_6==6) | (YS3_4_7==1) | (YS3_4_9==1)
label define ethnlab 1 "Fon" 2 "Yoruba" 3 "Other Ethnicity"
label values ethnicity ethnlab

gen schooling = 0 if YS3_16 == 0
replace schooling = 1 if YS3_16 == 3 // completed primary
replace schooling = 2 if YS3_16 == 4 | YS3_16 == 6 
replace schooling = 3 if YS3_16 == 5 | YS3_16 == 7
replace schooling = 4 if YS3_16 == 8 | YS3_16 == 9
label define schlab 0 "None" 1 "Primary" 2 "Secondary (1st cycle)" 3 "Secondary (2nd cycle)" 4 "Tertiary"
label values schooling schlab

gen cert = 0 if YS3_17_0 == 1
replace cert = 1 if YS3_17_2 == 1
replace cert = 2 if YS3_17_4 == 1
replace cert = 3 if YS3_17_6 == 1 
replace cert = 4 if YS3_17_7 == 1 | YS3_17_8 == 1 | YS3_17_9 == 1 | YS3_17_10 == 1 | YS3_17_14 == 1
replace cert = 5 if YS3_17_11 == 1
replace cert = 6 if YS3_17_12 == 1
label define certlab 0 "No Diplomas" 1 "Primary Diploma CEP"  2 "Secondary School, 1st Cycle BEPC" 3 "Baccalauréat BAC" 4 "Professional Aptitude CAP/AQP/ProBAC/EFAT/Other" 5 "Bachelor" 6 "Master"
label values cert certlab

foreach i of varlist YS3_17* {
tab `i'
}

foreach i of varlist YS3_4_1-YS3_4_9 {
replace `i' = 0 if missing(`i')
}

foreach i of varlist YS3_17_0-YS3_17_14 {
replace `i' = 0 if missing(`i')
}

gen Fon = (ethnicity == 1)
gen Bacc = (YS3_17_6 == 1)

preserve
gen neettl = (NEET == 0)
label define neetlab 0 "NEET" 1 "Other"
label values neettl neetlab

estpost tabstat baseline_age Female Siblings city Fon Bacc YS3_13 YS3_15, by(neettl) ///
	statistics(mean sd) columns(statistics) nototal listwise

esttab using neetstats.tex, main(mean) aux(sd) varlabels(baseline_age Age Bacc "Baccalauréat" city "Raised in City" YS3_13 "Completed apprenticeship" YS3_15 "Years of schooling" Fon "Ethnicity: Fon" Siblings "No. of Siblings") nomtitles unstack noobs nonumber nostar replace

eststo clear

esttab using neetdays.tex, cell("P_1(fmt(3) label(NEET)) P_2(fmt(3) label(Non-NEET)) b(fmt(3) star label(Diff.)) p(fmt(3) label(p-value))") varlabels(YS6_18_1 "Newspaper" YS6_18_2 "Book" YS6_18_3 "Radio" YS6_18_4 "Television" YS6_18_5 "Internet - search" YS6_18_6 "Internet - purchase" YS6_18_7 "Internet - job" YS6_18_8 "Facebook" YS6_18_9 "Whatsapp" YS6_18_10 "Twitter") noobs nonumber wide replace

restore



//regression analysis of status


eststo: quietly logit NEET baseline_age Female Siblings city father_sec mother_sec father_app mother_app i.ethnicity
eststo: quietly logit NEET baseline_age Female Siblings city father_sec mother_sec father_app mother_app i.ethnicity YS3_13 YS3_15 if YS3_13<99
eststo: quietly logit NEET baseline_age Female Siblings city father_sec mother_sec father_app mother_app i.ethnicity YS3_13 i.schooling if YS3_13<99
eststo: quietly logit NEET baseline_age Female Siblings city father_sec mother_sec father_app mother_app i.ethnicity YS3_13 i.cert if YS3_13<99
eststo: quietly logit NEET baseline_age Female Siblings city father_sec mother_sec father_app mother_app i.ethnicity YS3_13 i.cert if YS3_13<99 & YS1_2 == 0
esttab using neetlogit.tex, label title(Determinants of Youth NEET \label{tab:neetlogit1}) varlabels(age Age city "Raised in City" father_sec "Father Some Secondary" mother_sec "Mother Some Secondary" father_app "Father Apprenticeship" mother_app "Mother Apprenticeship" YS3_13 "Completed apprenticeship" YS3_15 "Years of schooling") eqlabels(none) nomtitles eform replace
eststo clear



// FOLLOW-UP

cd "$latex/Tables/"

estpost tabulate status f1ustatus
esttab using transm.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle(Follow-Up) collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Baseline")) replace

estpost tabulate f1ustatus f2ustatus
esttab using transm12.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("Follow-up 2") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Follow-up 1")) replace

estpost tabulate status f2ustatus
esttab, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("Follow-up 2") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Baseline"))


// daily activities (day in life)
foreach var of varlist YS6_18* {
replace `var' = . if `var' > 1
}

sort status
by status: eststo: quietly estpost summarize YS6_18_1 YS6_18_2 YS6_18_3 YS6_18_4 YS6_18_5 YS6_18_6 YS6_18_7 YS6_18_8 YS6_18_9 YS6_18_10
esttab using dayinlife.tex, cells(mean(fmt(2))) collabels(none) unstack noobs nomtitle eqlabels(`e(eqlabels)') replace
eststo clear

// tracks (3 surveys)
gen workingbl = (YS8_1 | YS8_2)
gen workingf1 = (F1U1_14 | F1U1_15)
gen workingf2 = (F2U1_14 | F2U1_15)


egen tracks = concat(status f1ustatus f2ustatus)
egen complete_tracks = concat(status f1ustatus f2ustatus) if !missing(status) & !missing(f1ustatus) & !missing(f2ustatus)

gen changetracks = (status != f1ustatus) if !missing(status) & !missing(f1ustatus)
replace changetracks = (status != f2ustatus) if !missing(status) & !missing(f2ustatus)
replace changetracks = (f1ustatus != f2ustatus) if !missing(f1ustatus) & !missing(f2ustatus)

gen neet2work = ((status == 2 & f1ustatus == 4) | (f1ustatus == 2 & f2ustatus == 4) | (status == 2 & f1ustatus == 3) | (f1ustatus == 2 & f2ustatus == 3))

gen everneet = (status == 2 | f1ustatus == 2 | f2ustatus == 2)
