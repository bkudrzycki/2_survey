clear all

global projectfolder "/Users/kudrzycb/polybox/Youth Employment/1a Youth Survey"
global analysis "$projectfolder/Paper/Analysis"


use "/Users/kudrzycb/Desktop/Endline_analysis_new/endline.dta"

//drop random doublon
gen IDYouth = "CQP" + string(YE1_3) if YE1_2 == 1
replace IDYouth = "YS" + string(YE1_3) if YE1_2 == 0
sort IDYouth
by IDYouth: gen doublons=sum(1)
drop if doublons == 2

merge 1:1 IDYouth using "$analysis/Data/youth_survey_merged.dta", nogen keep(match master)

drop YE7* YE8* YE9* YE10* YS7* YS8* YS9* YS10* FS* F* COV*

foreach i of varlist _all {
local longlabel: var label `i'
local shortlabel = substr(`"`longlabel'"',1,79)
label var `i' `"`shortlabel'"'
}

save "/Users/kudrzycb/Desktop/Endline_analysis_new/endline_merged.dta", replace

quietly recode YE5* (2 = 0)

statplot YE5_1_1_1-YE5_1_1_10, over(SELECTED) varnames asyvars bar(1, fcolor(maroon)) bar(2, fcolor(navy))  legend(pos(3) col(1) lab(1 "Non-CQP") lab(2 "CQP")) title("Electricians' Self-Perception")


clear

use "$Analysis/Data/youth_survey_merged.dta"

quietly recode FS8_1* (2 = 0)

statplot FS8_12_1_1-FS8_12_1_10, over(SELECTED) varnames asyvars bar(1, fcolor(maroon)) bar(2, fcolor(navy))  legend(pos(3) col(1) lab(1 "Non-CQP") lab(2 "CQP")) title("Electricians' Patrons' Perception")

statplot FS8_13_1_1-FS8_13_1_15, over(SELECTED) varnames asyvars bar(1, fcolor(maroon)) bar(2, fcolor(navy))  legend(pos(3) col(1) lab(1 "Non-CQP") lab(2 "CQP")) title("Masonry: Competencies ")

statplot FS8_13_2_1-FS8_13_2_15, over(SELECTED) varnames asyvars bar(1, fcolor(maroon)) bar(2, fcolor(navy))  legend(pos(3) col(1) lab(1 "Non-CQP") lab(2 "CQP")) title("Masonry: Experience ")
