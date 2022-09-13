clear all
set more off

* set user path: e.g. /Volumes/nadel/research/Data/PhDs/Bart 2022/Paper 3 - CQP

global projectfolder "~/polybox/Youth Employment/1a Youth Survey/Markdown"


* Project folder globals
* ---------------------
global SourceData         	"$projectfolder/data/source"
global WorkingData          "$projectfolder/data/stata"

****    Table 1: Census of 13 zones de dénombrement   *****

use "$SourceData/ZD_sample_frame.dta"

gen activity = 1 if activite == "Ecolier, élève, ou étudiant" 
replace activity = 2 if activite == "Autre (cherche 1ère travail, ménagère, aide familiale, ou autre inactif)"
replace activity = 3 if activite == "Occupé indépendant"
replace activity = 4 if activite == "Occupé, salarié occasionnel" | activite == "Occupé, salarié permanent"
replace activity = 5 if activite == "Apprenti"

label define act1 1 "School" 2 "Other" 3 "Self-Employed" 4 "Wage Employed" 5 "Apprentice"
label values activity act1

cd "$projectfolder/markdown/tables/replication"

estpost tabulate activity if age > 14 & age < 20
eststo m1
estpost tabulate activity if age > 19 & age < 30
eststo m2
estpost tabulate activity if age > 29
eststo m3
esttab m1 m2 m3 using census.tex, cell((b(fmt(g)) pct(fmt(2) par))) booktabs collabels(none) mtitle("Aged 15-19" "Aged 20-29" "Aged 30 and above") addnote("\textit{Note:} Percentage of age group in parentheses.") title("Census of 13 zones de denombrement \label{tab:tbl-census}") alignment(ll) unstack noobs nonumber eqlabels(, lhs("Activity")) replace


