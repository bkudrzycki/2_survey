clear all
set more off

cd "$WorkingData"

***********************************************************************
********************   Merge with Firm Data   *************************
***********************************************************************

//Start with dossier data as master document as patron and apprentice IDs are already matched
use "$WorkingData/dossiers_clean"

//merge youth dossiers with patron data
merge m:1 IDPatron using firm_clean, nogen keep(match master)

//merge youth dossiers with youth survey data
merge m:m IDYouth using youth_clean
drop if _merge == 1 // drop youth on original list who could not be contacted and were substituted by other youth
drop _merge

****************************************************************
**merge firm questions on specific apprentices into single variable (Johanna)

*recode FS1_8_7 (string) as int
replace FS1_8_7= "1" if FS1_8_7=="Prince Gabin"&YS1_3==1
replace FS1_8_7= "208" if FS1_8_7=="Prince Gabin"&YS1_3==208
replace FS1_8_7= "211" if FS1_8_7=="Prince Gabin"&YS1_3==211
destring FS1_8_7, replace

quietly destring, replace

*rename FS8_2, FS8_3
rename FS8_3 FS8_3_2
rename FS8_2 FS8_2_3

rename FS8_3_2 FS8_2
rename FS8_2_3 FS8_3

*generate variables for one-column versions
forvalues n=2/4{
gen FS_`n'=.
}
foreach n in 5 6 7 8 11 {
gen FS_`n'_1=.
gen FS_`n'_2=.
gen FS_`n'_3=.
gen FS_`n'_4=.
gen FS_`n'_5=.
gen FS_`n'_6=.
gen FS_`n'_7=.
}

forvalues n=2/4{
foreach val in 1 4 5 6 7{
replace FS_`n' = FS8_`n' if FS1_5==YS1_3
replace FS_`n' = A`val'_FS9_`n' if FS1_8_`val'==YS1_3
}
}

foreach n in 5 {
foreach val in 1 4 5 6 7{
replace FS_`n'_1 = FS8_`n'_1 if FS1_5==YS1_3
replace FS_`n'_3 = FS8_`n'_3 if FS1_5==YS1_3
replace FS_`n'_5 = FS8_`n'_5 if FS1_5==YS1_3
replace FS_`n'_7 = FS8_`n'_7 if FS1_5==YS1_3

replace FS_`n'_1 = A`val'_FS9_`n'_1 if FS1_8_`val'==YS1_3
replace FS_`n'_3 = A`val'_FS9_`n'_3 if FS1_8_`val'==YS1_3
replace FS_`n'_5 = A`val'_FS9_`n'_5 if FS1_8_`val'==YS1_3
replace FS_`n'_7 = A`val'_FS9_`n'_7 if FS1_8_`val'==YS1_3

}
}
 
foreach n in  6 7 8 11 {
foreach val in 1 4 5 6 7{
replace FS_`n'_1 = FS8_`n'_1 if FS1_5==YS1_3
replace FS_`n'_2 = FS8_`n'_2 if FS1_5==YS1_3
replace FS_`n'_3 = FS8_`n'_3 if FS1_5==YS1_3
replace FS_`n'_1= A`val'_FS9_`n'_1 if FS1_8_`val'==YS1_3
replace FS_`n'_2 = A`val'_FS9_`n'_2 if FS1_8_`val'==YS1_3
replace FS_`n'_3 = A`val'_FS9_`n'_3 if FS1_8_`val'==YS1_3
}
} 

foreach n in  6 11 {
foreach val in 1 4 5 6 7{
replace FS_`n'_4 = FS8_`n'_4 if FS1_5==YS1_3
replace FS_`n'_5 = FS8_`n'_5 if FS1_5==YS1_3
replace FS_`n'_4= A`val'_FS9_`n'_4 if FS1_8_`val'==YS1_3
replace FS_`n'_5 = A`val'_FS9_`n'_5 if FS1_8_`val'==YS1_3
}
} 

foreach n in  11 {
foreach val in 1 4 5 6 7{
replace FS_`n'_6 = FS8_`n'_6 if FS1_5==YS1_3
replace FS_`n'_6= A`val'_FS9_`n'_6 if FS1_8_`val'==YS1_3
}
} 

// append variable labels
foreach v of varlist A1_FS9_* {
	local x = substr("`v'", 8, .)
    local var_lblFS_`x': var label `v'
}

foreach var of varlist FS_* {
        label var `var' "`var_lbl`var''"
}

// append value labels
foreach n in  2 3 4{
foreach v of varlist FS_`n'* {
    local y = substr("`v'", 4, .)
	local lblname: value label A1_FS9_`y'
	capture confirm string variable `v'
	if _rc {
	label val FS_`y' `lblname'
}	
}
}

 // drop original A*_FS7* variables 
foreach i in 1 3 4 5 6 {
		drop A`i'_FS7*
}
 
***********************************************************************
*******************   Merge with Census Data   ************************
***********************************************************************

merge 1:m IDYouth using "$SourceData/ZD_sample_frame_linked"
drop if _merge == 2
drop _merge LocationLatitude LocationLongitude RecordedDate
rename age censusage

***********************************************************************
********************   Merge with Follow-up   *************************
***********************************************************************

merge 1:1 IDYouth using follow_up_clean, force
drop if _merge == 2 //drop youth found in the follow-up survey data only (no baseline data)
drop _merge


***********************************************************************
********************   Merge with Follow-up 2  ************************
***********************************************************************


merge 1:1 IDYouth using follow_up2, force
drop _merge

***********************************************************************
********************   Merge with Follow-up 3  ************************
***********************************************************************


merge 1:1 IDYouth using follow_up3, force
drop _merge


***********************************************************************
**********************   Merge with Endline  **************************
***********************************************************************

merge 1:1 IDYouth using endline, force
drop _merge

***************************   Clean up  *******************************

order IDYouth IDPatron YS_ResponseId YS_duration YS_LocationLatitude YS_LocationLongitude YS* FS_ResponseId FS_StartDate FS_EndDate FS_RecordedDate FS_duration FS_LocationLatitude FS_LocationLongitude FS1* FS2* FS3* FS4* FS5* FS6* FS8* A* FS_2* FS_3* FS_4* FS_5* FS_6* FS_7* FS_11* F1U_ResponseId F1U_StartDate F1U_EndDate F1U_RecordedDate F1U_duration F1U_LocationLatitude F1U_LocationLongitude F1U* F2U_ResponseId F2U_StartDate F2U_EndDate F2U_RecordedDate F2U_duration F2U_LocationLatitude F2U_LocationLongitude F2U* F3U_ResponseId F3U_StartDate F3U_EndDate F3U_RecordedDate F3U_duration F3U_LocationLatitude F3U_LocationLongitude F3U*

drop CQP Matching_name A_no_name_ Q4_2_10_TEXT___Topics Status IPAddress Progress Finished RecipientLastName RecipientFirstName RecipientEmail ExternalReference DistributionChannel UserLanguage

save youth_survey_merged, replace

