clear all
set more off

*********************************************************************
***************************   Cleaning   ****************************
*********************************************************************

///Create sample frame dataset to use for appending IDPatron

use "$SourceData/dossiers_sample_frame.dta"

replace NApprenti = 1745 if NOM_et_PRENOMS_APPRENTI == "KEKEREGUE AUGUSTINO D." //this should have been 745. that number is already taken by another youth

gen IDYouth = "CQP" + string(NApprenti)
label var IDYouth "Youth N°"
order IDYouth
sort IDYouth

rename NAtelier IDPatron
label var IDPatron "Workshop N°"

//if youth appear twice in sample frame, keep only those that are actually found in the survey data. This occurs because in the replacements spreadsheet, some replacement IDs were replaced but not marked as such.

drop if NOM_et_PRENOMS_APPRENTI == "AMADA FRANCK" // CQP647

drop if NOM_et_PRENOMS_APPRENTI == "SOUNOUVI GÉRAUD" // CQP682

drop if NOM_et_PRENOMS_APPRENTI ==  "KOUTHON SEVERIN" // CQP 735

drop if NOM_et_PRENOMS_APPRENTI == "TOGBEHOUNDE CASMIR" //CQP736

drop if IDYouth == "CQP." | IDYouth == "CQP645" | IDYouth == "CQP646" //neither of the youth are found in the baseline

drop A* B* C*

save "$WorkingData/dossiers_clean", replace

clear all

// Youth Survey

use "$SourceData/Enquête+des+jeunes_June+23,+2020_15.39.dta"

// generate birthdate

//use YS2_1 answer to replace birth year if YS2_10_3_1 == 9999
replace YS2_10_3_1 = 2019-YS2_1 if YS2_10_3_1 == 9999 & YS2_1 != 19 & YS2_1 != 30

//replace Crésus' responses that were date 2014 with the mode survey date (Aug 15th 2019)
replace StartDate = td(15Aug2019) if StartDate == td(01Jan2014)

//original age variable. replaced by more accurate one
*gen age = 2019-YS2_10_3_1 if YS2_10_3_1 < 2012
*label var age "Age: 2019 - birth year"

// generate birthdate in %td formate
egen x = concat(YS2_10_1_1 YS2_10_2_1 YS2_10_3_1), punct(" ")
gen birthdate = date(x, "DMY")
drop x
format birthdate %td

// generate age at baseline
gen baseline_age = floor((StartDate - birthdate) / 365)
label var baseline_age "Age at baseline"

// encode activity at baseline
gen status = 1 if YS7_1 == 1 //education
replace status = 3 if YS8_4 == 4 //self-employed
*replace status = 4 if (YS8_1 == 1 | YS8_2 == 1) & YS8_4 != 4 //working
replace status = 4 if YS8_4 == 1 | YS8_4  == 2| YS8_4  == 3 | YS8_4  == 5 | YS8_4  == 6 | YS8_4  == 8 //working
replace status = 5 if YS1_2 == 1 | YS4_1 == 1 //app
*** label status ***
replace status = 2 if missing(status)
gen baseline_activity = status
label define baseactlabs 1 "School" 2 "NEET" 3 "Self-Employed" 4 "Wage Employed" 5 "Apprentice"
label values baseline_activity baseactlabs
label var baseline_activity "Activity at baseline"
drop status

gen sex = YS1_7
label var sex "Sex of respondent"
// indicate if part of cqp sample at baseline

gen cqp = YS1_2
label var cqp "In CQP sample"

gen formapp = YS3_13
label var formapp "Formerly apprentice"

gen siblings = YS3_7
label var siblings "No. of Siblings"

replace YS9_1 = 1 if !missing(YS9_2) // question 9_1 was not forced, leading to some missed observations

drop if YS2_3 == 0 //chose not to participate
drop if Duration__in_seconds_ < 120 // interview lasting less than 2 minutes
drop if YS1_1 == 11 //Rubain (single practice interview)


//add some missing variable labels
label var YS6_6 "How many people live in the house together with you (NOT including you)?"
label var YS8_28 "Would you like to work more hours than you currently do per week (if this meant that you would also earn more)?"

//fix answers to question "How old are you?" (posed to CQP apprentices only)
replace YS2_1 = YS2_1 + 18

label define agelabs 19 "younger than 20" 30 "older than 29"
label values YS2_1 agelabs

*** Get rid of observations that do not match name on list ***
//Armel
drop if YS1_3==140 & YS1_2==1
drop if YS1_3==57 & YS1_2==0
drop if YS1_3==74 & YS1_2==0
drop if YS1_3==281 & YS1_2==0
drop if YS1_3==332 & YS1_2==0
drop if YS1_3==543 & YS1_2==0
drop if YS1_3==574 & YS1_2==0
drop if YS1_3==589 & YS1_2==0
drop if YS1_3==602 & YS1_2==0
drop if YS1_3==626 & YS1_2==0
drop if YS1_3==631 & YS1_2==0
drop if YS1_3==655 & YS1_2==0
drop if YS1_3==661 & YS1_2==0
drop if YS1_3==679 & YS1_2==0
drop if YS1_3==705 & YS1_2==0
drop if YS1_3==728 & YS1_2==0
drop if YS1_3==762 & YS1_2==0
drop if YS1_3==806 & YS1_2==0
drop if YS1_3==818 & YS1_2==0
drop if YS1_3==822 & YS1_2==0
drop if YS1_3==823 & YS1_2==0
drop if YS1_3==824 & YS1_2==0
drop if YS1_3==831 & YS1_2==0
drop if YS1_3==843 & YS1_2==0
drop if YS1_3==850 & YS1_2==0
drop if YS1_3==855 & YS1_2==0
drop if YS1_3==859 & YS1_2==0
drop if YS1_3==864 & YS1_2==0
drop if YS1_3==866 & YS1_2==0
drop if YS1_3==872 & YS1_2==0
drop if YS1_3==878 & YS1_2==0
drop if YS1_3==885 & YS1_2==0
drop if YS1_3==169 & YS1_2==0 & YS1_1 == 1
drop if YS1_3==1531  & YS1_2==0
drop if YS1_3==1560 & YS1_2==0
drop if YS1_3==1904 & YS1_2==0

drop if ResponseId == "R_j6W9cYZQbye4myg" // Armel interviewed two people with the same name but two IDs: YS246 and YS1305	LOKOSSOU MIRELLE.

//Ambroise
drop if YS1_3==768 & YS1_2==1
drop if YS1_3==929 & YS1_2==0 & YS1_1 == 6
drop if YS1_3==942 & YS1_2==0 & YS1_1 == 6
drop if YS1_3==1014 & YS1_2==0
drop if YS1_3==1045 & YS1_2==0
//Appolinaire
drop if YS1_3==41 & YS1_2==1
drop if YS1_3==267 & YS1_2==0
drop if YS1_3 == 987 & YS2_9_1 == "Florentin"
drop if YS1_3==10268 & YS1_2==0
drop if YS1_3==17422 & YS1_2==0
drop if ResponseId == "R_bUYhzS2TMa48qPe" // YS1036
//Crésus
drop if YS1_3==134 & YS1_2==0
drop if YS1_3==162 & YS1_2==0
//Emmanuel
drop if YS1_3==193 & YS1_2==0
drop if YS1_3==2332 & YS1_2==0
drop if YS1_3==7076 & YS1_2==0
drop if YS1_3==10211 & YS1_2==0
drop if YS1_3==10458 & YS1_2==0
drop if YS1_3==11329 & YS1_2==0
drop if YS1_3==13941 & YS1_2==0
drop if YS1_3==14057 & YS1_2==0
drop if YS1_3==16984 & YS1_2==0
//Floriane
drop if YS1_3==966 & YS1_2==0
drop if YS1_3==1179 & YS1_2==0
drop if YS1_3==714 & YS1_2==0
drop if YS1_3==887 & YS1_2==0
drop if YS1_3==1232 & YS1_2==0
//Nadège
drop if YS1_3==216 & YS1_2==0
drop if YS1_3==259 & YS1_2==0
drop if YS1_3==1619 & YS1_2==0
drop if YS1_3==1667 & YS1_2==0
//Olayide
drop if YS1_3==1700 & YS1_2==0
drop if YS1_3==15649 & YS1_2==0

//Fiacre
drop if ResponseId == "R_fwiLvz89gif4agj" // Fiacre had her for the pilot, but then she was transferred to Serge

//Serge
drop if ResponseId == "R_oO7mKGrnV61B2t8" // Serge had him but he was interviewed by Emmanuel as well; Emmanuel ended up taking him on for round 2

//Drop one observation if an ID appears twice:

drop if ResponseId == "R_iEckZdZlzUUNnwj" //Armel, was not supposed to interview CQP11
drop if ResponseId == "R_mgur5UGCMykB79S" //Armel interviewed YS1217 twice: keep exact match with list
drop if ResponseId == "R_ejjpBKC9keZ5V4J" //Emmanuel interviewed CQP55, assigned to and interviewed by Olayidé
drop if ResponseId == "R_mfV8SuLmHvPfUmZ" //Olayidé restarted interview with YS338 after 3 minutes
drop if ResponseId == "R_bCAALUmQByzBkoX" //Floriane interviewed CQP55, assigned to and interviewed by Nadège

gen IDYouth = "CQP" + string(YS1_3) if YS1_2 == 1 //generate youth ID - merging variable
replace IDYouth = "YS" + string(YS1_3) if YS1_2 == 0
order IDYouth

drop if IDYouth == "YS16" | IDYouth == "YS190" | IDYouth == "YS208" | IDYouth == "YS273" | IDYouth == "YS295" | IDYouth == "YS352" | IDYouth == "YS428" | IDYouth == "YS678" |  IDYouth == "YS720" | IDYouth == "YS742"  //blank observations

drop if IDYouth == "YS1769" //Sylvain refers to a previous discussion that led to us dropping this youth. reason unclear

//drop youth who were interviewed twice by different surveyors
drop if IDYouth == "YS805" //Boko/Doko Alice
drop if IDYouth == "YS2365" //Degila Gllfas
drop if IDYouth == "YS10631" //GBEDJI  Lucienne
drop if IDYouth == "5613" // Mignonnou Anne Marie

//differentiate between follow-up and baseline metadata
rename (ResponseId LocationLatitude LocationLongitude Duration__in_seconds_ StartDate EndDate RecordedDate) (YS_ResponseId YS_LocationLatitude YS_LocationLongitude YS_duration YS_StartDate YS_EndDate YS_RecordedDate)

cd "$WorkingData"
save youth_clean, replace

//Firm Survey

use "$SourceData/Enquête+auprès+des+patrons_February+10,+2020_13.28.dta"

//Drop one observation if an ID appears multiple times:
drop if ResponseId == "R_dFFOYWTJpZOXC9i" //name appears twice, with *almost* identical responses. keep longer interview
drop if ResponseId == "R_hyakfjosgUhPpiE" //name appears twice. drop 5-minute version
drop if ResponseId == "R_gJd9V4L9g97AxQy" | ResponseId == "R_jbEBd8HUMsSN2iK" //ID repeated 3 times, dropped false starts
drop if ResponseId == "R_iOfqtFybU4olYI5" //Keep interview with responses for ALL concerned apprentices (not just one)
drop if ResponseId == "R_nbzhLY05d1u5kJT" //double, drop Armel
drop if ResponseId == "R_ij6WUVJtQYIuTLU" //drop double in which apprentices don't match firms in dossier data
drop if ResponseId == "R_37TwKLZBzMS8azS" //empty observation
replace FS1_2 = 30 if ResponseId == "R_g6CvEE99F02nSCb" //correct patron's ID

drop FS6_19_1___Topics

gen IDPatron = FS1_2 //generate Patron ID
order IDPatron

rename (Duration__in_seconds_ ResponseId LocationLatitude LocationLongitude StartDate EndDate RecordedDate) (FS_duration FS_ResponseId FS_LocationLatitude FS_LocationLongitude FS_StartDate FS_EndDate FS_RecordedDate)

save firm_clean, replace

//Follow-up Survey

use "$SourceData/Enquête+de+suivi,+cycle+1_July+29,+2020_09.55.dta"

drop if Matching_name == 0 //drop follow-ups that don't match youth from the original sample

//drop BOTH observations if combination of ID number and name appears twice. Will follow-up with surveyors.
drop if ResponseId == "R_8HiEh4NwaHbJGEO" //Repeated CQP228 by Nadège, drop first instance (24h version, vs 16m)
drop if ResponseId == "R_3c8m62LeQx8950E" //Repeated CQP255 by Nadège, drop first instance (chronologically)
drop if ResponseId == "R_1u1zDHeT49astiu" //CQP55 interviewed by Olayidé first, then Emmanuel. Drop latter
drop if ResponseId == "R_13ZTxQzHsVeyRE0" //Repeated YS1217 by Armel, keep instance that matches original name from list
drop if ResponseId == "R_blXQKK3htptLgC0" //Repeated YS380 by Armel (non-matching occupation). Drop later version
drop if ResponseId == "R_l1tjxTTCkplpH0H" //Repeated YS560 by Appolinnaire, drop first (6 hour) version
drop if ResponseId == "R_eS8333qrtJz7FH5" //Repeated CQP613 by Nadège, drop later version
drop if ResponseId == "R_jACuCnEI9wCrPGA" //Repeated CQP7811 by Emmanuel, drop later version

renpfix FS F1U  //change prefix

//because old prefix was FS, value labels match those of firm survey, messing up labels when merging. Change all labels to start with FU:

quietly ds F1U*, has(vallabel)
foreach var of varlist `r(varlist)'{
	labvalclone "`:val lab `var''" "`var'"
	la val `var' `var'
	}

replace F1U1_3_8 = "" if F1U1_3_1 == F1U1_3_8  //drop middle name if same as first name

*drop F1U2_21* //filter question was wrong, filtered out all Lycées Techniques

//differentiate between follow-up and baseline metadata
rename (Duration__in_seconds_ ResponseId LocationLatitude LocationLongitude StartDate EndDate RecordedDate) (F1U_duration F1U_ResponseId F1U_LocationLatitude F1U_LocationLongitude F1U_StartDate F1U_EndDate F1U_RecordedDate)

//change variable label
label var F1U4_18 "Would you like to work more hours than you currently do per week (if this meant that you would also earn more)?"

gen IDYouth = CQP + string(F1U1_2) //generate youth ID - merging variable
order IDYouth

save follow_up_clean, replace

//Follow-up survey round 2

use "$SourceData/Enquête+de+suivi,+cycle+2_July+27,+2020_19.11.dta"

drop if missing(FS1_13) // only keep youth whose name matched list and consented to interview

replace FS1_3a = 2-FS1_3a

gen IDYouth = "CQP" + string(FS1_2) if FS1_3a == 1
replace IDYouth = "YS" + string(FS1_2) if FS1_3a == 0

drop if IDYouth == "YS10631"
drop if ResponseId == "R_4Lq0EOMqm49Bb8h"
drop if ResponseId == "R_iX4uMSwzzwufwBW"

rename (Duration__in_seconds_ ResponseId LocationLatitude LocationLongitude StartDate EndDate RecordedDate) (F2U_duration F2U_ResponseId F2U_LocationLatitude F2U_LocationLongitude F2U_StartDate F2U_EndDate F2U_RecordedDate)

renpfix FS F2U

quietly ds F2U*, has(vallabel)
foreach var of varlist `r(varlist)'{
	labvalclone "`:val lab `var''" "`var'"
	la val `var' `var'
	}

save follow_up2, replace

//Follow-up survey round 3

use "$SourceData/Enquête+de+suivi,+cycle+3_October+14,+2020_10.09.dta"

drop if missing(FS1_13) // only keep youth whose name matched list and consented to interview

replace FS1_3a = 2-FS1_3a

replace FS1_2 = 731 if FS1_2 == 96431438
replace FS1_2 = 903 if FS1_2 == 68709411
replace FS1_2 = 805 if FS1_2 == 11909
replace FS1_2 = 636 if FS1_2 == 630


gen IDYouth = "CQP" + string(FS1_2) if FS1_3a == 1
replace IDYouth = "YS" + string(FS1_2) if FS1_3a == 0

replace IDYouth = "YS1003" if IDYouth == "CQP1003"
replace IDYouth = "YS1079" if IDYouth == "CQP1079"
replace IDYouth = "YS1082" if IDYouth == "CQP1082"
replace IDYouth = "YS1630" if IDYouth == "CQP1630"
replace IDYouth = "YS355" if IDYouth == "CQP355"
replace IDYouth = "YS7" if IDYouth == "CQP7"
replace IDYouth = "YS985" if IDYouth == "CQP985"
replace IDYouth = "CQP374" if IDYouth == "CQP3674"
replace IDYouth = "YS277" if IDYouth == "YS10631"
replace IDYouth = "YS11909" if IDYouth == "YS805"

drop if ResponseId == "R_5Ozwu9xtwosYN7v"
drop if ResponseId == "R_6LvgiiivM0EYLMS"
drop if ResponseId == "R_mizTweTH5SjWl7X"

rename (Duration__in_seconds_ ResponseId LocationLatitude LocationLongitude StartDate EndDate RecordedDate) (F3U_duration F3U_ResponseId F3U_LocationLatitude F3U_LocationLongitude F3U_StartDate F3U_EndDate F3U_RecordedDate)

renpfix FS F3U

quietly ds F3U*, has(vallabel)
foreach var of varlist `r(varlist)'{
	labvalclone "`:val lab `var''" "`var'"
	la val `var' `var'
	}

save follow_up3, replace

//Endline
use "$SourceData/Enquête+des+jeunes+-+endline_September+29,+2021_17.53.dta"

gen IDYouth = "CQP" + string(YE1_3) if YE1_2 == 1
replace IDYouth = "YS" + string(YE1_3) if YE1_2 == 0
sort IDYouth

// Fiacre/Olayidé mixups. Based on comparing most recent certificates, birthdays, and if all else failed the baseline surveyor
drop if ResponseId == "R_mDJA213VQG7V6S0"
drop if ResponseId == "R_b9WDUpol5s7tt5O"
drop if ResponseId == "R_dr1zVU3Q1EZAQda"
drop if ResponseId == "R_5khKPR52sY7FUB9"
drop if ResponseId == "R_kw3id4Lo20sUxQB"
drop if ResponseId == "R_klmdnMD8rb2lICK"

rename (Duration__in_seconds_ ResponseId LocationLatitude LocationLongitude StartDate EndDate RecordedDate) (F4U_duration F4U_ResponseId F4U_LocationLatitude F4U_LocationLongitude F4U_StartDate F4U_EndDate F4U_RecordedDate)

save endline, replace


