* Encoding: UTF-8.
*1. Set root folder, e.g. '/Volumes/nadel/research/Data/PhDs/Bart 2022/Paper 3 - CQP'.
cd '/Users/kudrzycb/polybox/Youth Employment/1a Youth Survey/Markdown'.

*2. Import merged stata data.
get stata file="data/stata/youth_survey_merged.dta".

*3. Save as sav.
save outfile 'data/youth_survey_merged.sav'.

*2. Import reshaped stata data.
get stata file="data/stata/youth_survey_reshaped.dta".

*3. Save as sav.
save outfile 'data/youth_survey_reshaped.sav'.
