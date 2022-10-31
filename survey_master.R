## Master Script ##

# set user path: e.g. /Volumes/nadel/research/Data/PhDs/Bart 2022/Paper 1 - Youth Survey

path <- "/Users/Shared/Bart/Youth Employment/1_survey/"

setwd(path)

#clean and reshape data
source("code/prep/survey_cleaning.R") #clean youth survey
