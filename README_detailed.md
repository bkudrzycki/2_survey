# README
This folder was made to facilitate replication of the results and PDF of the paper "Lost in Transition". 

# Prerequisites
Stata and R (preferably RStudio) are necessary to fully reproduce paper.

The following default path is used in the scripts below: 

"~/polybox/Youth Employment/Thesis"

If data is copied into a different directory, path names have to be adjusted where indicated below.

# Replication & Organization

The paper can be replicated from raw data in a few steps, executed in the following order:

  1. **Run Stata file "survey_master.do"**
  
  1.1. It will run "survey_cleaning.do" (located in "code/prep" folder)
          --> have to change the path several times (each time a new data set is loaded or saved)
          --> the loaded data sets (7 altogether) are to be found in "data/source" folder (saved as .dta or .sav)
          --> the saved data sets (7 altogether) are to be found in "data/stata" folder (saved as .dta)

  1.2. It will run "survey_merge.do" (located in "code/prep" folder)
          --> this file merges all the cleaned and saved data sets from "suvrey_cleaning.do" into one data set "youth_survey_merged.dta" (to be found in "data/stata" folder)
            
  1.3. It will run "survey_reshape.do" (located in "code/prep" folder)
          --> this file uses "youth_survey_merged.dta", fixes some problems and saves the updated data set as "youth_survey_reshaped.dta" (to be found in "data/stata" folder)
          
  1.4. It will run "replication.do" (supposedly located in "code" folder nut now in "archive/code" folder)
     
      
  2. **Run SPSS file "survey_master.sps"**
          --> It simply converts both our data sets, "youth_survey_merged.dta" and "youth_survey_reshaped.dta", from .dta format to .sav format.
          
  3. **Run R file "survey_master.R"**
      
      3.1 It will run "survey_cleaning.R" (located in "code/prep" folder)
          --> This file uses data set "youth_survey_reshaped.sav", cleans it and saves two data sets (both to be found in "data" folder):
              * "ys_panel.rda"
              * "ys_panel_labels.rda"
              
  4. **Run R Markdown file "youth_survey.Rmd"**
      --> This file uses both data sets saved previously ("ys_panel.rda" and "ys_panel_labels.rda"), and calls other files:
          * "survey_body.R" (located in "code" folder) --> contains all the tables and figures to be displayed in the body of the paper
          * "survey_appendix.R" (located in "code" folder) --> contains all the tables and figures to be displayed in the appendix of the paper
          Rmk: * All the figures are actually saved in the "markdown/figures" folder.
               * Calls some functions defined in the "functions" folder.
            
          
          
          
          