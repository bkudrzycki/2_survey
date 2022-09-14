## Master Script ##

# set user path: e.g. /Volumes/nadel/research/Data/PhDs/Bart 2022/Paper 3 - CQP

path <- "~/polybox/Youth Employment/1a Youth Survey/Markdown"

setwd(path)

#clean and reshape data
source("code/prep/cleaning.R") #clean youth survey and firm survey
    # inputs:
        # - Stata-cleaned youth survey data ("data/youth_survey_merged.sav")
        # - Raw firm survey baseline data ("data/source/Enquête+auprès+des+patrons_February+10,+2020_13.28.sav")
        # - Raw firm survey endline data and supplement ("data/source/Enquête+auprès+des+patrons+-+endline_October+6,+2021_12.12.sav", "data/source/Enquête+des+jeunes+-+endline+-+supplementaire_November+18,+2021_22.23.sav")
        # - Dossier data: CQP applications and admission ("data/selected_new.xls")
    # outputs:
        # - Cleaned youth data ("data/ys.rda")
        # - Cleaned firm data ("data/fs.rda" and "data/fs_end.rda")

source("code/prep/pivot_longer.R") # extract individual apprentice assessments from firm surveys for matching to apprentice data
    # Inputs: "data/fs.rda" and "data/fs_end.rda"
    # Outputs: "data/base_cqps.rda", "data/base_trad.rda", "data/end_cqps.rda", and "data/end_trad.rda

source("code/prep/recode.R") # match firm and apprentice data, recode answers as needed for analysis, generate final merged dataset
  # Inputs: "data/ys.rda", "data/fs.rda", "data/fs_end.rda", "data/base_cqps.rda", "data/base_trad.rda", "data/end_cqps.rda", and "data/end_trad.rda
  # Outputs: "df.rda"

# to generate the paper in PDF format, open "markdown/cnb_apprenticeship.Rmd" and knit file (shift + cmd + K) or run the command below:
# rmarkdown::render("markdown/youth_survey.Rmd", envir = new.env())

# code used to generate individual tables/figures can be found in the following scripts: 
    # - "code/tbls_body.R"
    # - "code/tbls_appendix.R"
    # - "code/figures.R"
