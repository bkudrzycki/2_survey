## Master Script ##

setwd("~/polybox/Youth Employment/Thesis")

#clean data
source("R/source/cleaning.R")

#tables
source("R/tables/attrition.R")
source("R/tables/propensities.R")
source("R/tables/reweighted.R")
source("R/tables/transition_duration.R")

#figures
source("R/calculations/survival analysis.R")
