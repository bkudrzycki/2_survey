clear all
set more off

*****************************************************
********************  Master ************************
*****************************************************

* set user path: e.g. /Volumes/nadel/research/Data/PhDs/Bart 2022/Paper 3 - CQP

global projectfolder "~/polybox/Youth Employment/2 CQP/Paper"


* Project folder globals
* ---------------------
global SourceData         	"$projectfolder/data/source"
global WorkingData          "$projectfolder/data/stata"


* Run Master .do
* ---------------------

//1. Clean
do "$projectfolder/code/prep/cleaning.do"

//2. Merge (with firm and follow-up data)
do "$projectfolder/code/prep/merge.do"

//3. Reshape to attain desired panel structure
do "$projectfolder/code/prep/reshape.do"
