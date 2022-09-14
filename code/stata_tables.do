clear all
do "$analysis/Do Files/data_loader"

cd "$latex/Tables"

********************   longitudinal analysis    ***********************

preserve
drop if cqp

cd "/Users/kudrzycb/polybox/Youth Employment/Thesis/tables/panel/Stata"

estpost tabulate status f1ustatus
esttab using transm01.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("Follow-Up 1") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Baseline")) replace

estpost tabulate f1ustatus f2ustatus
esttab using transm12.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("Follow-up 2") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Follow-up 1")) replace

estpost tabulate f2ustatus f3ustatus
esttab using transm23.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("Follow-up 3") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Follow-up 2")) replace

estpost tabulate f3ustatus f4ustatus
esttab using transm34.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("Endline") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Follow-up 3")) replace

estpost tabulate status f2ustatus
esttab using transm02.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("Endline") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Baseline")) replace

estpost tabulate status f3ustatus
esttab using transm03.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("Endline") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Baseline")) replace

estpost tabulate status f4ustatus
esttab using transm04.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("Endline") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Baseline")) replace

estpost tabulate f1ustatus f3ustatus
esttab using transm13.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("Endline") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Baseline")) replace

estpost tabulate f1ustatus f4ustatus
esttab using transm14.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("Endline") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Baseline")) replace

estpost tabulate f2ustatus f4ustatus
esttab using transm24.tex, cell(b(fmt(g)) rowpct(fmt(2) par) colpct(fmt(2)  par([ ]))) mtitle("Endline") collabels(none) wide unstack noobs nonumber eqlabels(, lhs("Baseline")) replace

restore

cd "$latex/Tables"
