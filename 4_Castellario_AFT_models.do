*################################################################################*
/*INQUISITION IN GIAVENO (1335)
(4) Castellario: Gender study (Accelerated failure-time regression models)
STATA script by Davor Salihovic (Uni of Antwerp) & Jose Luis Estevez (Uni of Helsinki)
Date: 8 May 2025*/
*################################################################################()

*Import the data*
clear
import delimited "C:\path\to\your\data3.csv"

/*Declare data as survival-time data,
with "time1" and "summons" as the time and event variables, and time at risk divided by
individuals, "id"*/
stset time1, id(id) failure(summons)

*Check the data - if _t0, _t, _d, _st correct*
stdescribe

/*Check if constant and time-varying variables are correct,
by each of the 219 subjects*/
stvary

*Check the development of the risk set*
sts list

*###################################*

* Standardize "Day of accusation", "Denunciations", "Kin denounced", and calendar "Day"
egen day_acc_std = std(day_accused)
egen denunc_std = std(denunc)
egen kin_denunc_std = std(kin_denunc)
egen day_std = std(day)

/*Observe the sample KM estimates 
and smoothed hazard estimates - note the non-monotonic hazards*/
sts graph, surv by(woman)
sts graph, hazard by(woman)

*###################################*

*ANALYSIS*
 
/*Run AFT lognormal models,
after check for the generalized-gamma kappa parameter*/

* 0) Generalized-gamma model
streg woman day_acc_std denunc_std kin_denunc_std martinus franciscus host, dist(ggamma) nolog

* 1) Model 5 - fixed effects only*
mestreg woman day_acc_std denunc_std kin_denunc_std martinus franciscus host, dist(lnormal) nolog

* 2) Model 6 - adding random effects for persons*
mestreg woman day_acc_std denunc_std kin_denunc_std martinus franciscus host, || id:, dist(lnormal) nolog

* 3) Model 7 - adding random effects for day of trial*
mestreg woman day_acc_std denunc_std kin_denunc_std martinus franciscus host, || day_std:, dist(lnormal) nolog
estimates store m7

* 4) Model 8 - crossed-effects, both day of trial and persons*
mestreg woman day_acc_std denunc_std kin_denunc_std martinus franciscus host, || _all:R.day_std || id:, dist(lnormal) difficult
estimates store m8

lrtest m7 m8 
 
*###################################*
