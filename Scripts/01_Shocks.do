/*-------------------------------------------------------------------------------
# Name:		01_Shocks
# Purpose:	Replication code to mirror R data processing and check for errors
# Author:	Tim Essam, Ph.D.
# Created:	2018/11/12
# Owner:	USAID GeoCenter 
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------

# Directories set up in R files; Stata files are for double checking numbers
# and for testing the sampling weights and how to declare complex sampling design
*/
*

* Run 00_SetupFolders to ensure globals are set
include "/Users/timessam/Documents/USAID/Github/2018_Kenya/Scripts/00_SetupFolders.do"


* Load and process shock module
  use "$pathin/Recent_Shocks.dta", clear 
  clonevar code = q01

* Trying new way of merging data 
merge m:1 code using "/Users/timessam/Documents/USAID/Github/2018_Kenya/Data/KIHBS/shocks_cw.dta"


egen tot_cost = mean(q05) if q05 <= 9000000 & q05 != ., by(shock_alt clid hhid)
g tot_cost_usd = tot_cost * 0.0098

tabsort shock_alt, sum(tot_cost_usd) sort(mean)

* Shock crosswalk is the standard one with the 
  g shock_count = 1

* Total shocks by household
  egen tot_shock = total(shock_count), by(clid hhid)

	g byte ag 			= inlist(q01, 102)
	g byte conflict	 	= inlist(q01, 119, 120)
	g byte crop_price 	= inlist(q01, 107)
	g byte demographic 	= inlist(q01, 112, 113, 114, 115, 116) 
	g byte financial 	= inlist(q01, 105, 106, 107)
	g byte food_price 	= inlist(q01, 109)
	g byte hazard 		= inlist(q01, 101, 118)
	g byte health 		= inlist(q01, 125)
	g byte input_price 	= inlist(q01, 110)
	g byte livestock 	= inlist(q01, 103, 104)
	g byte other 		= inlist(q01, 121, 122, 126, 127)
	g byte violence 	= inlist(q01, 123, 124)
	g byte water_short 	= inlist(q01, 111)


	g byte ag_shk 		= inlist(q01, 102, 103, 104)
	g byte conflict_shk = inlist(q01, 119, 120, 123, 124)
	g byte fin_shk 		= inlist(q01, 105, 106, 107, 117)
	g byte hazard_shk 	= inlist(q01, 101, 111, 118)
	g byte health_shk 	= inlist(q01, 112, 113, 114, 115, 125)
	g byte other_shk 	= inlist(q01, 116, 121, 122, 126, 127)
	g byte price_shk 	= inlist(q01, 108, 109, 110)
	
* Add in coping behavior good / bad / other
* TODO	
	


* Collapse down to the household level
ds(q0* _merge clid hhid code tot_cost tot_cost_usd shock shock_alt shock_count), not
	include "$programs/copylabels.do"
	collapse (max) `r(varlist)', by(clid hhid)
	include "$programs/attachlabels.do"


	foreach x of varlist ag - water_short {
		la var `x' "`x' shock"
		}

	foreach x of varlist *_shk {
		la var `x' "`x' shock traditional grouping"
		}

* Merge with other household data to check statistics by county
	merge 1:1 clid hhid using "$pathout/hh_base.dta", gen(_merge_shocks)

* Replace all new observations with 0s for the shocks
	foreach x of varlist tot_shock - price_shk {
		replace `x' = 0 if `x' == .
		}
	mdesc

* Look at data by district
	tabsort county, sum(hazard_shk) so(mean)
	g byte anyshock = tot_shock > 0 & !missing(tot_shock)


* See if you can figure out weighting
*svyset ea_id [pweight=pw], strata(saq01) singleunit(centered) 
svyset clid [pw = weight], strata(strat) singleunit(centered)

svy: mean livestock, over(county)
pesort anyshock county



*g byte goodcope = inlist(hh_s8q04_a, 1, 2, 3, 4, 6, 7, 8, 10, 12, 16) & rptShock == 1 
*g byte badcope 	= inlist(hh_s8q04_a,  5, 9, 11, 13, 14, 15, 17) & rptShock == 1
*g byte othcope = inlist(hh_s8q04_a, 18, 19, 20, 25, 60)









*	Good Coping: use of savings, credit, asset sales, additional employment, 
*					migration, and assistance
*	Bad Coping: increases vulnerabiliy* compromising health and edudcation 
*				expenses, productive asset sales, conumsumption reductions 
