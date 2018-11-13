/*-------------------------------------------------------------------------------
# Name:		00_SetupGlobals
# Purpose:	Setup the Kenya folders for project
# Author:	Tim Essam, Ph.D.
# Created:	2018/11/12
# Owner:	USAID GeoCenter 
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------

# Directories set up in R files; Stata files are for double checking numbers
# and for testing the sampling weights and how to declare complex sampling design
*/

clear
capture log close


global pathin "/Users/timessam/Documents/USAID/Github/2018_Kenya/Data/KIHBS"
global pathout "/Users/timessam/Documents/USAID/Github/2018_Kenya/Data/KIHBS/Dataout"
global programs "/Users/timessam/Documents/USAID/Github/2018_Kenya/Scripts"


* Run programs needed for collapsing, coef plots, etc
include "$programs/cnumlist.do"
include "$programs/pesort.do"


* Clean up household base file
use "$pathin/HH_Information.dta", clear
keep county strat resid eatype qrt cycle clid hhid hhsize iday weight
save "$pathout/hh_base.dta", replace
