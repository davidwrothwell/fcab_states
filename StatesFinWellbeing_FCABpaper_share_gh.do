local date: display %td_CCYY_NN_DD date(c(current_date), "DMY")
local date = subinstr(trim("`date'"), " ", "_", .)
display "`date'"
cd "C:\Users\rothweld\Dropbox\Collab\FCAB2020_WelfarePoliciesFinancialResilience\Data"

log using log_`date'.txt, replace text

estimates clear

********************************************************************************
* .do file for paper on how emergency savings varies by states 
* How much does state context matter in emergency savings? Disentangling the
* individual and contextual contributions of the financial capability constructs
* By David Rothwell, w/ Leanne Giordono and Rob Stawski
* Created May 5, 2020
* Updated October 12, 2021
********************************************************************************

********************************************************************************
* contains local paths, these need to be changed
********************************************************************************

* IMPORT FINRA TRACKING DATA
* import delimited "C:\Users\David\Dropbox\Work\My Stata\STA017_FINRAdataUS\NFCS 2018 State Tracking Data 190623.csv"

/* use R to convert data see spss_r_stata_convert.R in this folder
library(foreign)
file.choose()
setwd("C:/Users/rothweld/Dropbox/Work/My Stata/STA017_FINRAdataUS")
dataset = read.spss("C:/Users/rothweld/Dropbox/Work/My Stata/STA017_FINRAdataUS/NFCS2018StateTrackingData190623.sav", to.data.frame=TRUE)
write.dta(dataset, "C:/Users/rothweld/Dropbox/Work/My Stata/STA017_FINRAdataUS/NFCS2018StataConvert.dta") 
*/

cd "C:\Users\rothweld\Dropbox\Collab\FCAB2020_WelfarePoliciesFinancialResilience\Data"
use NFCS2018StataConvert.dta, clear

renvars *, lower //all lower case

save NFCS2018statetracking, replace

fre j5
recode j5 (3/4=.)(2=0)
label define j5_lab 1"yes" 0"no"
label values j5 j5_lab
label var j5 "saved for emergency"

fre j4
recode j4 (1=3) (3=1) (4/5=.)
label define j4_lab 1"not diff" 2"somewhat diff" 3"very diff"
label values j4 j4_lab
fre j4

fre j20
recode j20 (5/6 98/99=.) (1=4)(2=3)(3=2)(4=1) // 2009 data missing for this variable
label define j20_lab 1"Certain could not" 2"Probably not" 3"Probably" 4"Certainly could"
label values j20 j20_lab
label var j20 "could cover 2000 emergency"

gen j20binary = j20
recode j20binary (3/4=1)(1/2=0)
label var j20binary "binary could cover 2000 emergency"

egen finprepgroup = group(j20binary j5)
gen finprep = finprepgroup
label define finprep_lab 1"no emergency sav no cover emergency " 2"emergency save no cover emergency" 3"no emergency could cover" 4"emergency saved and cover emergency"
label values finprep finprep_lab
label var finprep "4 level group emergency save and could cover emerg"

recode finprep (1/3=0)(4=1), gen(finprepall)
label var finprepall "saved for emergency and could cover unexpected"

recode finprep (1=1)(2/4=0), gen(finprepnone)
label var finprepnone "did not save for emergency or could not cover expense"

recode j1 (98/99=.) (11/12 = .) //financial satisfaction
 
gen lowinc = a8
recode lowinc (1/2=1)(3/8=0)
label var lowinc "income scale recoded low income"

gen inccat = a8 //household income
recode inccat (1/2=1)(3/5=2)(6/8=3)
label define inccat_lab 1"low <25k" 2"medium 25-75k" 3"high >75k"
label values inccat inccat_lab
label var inccat "income 3 groups"

gen inccat1 = a8
recode inccat1 (1/4=1) (5/6=2) (7/8=3)
label define inccat1_lab 1"low <50k" 2"medium 50-100k" 3"high >100"
label values inccat1 inccat1_lab
label var inccat1 "income 3 groups despard"

gen home = ea_1
recode home (2/4=0)

gen nchild = a11
recode nchild (5/6=0)

gen employ = a9
recode employ (1/2=1) (3=2) (4/8=3)
label define employ_lab 1"full time se" 2"part time" 3"other"
label values employ employ_lab 
label var employ "employment status 3 groups"

gen insur = h1
recode insur (2/4=0)

******************
fre track

* control variables
fre a3 //gender
recode a3 (1=0)(2=1), gen(female)

fre a3ar_w //age
rename a3ar_w agecat

fre a4a_new_w //ethnicity
recode a4a_new_w (1=0)(2=1), gen(minority)

fre a7a //marital status
recode a7a (1=0)(2/3=1), gen(nonmarried)

decode stateq, generate(statenamed) // convert statename and fips
replace statenamed = upper(statenamed)
statastates, name(statenamed)

save NFCS2018Stata_clean.dta, replace

********************************************************************************
*********************** CALLING STATE LEVEL DATA TO MERGE
********************************************************************************
* political ideology Fording, R. C. (2018). State Ideology Data. https://rcfording.wordpress.com/state-ideology-data/ 
clear 
use "C:\Users\rothweld\Dropbox\Collab\FCAB2020_WelfarePoliciesFinancialResilience\Data\Fording_state_ideology_data\stateideology_v2018.dta"
encode statename, generate(staten)
carryforward staten, gen(yn)
decode yn, generate(yns1)
statastates, name(yns1) 
keep if year ==2009 | year==2012 | year==2015 | year==2017
gen year1= .
replace year1=year+1 if year==2017
replace year1=year if year<2017
drop year
rename year1 year
rename inst6017_nom govideology
keep year state_fips govideology
save fording_ideology, replace

* tanf to poverty ratio
clear
use "C:\Users\rothweld\Dropbox\Work\My Stata\STA020_NSCH_ChildHealth\Data\tanfpov.dta" 
egen tanfpovrat = rowmean(y2009-y2017)
export excel using "tanfpovout", firstrow(variables) replace
clear
use "C:\Users\rothweld\Dropbox\Work\My Stata\STA020_NSCH_ChildHealth\Data\tanfpov.dta" 
gen y2018 = y2017 //copy 2017 data to 2018
reshape long y, i(state) j(year)
rename y tanfratio
keep if year ==2009 | year==2012 | year==2015 | year==2018
rename state statename
statastates, name(statename) 
save tanfpratioin, replace

* input UKCPR data on TANF SNAP genorosity
clear
import excel "C:\Users\rothweld\Dropbox\Work\My Data\UKCPR_welfaredatabase\UKCPR_subset1.xlsx", sheet("Sheet1") firstrow
keep if year>2008
egen TANFFSmean = rowmean(AFDCTANF_FS2PersonBenefit AFDCTANF_FS3PersonBenefit AFDCTANF_FS4PersonBenefit)
rename state_name state
statastates, abbreviation(state) nogen
sort state year
keep if year ==2009 | year==2012 | year==2015 | year==2018
save UKCPRin, replace

* input Fox data on welfare genorosity
clear
use "C:\Users\rothweld\Dropbox\Collab\FCAB2020_WelfarePoliciesFinancialResilience\Data\Fox Welfare Generosity data\WG_ALL_measures_most_updated_11_2019.dta"
keep if year ==2009 | year==2012 | year==2015 | year==2018
renvars *, lower //all lower case
keep wg_index wg_medicaid wg_snap wg_tanf wg_ui state state_abbrev state_fips year
save foxwelfaregen, replace

* input CPS data, 3 years pooled data - see CPS folder
clear 
use "C:\Users\rothweld\Dropbox\Collab\FCAB2020_WelfarePoliciesFinancialResilience\Data\IPUMS_CPS_data\cps_allwaves.dta"
gen poveffort = spmpovmi - spmpov
label var poveffort "spm market income pov-spmpov"
gen yearmatch = yeart
recode yearmatch (2019=2018)(2017=2015)(2014=2012) (2012=2009) //match to NFCS datayears
rename yearmatch  year
drop yeart
save cpsdata, replace

* input state correlates database
clear
import delimited C:\Users\rothweld\Dropbox\Collab\FCAB2020_WelfarePoliciesFinancialResilience\Data\CorrelatesStatePolicyMichiganState\BLS2018_state.csv
* https://www.bls.gov/lau/lastrk18.htm
statastates, name(state)
gen year=2018
save BLS2018_state, replace
clear
import delimited C:\Users\rothweld\Dropbox\Collab\FCAB2020_WelfarePoliciesFinancialResilience\Data\CorrelatesStatePolicyMichiganState\stateeconomic.csv
keep year state state_fips unemployment
keep if year ==2009 | year==2012 | year==2015 | year==2018
merge m:1 state_fips year using BLS2018_state, gen(_merge7)
gen state_unemp = . 
replace state_unemp = unemployment
replace state_unemp = rate if year==2018
keep state state_fips state_unemp year
save BLS2018_state, replace

* merge together
clear 
use fording_ideology
merge 1:1 state_fips year using tanfpratioin, nogen
merge 1:1 state_fips year using UKCPRin, nogen
merge 1:1 state_fips year using foxwelfaregen, nogen
merge 1:1 state_fips year using cpsdata, nogen
merge 1:1 state_fips year using BLS2018_state, nogen
merge 1:1 state_fips year using "C:\Users\rothweld\Dropbox\Collab\FCAB2020_WelfarePoliciesFinancialResilience\Data\FRED_medianincomes\FRED_medinc.dta", nogen
save statedatamerged, replace

* import financial access data
clear
use "C:\Users\rothweld\Dropbox\Collab\FCAB2020_WelfarePoliciesFinancialResilience\Data\Census_statepop\censuspop.dta", clear
statastates, name(state)
drop _merge
merge 1:1 state_fips year using "C:\Users\rothweld\Dropbox\Collab\FCAB2020_WelfarePoliciesFinancialResilience\Data\FDIC_bankbranches\fdicbranches_collapsed"
drop _merge
merge 1:1 state_fips year using "C:\Users\rothweld\Dropbox\Collab\FCAB2020_WelfarePoliciesFinancialResilience\Data\NCUA_branches\cubranches_collapsed"
drop _merge
gen popone = pop/100000
gen bracnchaccess=((branches)/popone)
gen cuacccess=((branchescu)/popone)
merge 1:1 state_fips year using "C:\Users\rothweld\Dropbox\Collab\FCAB2020_WelfarePoliciesFinancialResilience\Data\FDIC_unbanked\fdic_unbanked"
drop if state_fips==11
save financialaccess, replace

clear 
use NFCS2018Stata_clean.dta
gen year = . 
replace year = 2009 if track==1
replace year = 2012 if track==2
replace year = 2015 if track==3
replace year = 2018 if track==4

merge m:1 state_fips year using statedatamerged, nogen
merge m:1 state_fips year using financialaccess, nogen

save  NFCS2018Stata_cleanstacked, replace

********************************************************************************
* Restricting sample and checking data for missing, etc.
* CODING CLUSTERS
********************************************************************************
clear 
use NFCS2018Stata_cleanstacked
keep if agecat <6 & agecat>1 //drop 18-24 / 65 +
drop if stateq==9

tostring year, gen(year2)
gen stateyr = state+year2
unique stateyr

encode state_abbrev, generate(state_aname)

gen incflag = inccat
recode incflag (1=0) (2=.) (3=1) //1= high income 75 k; 0 = <25k

**** CODING CLUSTERS FROM Daiger von Gleichen, R., & Parolin, Z. (2020). Varieties ///
* of liberalism: A comparative analysis of family policy and poverty outcomes ///
* across the 50 United States. Social Policy & Administration.

gen cluster=.
local cluster1 AL AZ AR CO FL GA ID IN KS KY LA MS MO MT NE ///
        NV NC OK SC SD TN TX UT VA WY //
local cluster2 AK HI IL IA ME MD MI NH NM ND OH PA WA WV WI //
local cluster3 CA CT DE MA MN NJ NY OR RI //
local cluster4 VT
foreach value in `cluster1' {
replace cluster=1 if state_abbrev=="`value'"
    }
foreach value in `cluster2' {
replace cluster=2 if state_abbrev=="`value'"
    }
foreach value in `cluster3' {
replace cluster=3 if state_abbrev=="`value'"
    }
foreach value in `cluster4' {
replace cluster=4 if state_abbrev=="`value'"
    }
fre cluster

save  NFCS2018Stata_cleanstackeda, replace

********************************************************************************
* Analysis
********************************************************************************
* create index
clear 
use NFCS2018Stata_cleanstackeda
* j20 could cover $2000 emergency 1-4 point scale
* j5 saved for emergency to cover 3 months 0/1 binary
* j1 financial satisfaction 1-10

fre j20 j5 j1
alpha j20 j5 j1, gen(finresstd) std
alpha j20 j5 j1, gen(finres) 
label var finres "financial resilient"
label var finresstd "financial resilient standardized"

alpha j20 j5, gen(finres2std) std
alpha j20 j5, gen(finres2) 
label var finres2 "financial resilient 2 items"
label var finres2st "financial resilient 2 items standardized"

drop if finresstd ==.

*** gen vars to match Despard et al 2020 financial capability framework
gen emersav = j5

* financial literacy/knowledge
gen fk1 = m6
recode fk1 (2/6=0)
gen fk2 = m7
recode fk2 (1/2=0) (3=1) (4/5=0)
gen fk3 = m8
recode fk3 (1=0) (2=1) (3/6=0)
gen fk4 = m9
recode fk4 (1=1) (2/4=0)
gen fk5= m10
recode fk5 (2=1) (1=0) (3/4=0)
egen fksum = rowtotal(fk1 fk2 fk3 fk4 fk5)

* subjective knowledge
gen sk = m4
recode sk (8/9=.)

* financial confidence
gen fincon = m1_1
recode fincon (8/9=.)

* savings account ownership
gen finacc = b2
recode finacc (2=0) (3/4=.)


********************************************************************************
* gen between within vars
foreach var in fksum sk fincon finacc {
	egen m_`var' = mean(`var'), by(state)
	gen d_`var' = `var'-m_`var'
}

* gen between within vars for stateyr
foreach var in fksum sk fincon finacc {
	egen my_`var' = mean(`var'), by(stateyr)
	gen dy_`var' = `var'-my_`var'
}

* grand mean centering state variables
sum state_unemp 
gen cen_unemp = state_unemp-r(mean)
sum govideology 
gen cen_govideology = govideology-r(mean)
sum wg_index 
gen cen_wg_index = wg_index-r(mean)
sum medinc
gen cen_medinc = medinc-r(mean)
sum bracnchaccess
gen cen_bracnchaccess = bracnchaccess-r(mean) 
sum cuacccess
gen cen_cuaccess = cuacccess-r(mean)
sum unb
gen cen_unb = unb-r(mean)

tabulate agecat, gen(a_)
tabulate inccat1, gen(inc1_)
tabulate cluster, gen(cl_)
tabulate nchild, gen(ch_)
tabulate employ, gen(emp_)

save NFCS2018Stata_cleanstackedab, replace

* set macros
global indiv_vars "a_1 a_2 a_3 a_4 inc1_1 inc1_2 inc1_3 female minority nonmarried ch_1 ch_2 ch_3 ch_4 ch_5 emp_1 emp_2 emp_3 insur home j4"
global indiv_varsreg "i.agecat i.inccat1 i.female i.minority i.nonmarried nchild i.employ i.insur i.home j4"
global state_vars "cen_unemp cen_govideology cen_wg_index cen_medinc medinc bracnchaccess cuacccess unb" 

* Figures and tables
clear 
use NFCS2018Stata_cleanstackedab

* TABLE 1 Descriptives
gcollapse(mean) $indiv_vars $state_vars state_unemp govideology transfershare poveffort wg_index cl_1 cl_2 cl_3 cl_4 d_fksum m_fksum d_sk m_sk d_fincon m_fincon d_finacc m_finacc fksum sk fincon finacc emersav  [aw=wgt_s3], by(state year stateyr) cw

graph matrix fksum d_fksum m_fksum sk d_sk m_sk fincon d_fincon m_fincon finacc d_finacc m_finacc emersav
tabstat $indiv_vars fksum d_fksum m_fksum sk d_sk m_sk fincon d_fincon m_fincon finacc d_finacc m_finacc emersav, stat(mean sd min max) long col(stat)
foreach var of varlist $indiv_vars fksum sk fincon finacc d_fksum m_fksum d_sk m_sk d_fincon m_fincon d_finacc m_finacc emersav {
   sort `var'
   di `var'
   list stateyr `var' in 1
   list stateyr `var' in 200
   }
   
tabstat $state_vars transfershare poveffort wg_index state_unemp govideology cl_1 cl_2 cl_3 cl_4 , stat(mean sd min max) long col(stat)
foreach var of varlist $state_vars transfershare poveffort wg_index cl_1 cl_2 cl_3 cl_4  state_unemp {
   sort `var'
   di `var'
   list stateyr `var' in 1
   list stateyr `var' in 200
   }
sort govideology
list govideology stateyr if govideology !=. 
sort transfershare
list transfershare stateyr if transfershare !=. 
sort poveffort
list poveffort stateyr if poveffort !=.
sort wg_index
list wg_index stateyr if wg_index !=.


********************************************************************************
* correlations state level variables
use NFCS2018Stata_cleanstackedab, clear
keep year state stateyr state_fips emersav wgt_s3
collapse emersav [w=wgt_s3], by(year state state_fips)
table state (), stat(mean emersav) stat(semean emersav) //manually export to excel for calculations 

rename emersav emersavg

merge 1:m state_fips year using NFCS2018Stata_cleanstackedab, nogen

keep year state stateyr emersavg state_unemp govideology wg_index medinc bracnchaccess cuacccess unb cluster m_fksum m_sk m_fincon m_finacc wgt_s3
collapse emersavg state_unemp govideology wg_index medinc bracnchaccess cuacccess unb cluster m_fksum m_sk m_fincon m_finacc, by(year state)
graph matrix emersavg state_unemp govideology wg_index medinc bracnchaccess cuacccess unb m_fksum m_sk m_fincon m_finacc

encode state, gen(statec)
tabstat state_unemp govideology wg_index medinc bracnchaccess cuacccess unb m_fksum m_sk m_fincon m_finacc cluster, stat(mean sd min max) long col(stat)
regress emersavg state_unemp  i.year
regress emersavg govideology  i.year
regress emersavg wg_index  i.year
regress emersavg medinc  i.year
regress emersavg bracnchaccess i.year
regress emersavg cuacccess  i.year
regress emersavg unb i.year
regress emersavg i.cluster i.year
regress emersavg i.cluster bracnchaccess i.year
regress emersavg i.cluster cuacccess i.year
regress emersavg i.cluster unb i.year
pwcorr emersavg state_unemp govideology wg_index medinc bracnchaccess cuacccess unb, sig 
pwcorr emersavg state_unemp govideology wg_index medinc bracnchaccess cuacccess unb if year>2009, sig 
pwcorr emersavg state_unemp govideology wg_index medinc bracnchaccess cuacccess unb, sig bonferroni
pwcorr emersavg m_fksum m_sk m_fincon m_finacc cuacccess, sig 

********************************************************************************
* FIGURE 1 rates of emergency savings across states
use NFCS2018Stata_cleanstackedab, clear

estimates clear
regress emersav i.state_aname i.year [aw=wgt_s3], vce(cluster stateyr)
estpost margins state_aname
eststo lpmstate
coefplot lpmstate,  sort vertical scheme(plotplain)  //edit this output in graphics editor
esttab using "C:\Users\rothweld\Dropbox\Collab\FCAB2020_WelfarePoliciesFinancialResilience\Data\Rplots\lpmstate_out.csv", label not nostar plain noobs replace

/*regress emersav i.inccat1##i.state_aname if inccat1 !=2 [aw=wgt_s3], vce(cluster stateyr)
estpost margins inccat1#state_aname
eststo lpmstate_inc
coefplot lpmstate_inc, sort vertical
estpost margins state_aname

esttab using "C:\Users\rothweld\Dropbox\Collab\FCAB2020_WelfarePoliciesFinancialResilience\Data\Rplots\lpmstate_inc_out.csv", label not nostar plain noobs replace
*/

********************************************************************************
* modeling
********************************************************************************
use NFCS2018Stata_cleanstackedab, clear

mixed emersav ///
    ///
   || state: , 
estat icc

* testing OLS assumption
clear 
use NFCS2018Stata_cleanstackedab

xtreg emersav, i(state_fip) fe
xtreg emersav, i(state_fip) re mle

regress emersav $indiv_varsreg 
predict yhat, res
anova yhat state_fips

*Figure 1 caterpillar plot
meprobit emersav || state:   
estat icc
egen one = tag(state)
predict be, reffects reses(se) 
sort be
gen rank=sum(one)
sort rank
list state be rank if one==1
serrbar be se rank if one, scheme(plotplain) scale(1.96) yline(0) mvopt(mlabel(state_abbrev) mlabpos(0) msize(zero))

********************************************
**************** models ********************
********************************************



*******
foreach var in a_1 inc1_1 female minority {
	egen my_`var' = mean(`var'), by(stateyr)
	gen dy_`var' = `var'-my_`var'
	egen m_`var' = mean(`var'), by(state)
	gen d_`var' = `var'-m_`var'
}

alpha  bracnchaccess cuacccess unb m_finacc if year>2009, gen(access4) detail item
pca bracnchaccess cuacccess unb m_finacc if year>2009
predict pc1 pc2, score

gen cen_medinca = cen_medinc/1000
gen cen_year= 2018-year

*********************************************
save NFCS2018Stata_cleanstackedab1, replace
*********************************************
meprobit emersav  ///
    cen_year  ///
	///
   || state: , 
estimates store mb
estat sd, post
estimates store mbse
estimates restore mb
estat icc
estadd scalar icc2=r(icc2)
estimates store mbic
predict be1s, reffects reses(se1s) 
sort be1s
gen rank1s=sum(one)
sort rank1s
list state be1s se1s rank1s if one==1
serrbar be1s se1s rank1s if one, scheme(plotplain) scale(1.96) yline(0) mvopt(mlabel(state_abbrev) mlabpos(0) msize(zero))
graph save "Graph" "be1s.gph", replace
estpost margins, dydx(*)
eststo margin1 

meprobit emersav  ///
   cen_year  ///
	inc1_1 a_1 female minority  ///
   || state: , 
estimates store m1
estat sd, post
estimates store m1sd
estimates restore m1
estat icc
estadd scalar icc2=r(icc2)
estimates store m1ic
predict be2s, reffects reses(se2s) 
sort be2s
gen rank2s=sum(one)
sort rank2s
list state be2s se2s rank2s if one==1
serrbar be2s se2s rank2s if one, scheme(plotplain) scale(1.96) yline(0) mvopt(mlabel(state_abbrev) mlabpos(0) msize(zero)) ysc(r(-.4 .4))
graph save "Graph" "be2s.gph", replace
estpost margins, dydx(*)
eststo margin2

meprobit emersav  ///
    cen_year  ///
	inc1_1 a_1 female minority ///
	d_fksum m_fksum d_sk m_sk d_fincon m_fincon d_finacc m_finacc || state: ,
estimates store m2
test (d_fksum = m_fksum) (d_sk = m_sk) (d_fincon = m_fincon) (d_finacc = m_finacc)
test (d_fksum = m_fksum)
test (d_sk = m_sk)
test (d_fincon = m_fincon)
test (d_finacc = m_finacc)
/* keep hybrids for fksum and finacc; use single level var for sk and fincon) */
estimates restore m2
estat sd, post
estimates store m2sd
estimates restore m2
estat icc
estadd scalar icc2=r(icc2)
estimates store m2ic
predict be3s1, reffects reses(se3s1)
sort be3s1
gen rank3s1=sum(one)
sort rank3s1
list state be3s1 se3s1 rank3s1 if one==1
serrbar be3s1 se3s1 rank3s1 if one, scheme(plotplain) scale(1.96) yline(0) mvopt(mlabel(state_abbrev) mlabpos(0) msize(zero)) ysc(r(-.4 .4))
graph save "Graph" "be3s1.gph", replace
estpost margins, dydx(*)
eststo margin3

meprobit emersav  ///
    cen_year  ///
	inc1_1 a_1 female minority ///
	fksum sk fincon d_finacc m_finacc ///
	cen_unemp cen_medinca ///
	cen_govideology cen_wg_index || state: ,  
test (d_finacc = m_finacc) // fails random effects assumption
estimates store m3
estat sd, post
estimates store m3sd
estimates restore m3
estat icc
estadd scalar icc2=r(icc2)
estimates store m3ic
predict be3s2, reffects reses(se3s2)
sort be3s2
gen rank3s2=sum(one)
sort rank3s2
list state be3s2 se3s2 rank3s2 if one==1
serrbar be3s2 se3s2 rank3s2 if one, scheme(plotplain) scale(1.96) yline(0) mvopt(mlabel(state_abbrev) mlabpos(0) msize(zero)) ysc(r(-.4 .4))
graph save "Graph" "be3s2.gph", replace
estpost margins, dydx(*)
eststo margin4

meprobit emersav  ///
    cen_year  ///
	inc1_1 a_1 female minority ///
	fksum sk fincon d_finacc m_finacc ///
	cen_unemp cen_medinca ///
    cen_govideology cen_wg_index ///
    cen_bracnchaccess cen_cuaccess cen_unb || state: , 
estimates store m4
estat sd, post
estimates store m4sd
estimates restore m4
estat icc
estadd scalar icc2=r(icc2)
estimates store m4ic
predict be4s, reffects reses(se4s) 
sort be4s
gen rank4s=sum(one)
sort rank4s
list state be4s se4s rank4s if one==1
serrbar be4s se4s rank4s if one, scheme(plotplain) scale(1.96) yline(0) mvopt(mlabel(state_abbrev) mlabpos(0) msize(zero)) ysc(r(-.4 .4))
graph save "Graph" "be4s.gph", replace
estpost margins, dydx(*)
eststo margin5

meprobit emersav  ///
    cen_year  ///
	cen_unemp cen_medinca ///
    cen_govideology cen_wg_index ///
    cen_bracnchaccess cen_cuaccess cen_unb || state: , 
estimates store m5
estat sd, post
estimates store m5sd
estimates restore m5
estat icc
estadd scalar icc2=r(icc2)
estimates store m5ic
estpost margins, dydx(*)

esttab margin1 margin2 margin3 margin4 margin5, mtitles not b(3)

esttab mb mbse mbic m1 m1sd m1ic m2 m2sd m2ic m3 m3sd m3ic m4 m4sd m4ic m5 m5sd m5ic using models_rev.csv, se bic aic mtitles noparentheses nolines nobase noobs scalars(icc2) replace 

graph combine "be1s.gph" "be2s.gph" "be3s1.gph" "be3s2.gph" "be4s.gph" 
graph save "Graph" "be1_4s_combined.gph", replace

graph combine "be1s.gph" "be4s.gph" 

***********************************************************
*** sensitivity stateyr instead of state
*** ***
***********************************************************
use NFCS2018Stata_cleanstackedab1, clear
meprobit emersav  ///
    cen_year	///
   || stateyr: , 
estimates store mb_sy
estat sd, post
estimates store mbse_sy
estimates restore mb_sy
estat icc
estadd scalar icc2=r(icc2)
estimates store mbic_sy
estpost margins, dydx(*)
eststo margin1sy
meprobit emersav  ///
    cen_year	///
   || stateyr: , 
predict be1sy, reffects reses(se1sy) 
collapse be1sy se1sy , by(state state_abbrev)
egen pickone1 = tag(state)
sort be1sy
generate rank1sy = sum(pickone1)
sort rank1sy
list state be1sy se1sy rank1sy if pickone1==1
serrbar be1sy se1sy rank1sy if pickone1==1, scheme(plotplain) scale(1.96) yline(0) mvopt(mlabel(state_abbrev) mlabpos(0) msize(zero))
graph save "Graph" "be1sy.gph", replace

use NFCS2018Stata_cleanstackedab1, clear
meprobit emersav  ///
   cen_year ///
   inc1_1 a_1 female minority  ///
   || stateyr: , 
estimates store m1_sy
estat sd, post
estimates store m1sd_sy
estimates restore m1_sy
estat icc
estadd scalar icc2=r(icc2)
estimates store m1ic_sy
estpost margins, dydx(*)
eststo margin2sy

meprobit emersav  ///
    cen_year ///
	inc1_1 a_1 female minority ///
	dy_fksum my_fksum dy_sk my_sk dy_fincon  my_fincon dy_finacc my_finacc  || stateyr: , 
estimates store m2_sy
test (dy_fksum = my_fksum) (dy_sk = my_sk) (dy_fincon = my_fincon) (dy_finacc = my_finacc)
test (dy_fksum = my_fksum)
test (dy_sk = my_sk)
test (dy_fincon = my_fincon)
test (dy_finacc = my_finacc)
estimates restore m2_sy
estat sd, post
estimates store m2sd_sy
estimates restore m2_sy
estat icc
estadd scalar icc2=r(icc2)
estimates store m2ic_sy
estpost margins, dydx(*)
eststo margin3sy

meprobit emersav  ///
    cen_year ///
	inc1_1 a_1 female minority ///
	fksum sk fincon dy_finacc my_finacc   ///
	cen_unemp cen_medinca ///
	cen_govideology cen_wg_index || stateyr: , 
estimates store m3_sy
test (dy_finacc = my_finacc)
estimates restore m3_sy
estat sd, post
estimates store m3sd_sy
estimates restore m3_sy
estat icc
estadd scalar icc2=r(icc2)
estimates store m3ic_sy
estpost margins, dydx(*)
eststo margin4sy

meprobit emersav  ///
   	cen_year inc1_1 a_1 female minority ///
	fksum sk fincon dy_finacc my_finacc ///
	cen_unemp cen_medinca ///
	cen_govideology cen_wg_index ///
	cen_bracnchaccess cen_cuaccess || stateyr: ,
estimates store m4_sy
test (dy_finacc = my_finacc)
estimates restore m4_sy
estat sd, post
estimates store m4sd_sy
estimates restore m4_sy
estat icc
estadd scalar icc2=r(icc2)
estimates store m4ic_sy
estpost margins, dydx(*)
eststo margin5sy
meprobit emersav  ///
   	cen_year inc1_1 a_1 female minority ///
	fksum sk fincon fincon dy_finacc my_finacc ///
	cen_unemp cen_medinca ///
	cen_govideology cen_wg_index ///
	cen_bracnchaccess cen_cuaccess || stateyr: , 
predict be4sy, reffects reses(se4sy) 
collapse be4sy se4sy , by(state state_abbrev)
egen pickone4 = tag(state)
sort be4sy
generate rank4sy = sum(pickone4)
sort rank4sy
list state be4sy se4sy rank4sy if pickone4==1
serrbar be4sy se4sy rank4sy if pickone4==1, scheme(plotplain) scale(1.96) yline(0) mvopt(mlabel(state_abbrev) mlabpos(0) msize(zero)) ysc(r(-.4 .4))
graph save "Graph" "be4sy.gph", replace

use NFCS2018Stata_cleanstackedab_models, clear
meprobit emersav  ///
	cen_year ///
	cen_unemp cen_medinca ///
	cen_govideology cen_wg_index ///
	cen_bracnchaccess cen_cuaccess || stateyr: , 
estimates store m5_sy
estat sd, post
estimates store m5sd_sy
estimates restore m5_sy
estat icc
estadd scalar icc2=r(icc2)
estimates store m5ic_sy
estpost margins, dydx(*)

esttab margin1sy margin2sy margin3sy margin4sy margin5sy, mtitles not b(3)

esttab mb_sy mbse_sy mbic_sy m1_sy m1sd_sy m1ic_sy m2_sy m2sd_sy m2ic_sy m3_sy m3sd_sy m3ic_sy m4_sy m4sd_sy m4ic_sy m5_sy m5sd_sy m5ic_sy using models_rev_sy.csv, se bic aic mtitles  noparentheses nolines nobase noobs scalars(icc2) replace 

graph combine "be1sy.gph" "be4sy.gph" 

save NFCS2018Stata_cleanstackedab_models, replace

*** stats for bottom and top states, significant state level variables
use NFCS2018Stata_cleanstackedab_models, clear
collapse be1sy se1sy emersav m_finacc medinc govideology cuacccess [aw=wgt_s3], by(state state_abbrev)
tabstat emersav m_finacc medinc govideology cuacccess, by(state) stat(mean sd min max) long col(stat)
sort emersav
generate sortsave = _n
listtab state emersav be1sy se1sy m_finacc medinc govideology cuacccess if sortsave <11 using bottom10.txt, rstyle(tabdelim) replace
listtab state emersav be1sy se1sy m_finacc medinc govideology cuacccess if sortsave >40 using upper10.txt, rstyle(tabdelim) replace

/* Notes on paper prep
1. Used stateyr as cluster var in final models. First section above with state as cluster var is not updated. 
2. Copied margins results from esttab to table 2, results from esttab .csv show the AIC, BIC, and sd(state)
3. Copied random effects to excel then import to R for graphs
4. Last section of code includes collapsed data for Table 3
*/


log close

/*

***********************************************
*** sensitivity 3 level nesting

meprobit emersav  ///
    i.year  ///
	inc1_1 a_1 female minority ///
	fksum  || state: , 

meprobit emersav  ///
    i.year  ///
	inc1_1 a_1 female minority ///
	fksum sk fincon  || state: inc1_1, 

meprobit emersav  ///
   	inc1_1 a_1 female minority ///
	d_fksum d_sk d_fincon d_finacc m_fksum m_sk m_fincon m_finacc ///
	cen_bracnchaccess cen_cuaccess cen_unb ///
	cen_unemp cen_medinca ///
	cen_govideology cen_wg_index  || state: || stateyr: , 

melogit emersav  ///
   	cen_year inc1_1 a_1 female minority ///
	d_fksum d_sk d_fincon d_finacc m_fksum m_sk m_fincon m_finacc ///
	cen_bracnchaccess cen_cuaccess cen_unb ///
	cen_unemp cen_medinca ///
	cen_govideology cen_wg_index  || state: || stateyr:  
	
meprobit emersav  ///
   	cen_year inc1_1 a_1 female minority ///
	fksum sk fincon finacc ///
	cen_bracnchaccess cen_cuaccess cen_unb ///
	cen_unemp cen_medinca ///
	cen_govideology cen_wg_index  || state: || stateyr: inc1_1,

	


*** Senstivity dropping 2009
drop if year==2009
meprobit emersav  ///
    i.year  ///
	///
   || state: , 
estimates store mb_09
estat sd, post
estimates store mbse_09
estimates restore mb_09
estat icc
estadd scalar icc2=r(icc2)
estimates store mbic_09

meprobit emersav  ///
    i.year  ///
	inc1_1 a_1 female minority  ///
   || state: , 
estimates store m1_09
estat sd, post
estimates store m1sd_09
estimates restore m1_09
estat icc
estadd scalar icc2=r(icc2)
estimates store m1ic_09

meprobit emersav  ///
    i.year  ///
	inc1_1 a_1 female minority ///
	fksum sk fincon finacc || state: , 
estimates store m2_09
estat sd, post
estimates store m2sd_09
estimates restore m2_09
estat icc
estadd scalar icc2=r(icc2)
estimates store m2ic_09

meprobit emersav  ///
    i.year  ///
	inc1_1 a_1 female minority ///
	fksum sk fincon finacc ///
	cen_unemp cen_medinca ///
	cen_govideology cen_wg_index || state: ,  
estimates store m3_09
estat sd, post
estimates store m3sd_09
estimates restore m3_09
estat icc
estadd scalar icc2=r(icc2)
estimates store m3ic_09

meprobit emersav  ///
    i.year  ///
	inc1_1 a_1 female minority ///
	fksum sk fincon finacc ///
	bracnchaccess cuacccess unb ///
	cen_unemp cen_medinca ///
	cen_govideology cen_wg_index || state: , 
estimates store m4_09
estat sd, post
estimates store m4sd_09
estimates restore m4_09
estat icc
estadd scalar icc2=r(icc2)
estimates store m4ic_09

meprobit emersav  ///
    i.year  ///
	bracnchaccess cuacccess unb ///
	cen_unemp cen_medinca ///
	cen_govideology cen_wg_index || state: , 
estimates store m5_09
estat sd, post
estimates store m5sd_09
estimates restore m5_09
estat icc
estadd scalar icc2=r(icc2)
estimates store m5ic_09

esttab mb_09 mbse_09 mbic_09 m1_09 m1sd_09 m1ic_09 m2_09 m2sd_09 m2ic_09 m3_09 m3sd_09 m3ic_09 m4_09 m4sd_09 m4ic_09 m5_09 m5sd_09 m5ic_09 using models_rev_09.csv, se bic aic mtitles  noparentheses nolines nobase noobs scalars(icc2) replace 

*** Senstivity dropping 2009 stateyr
meprobit emersav  ///
    	///
   || stateyr: , 
estimates store mb_sy09
estat sd, post
estimates store mbse_sy09
estimates restore mb_sy09
estat icc
estadd scalar icc2=r(icc2)
estimates store mbic_sy09

meprobit emersav  ///
    inc1_1 a_1 female minority  ///
   || stateyr: , 
estimates store m1_sy09
estat sd, post
estimates store m1sd_sy09
estimates restore m1_sy09
estat icc
estadd scalar icc2=r(icc2)
estimates store m1ic_sy09

meprobit emersav  ///
    inc1_1 a_1 female minority ///
	fksum sk fincon finacc || stateyr: , 
estimates store m2_sy09
estat sd, post
estimates store m2sd_sy09
estimates restore m2_sy09
estat icc
estadd scalar icc2=r(icc2)
estimates store m2ic_sy09

meprobit emersav  ///
    inc1_1 a_1 female minority ///
	fksum sk fincon finacc  ///
	cen_unemp cen_medinca ///
	cen_govideology cen_wg_index || stateyr: ,  
estimates store m3_sy09
estat sd, post
estimates store m3sd_sy09
estimates restore m3_sy09
estat icc
estadd scalar icc2=r(icc2)
estimates store m3ic_sy09

meprobit emersav  ///
   	inc1_1 a_1 female minority ///
	fksum sk fincon finacc ///
	bracnchaccess cuacccess unb ///
	cen_unemp cen_medinca ///
	cen_govideology cen_wg_index || stateyr: , 
estimates store m4_sy09
estat sd, post
estimates store m4sd_sy09
estimates restore m4_sy09
estat icc
estadd scalar icc2=r(icc2)
estimates store m4ic_sy09

meprobit emersav  ///
	bracnchaccess cuacccess unb ///
	cen_unemp cen_medinca ///
	cen_govideology cen_wg_index || stateyr: , 
estimates store m5_sy09
estat sd, post
estimates store m5sd_sy09
estimates restore m5_sy09
estat icc
estadd scalar icc2=r(icc2)
estimates store m5ic_sy09

esttab mb_sy09 mbse_sy09 mbic_sy09 m1_sy09 m1sd_sy09 m1ic_sy09 m2_sy09 m2sd_sy09 m2ic_sy09 m3_sy09 m3sd_sy09 m3ic_sy09 m4_sy09 m4sd_sy09 m4ic_sy09 m5_sy09 m5sd_sy09 m5ic_sy09 using models_rev_sy09.csv, se bic aic mtitles  noparentheses nolines nobase noobs scalars(icc2) replace 

*** sensitivity random slopes
meprobit emersav i.year ///
   	inc1_1 a_1 female minority ///
	fksum sk fincon finacc ///
	bracnchaccess cuacccess unb ///
	cen_unemp cen_medinca ///
	cen_govideology cen_wg_index || state: inc1_1 a_1 female minority, 

*
* ARCHIVED
** table 2 revised for manuscript
* null model
meprobit emersav  ///
      ///
   || state: , 
estimates store me1
estat sd, post
estimates store sd1
estimates restore me1
*estpost margins, dydx(d_fksum)
*eststo mar1
estimates restore me1
estat icc
estadd scalar icc2=r(icc2)
estimates store ic1

estimates clear
meprobit emersav  ///
    i.year $indiv_varsreg $state_vars fksum sk fincon finacc ///
   || state: , 
estimates store mxbase
estat sd, post
estimates store sdxbase
estimates restore mxbase
estpost margins, dydx(fksum sk fincon finacc )
eststo marmxbase
estimates restore mxbase
estat icc
estadd scalar icc2=r(icc2)
estimates store mxicbase
estat ic 

meprobit emersav  ///
    i.year $indiv_varsreg $state_vars d_fksum m_fksum ///
   || state: ,  
test m_fksum = d_fksum
estimates store mxm1
estat sd, post
estimates store sdm1
estimates restore mxm1
estpost margins, dydx(d_fksum m_fksum)
eststo marmx1
estimates restore mxm1
estat icc
estadd scalar icc2=r(icc2)
estimates store mxic1

meprobit emersav  ///
    i.year $indiv_varsreg $state_vars d_sk m_sk ///
   || state: ,  
test m_sk = d_sk
estimates store mxm2
estat sd, post
estimates store sdm2
estimates restore mxm2
estpost margins, dydx(d_sk m_sk)
eststo marmx2
estimates restore mxm2
estat icc
estadd scalar icc2=r(icc2)
estimates store mxic2

meprobit emersav  ///
    i.year $indiv_varsreg $state_vars d_fincon m_fincon ///
   || state: ,  
test m_fincon = d_fincon
estimates store mxm3
estat sd, post
estimates store sdm3
estimates restore mxm3
estpost margins, dydx(d_fincon m_fincon)
eststo marmx3
estimates restore mxm3
estat icc
estadd scalar icc2=r(icc2)
estimates store mxic3

meprobit emersav  ///
    i.year $indiv_varsreg $state_vars d_finacc m_finacc ///
   || state: ,  
test m_finacc = d_finacc
estimates store mxm4
estat sd, post
estimates store sdm4
estimates restore mxm4
estpost margins, dydx(d_finacc m_finacc)
eststo marmx4
estimates restore mxm4
estat icc
estadd scalar icc2=r(icc2)
estimates store mxic4

meprobit emersav  ///
    i.year $indiv_varsreg $state_vars d_fksum m_fksum d_sk m_sk d_fincon m_fincon d_finacc m_finacc  ///
   || state: , 
test (d_fksum=m_fksum) (d_sk=m_sk) (d_fincon=m_fincon) (d_finacc=m_finacc) 
estimates store mxm5
estat sd, post
estimates store sdx5
estimates restore mxm5
estpost margins, dydx(d_fksum m_fksum d_sk m_sk d_fincon m_fincon d_finacc m_finacc)
eststo marmx5
estimates restore mxm5
estat icc
estadd scalar icc2=r(icc2)
estimates store mxic5
estat ic 

esttab sdxbase mxicbase marmxbase sdm1 mxic1 marmx1 sdm2 mxic2 marmx2 sdm3 mxic3 marmx3 sdm4 mxic4 marmx4 sdx5 mxic5 marmx5 using models_mx.csv, se bic aic mtitles  noparentheses nolines nobase noobs scalars(icc2) replace
   
*margins predictions
margins, at(m_finacc = (.62 .73 .83 .90) inccat1=(1 2 3)) atmeans
marginsplot, scheme(plotplain)

log close

/*   
*fksum test
meprobit emersav  ///
     i.year $indiv_varsreg  ///
   || state: ,   
estimates store fk1
meprobit emersav  ///
     i.year $indiv_varsreg  cen_unemp  ///
   || state: ,   
estimates store fk1_1
meprobit emersav  ///
     i.year $indiv_varsreg d_fksum m_fksum  ///
   || state: , 
estimates store fk2
meprobit emersav  ///
     i.year $indiv_varsreg d_fksum m_fksum cen_unemp ///
   || state: ,  
estimates store fk3
meprobit emersav  ///
     i.year $indiv_varsreg d_fksum m_fksum cen_unemp cen_govideology ///
   || state: , 
estimates store fk4
meprobit emersav  ///
     i.year $indiv_varsreg d_fksum m_fksum cen_unemp cen_govideology cen_wg_index ///
   || state: , 
estimates store fk5
esttab fk*
graph matrix cen_unemp cen_govideology cen_wg_index fksum d_fksum m_fksum 
/*   
   
   
   
* testing output
mixed finresstd  ///
   i.year i.inccat i.female i.agecat i.minority i.nonmarried ///
   || state: ,  difficult stddev
estimates store cmt
esttab cmt, se bic aic mtitles stardetach noparentheses nolines nobase noobs replace  ///
   eqlabels("" "between group stateyr(_cons)" "within group residual", none) 
esttab cmt, se bic aic mtitles stardetach noparentheses nolines nobase noobs replace transform(ln*: exp(@) (@)) ///
   eqlabels("" "between group stateyr(_cons)" "within group residual", none) 

**** multiple test syntax
meprobit emersav  ///
    i.year $indiv_varsreg $state_vars d_fksum d_sk  d_fincon m_fincon d_finacc m_finacc ///
   || state: ,  
test (d_fincon=m_fincon) (d_finacc=m_finacc) 

meprobit emersav  ///
    i.year m_finacc ///
   || state: ,  
***   
* statyr between within
meprobit emersav  ///
    i.year $indiv_varsreg $state_vars dy_fksum my_fksum ///
   || state: 
test (dy_fksum= my_fksum) 

meprobit emersav  ///
    i.year $indiv_varsreg $state_vars dy_sk my_sk ///
   || state: 
test (dy_sk= my_sk)

meprobit emersav  ///
    i.year $indiv_varsreg $state_vars dy_fincon my_fincon ///
   || state: 
test (dy_fincon=my_fincon)

meprobit emersav  ///
    i.year $indiv_varsreg $state_vars dy_finacc my_finacc ///
   || state: 
test (dy_finacc=my_finacc)

meprobit emersav  ///
    i.year $indiv_varsreg $state_vars fksum dy_sk my_sk fincon dy_finacc my_finacc  ///
   || state: 

meprobit emersav  ///
    i.year $indiv_varsreg  dy_fksum my_fksum dy_sk my_sk dy_fincon my_fincon dy_finacc my_finacc $state_vars ///
   || state: 
*"cen_unemp cen_govideology cen_wg_index"  
meprobit emersav  ///
    i.year $indiv_varsreg  dy_fksum my_fksum dy_sk my_sk dy_fincon my_fincon dy_finacc my_finacc cen_unemp cen_govideology cen_wg_index ///
   || state:    

test (dy_fksum= my_fksum) 
test (dy_sk= my_sk)  
test (dy_fincon= my_fincon)  
test (dy_finacc =my_finacc)

meprobit emersav  ///
   $indiv_varsreg $state_vars dy_finacc my_finacc if year==2009 ///
   || state: 
meprobit emersav  ///
   $indiv_varsreg $state_vars dy_finacc my_finacc if year==2012 ///
   || state: 
meprobit emersav  ///
   $indiv_varsreg $state_vars dy_finacc my_finacc if year==2015 ///
   || state: 
meprobit emersav  ///
   $indiv_varsreg $state_vars dy_finacc my_finacc if year==2018 ///
   || state: 
test (dy_finacc=my_finacc)

* collapse state analysis3
clear 
use NFCS2018Stata_cleanstackedab
gcollapse(mean) emersav fksum m_fksum my_fksum sk m_sk my_sk fincon m_fincon my_fincon finacc m_finacc my_finacc year, by(state) cw
twoway (scatter emersav m_fksum, mlabel(state))
twoway (scatter emersav m_sk, mlabel(state))
twoway (scatter emersav m_fincon, mlabel(state))
twoway (scatter emersav m_finacc, mlabel(state))
corr emersav m_fksum m_sk m_fincon m_finacc 

gcollapse(mean) emersav fksum m_fksum my_fksum sk m_sk my_sk fincon m_fincon my_fincon finacc m_finacc my_finacc year, by(stateyr) cw
twoway (scatter emersav my_fksum, mlabel(stateyr))
twoway (scatter emersav my_sk, mlabel(stateyr))
twoway (scatter emersav my_fincon, mlabel(stateyr))
twoway (scatter emersav my_finacc, mlabel(stateyr))

twoway (scatter emersav my_finacc if year==2009, mlabel(stateyr))
twoway (scatter emersav my_finacc if year==2012, mlabel(stateyr))
twoway (scatter emersav my_finacc if year==2015, mlabel(stateyr))
twoway (scatter emersav my_finacc if year==2018, mlabel(stateyr))

corr emersav my_fksum my_sk my_fincon my_finacc 
corr emersav m_fksum m_sk m_fincon m_finacc 
corr emersav my_fksum my_sk my_fincon my_finacc if year==2009
corr emersav my_fksum my_sk my_fincon my_finacc if year==2012
corr emersav my_fksum my_sk my_fincon my_finacc if year==2015
corr emersav my_fksum my_sk my_fincon my_finacc if year==2018



meprobit emersav  ///
    i.year $indiv_varsreg fksum dy_sk my_sk fincon dy_finacc my_finacc  ///
   || state: 


meprobit emersav  ///
    $indiv_varsreg $state_vars  dy_fincon my_fincon dy_finacc my_finacc ///
   || state: 
   
* state level simulation
meprobit emersav  ///
    i.year $indiv_varsreg $state_vars m_finacc ///
   || state: ,  
   
meprobit emersav  ///
    i.year $indiv_varsreg $state_vars m_finacc i.state_fips ///
   || state: ,  
  




   
   
   
   
   
   
   
* generate two level models, state as group variable, year as control
*model 1_2 base*
mixed finresstd ///
   i.year  ///
   || state: ,  difficult stddev
estimates store m1_2
estimates restore m1_2
estat icc
	estadd scalar icc2 = r(icc2)
estimates store m1_2
estimates save m1_2, replace

* i.female i.agecat i.minority i.nonmarried --> consider this for appendix
mixed finresstd ///
    i.year ///
   || state: ,  difficult stddev
estimates store cm1
mixed finresstd  ///
   i.year i.inccat  ///
   || state: ,  difficult stddev
estimates store cm2
mixed finresstd  ///
   i.year i.inccat i.agecat ///
   || state: ,  difficult stddev
estimates store cm2_2
mixed finresstd  ///
   i.year i.inccat i.female  ///
   || state: ,  difficult stddev
estimates store cm3
mixed finresstd  ///
   i.year i.inccat i.female i.agecat ///
   || state: ,  difficult stddev
estimates store cm4
mixed finresstd  ///
   i.year i.inccat i.female i.agecat i.minority ///
   || state: ,  difficult stddev
estimates store cm5
mixed finresstd  ///
   i.year i.inccat i.female i.agecat i.minority i.nonmarried ///
   || state: ,  difficult stddev
estimates store cm6
esttab cm1 cm2 cm3 cm4 cm5 cm6, se bic aic mtitles stardetach noparentheses nolines nobase noobs replace transform(ln*: exp(@) (@)) ///
   eqlabels("" "between group stateyr(_cons)" "within group residual", none) 

*model 2_2 add ind covariates*
mixed finresstd ///
   i.year  $indiv_varsreg ///
   || state: ,  difficult stddev
estimates store m2_2
estimates restore m2_2
estat icc
	estadd scalar icc2 = r(icc2)
estimates store m2_2
estimates save m2_2, replace

*model 3_2 
mixed finresstd ///
   i.year $indiv_varsreg ///
   $state_vars ///
   || state: ,  difficult stddev
estimates store m4_2
estimates restore m4_2
estat icc
	estadd scalar icc2 = r(icc2)
estimates store m3_2
estimates save m3_2, replace

*model 4_2 
mixed finresstd ///
   i.year $indiv_varsreg ///
   $state_vars transfershare ///
   || state: ,  difficult stddev
estimates store m4_2
estimates restore m4_2
estat icc
	estadd scalar icc2 = r(icc2)
estimates store m4_2
estimates save m4_2, replace

*model 5_2 add poveffort*
mixed finresstd ///
   i.year $indiv_varsreg ///
   $state_vars poveffort ///
   || state: ,  difficult stddev
estimates store m5_2
estimates restore m5_2
estat icc
	estadd scalar icc2 = r(icc2)
estimates store m5_2
estimates save m5_2, replace

*model 6_2 add welfare genorosity*
mixed finresstd ///
   i.year $indiv_varsreg ///
   $state_vars wg_index ///
   || state: ,  difficult stddev
estimates store m6_2
estimates restore m6_2
estat icc
	estadd scalar icc2 = r(icc2)
estimates store m6_2
estimates save m6_2, replace

*model 7_2 add clusters*
mixed finresstd ///
   i.year $indiv_varsreg ///
   $state_vars i.cluster ///
   || state: ,  difficult stddev
estimates store m7_2
estimates restore m7_2
estat icc
	estadd scalar icc2 = r(icc2)
estimates store m7_2
estimates save m7_2, replace

*model 8_2 add all variables *
mixed finresstd ///
   i.year $indiv_varsreg ///
   $state_vars transfershare poveffort wg_index i.cluster  ///
   || state: ,  difficult stddev
estimates store m8_2
estimates restore m8_2
estat icc
	estadd scalar icc2 = r(icc2)
estimates store m8_2
estimates save m8_2, replace

esttab m1_2 m2_2 m3_2 m4_2 m5_2 m6_2 m7_2 using models_t1.csv, se bic aic mtitles stardetach noparentheses nolines nobase noobs replace transform(ln*: exp(@) (@)) ///
   eqlabels("" "between group stateyr(_cons)" "within group residual", none) 

*** r squared calculation
mixed finresstd  || state: ,  difficult 
mixed finresstd i.year $indiv_varsreg || state: ,  difficult 
mixed finresstd i.year $indiv_varsreg $state_vars || state: ,  difficult 
mixed finresstd i.year $indiv_varsreg $state_vars i.cluster || state: ,  difficult 
mixed finresstd i.year $indiv_varsreg $state_vars transfershare poveffort wg_index i.cluster || state: ,  difficult 


   ******************************
*** sensitivity tests   
*********************************
mixed finresstd ///
   i.year $indiv_varsreg ///
   $state_vars c.wg_index##i.inccat ///
   || state: ,  difficult stddev   
margins inccat, at(wg_index=(.43 .55 .75)) vsquish 


xtmixed finresstd ///
   i.year  ///
   || state: ,  difficult stddev  
mltrsq
xtmixed finresstd ///
   i.year $indiv_varsreg ///
   || state: ,  difficult stddev  
mltrsq



tabulate year, gen(y_)  
mixed finresstd  y_2 y_3 y_4 || state: y_2 y_3 y_4 ,  difficult  std   
   
   
/* next steps    
x revise and keep state level 2
x bring in fox data on genorosity
x control variables at level 2  
x separate out one model for low income one for not lowinc || or run interaction
x revisit income categories

********************************************************************************
/* BELOW HERE EXPLORATORY ANALYSIS , ARCHIVED August 7, 2020 */

*model 1_3 base*
clear 
use NFCS2018Stata_cleanstacked
mixed finresstd ///
   i.inccat i.year ///
   || state: || stateyr:,  difficult stddev
estimates store m1
estimates restore m1
estat icc
	estadd scalar icc3 = r(icc3)
	estadd scalar icc2 = r(icc2)
estimates store m1
estimates save m1, replace

*model 2_3 add ind covariates*
mixed finresstd ///
   i.inccat i.year $indiv_varsreg ///
   || state: || stateyr:,  difficult stddev
estimates store m2
estimates restore m2
estat icc
	estadd scalar icc3 = r(icc3)
	estadd scalar icc2 = r(icc2)
estimates store m2
estimates save m2, replace

*model 3_3 add transfer share*
mixed finresstd ///
   i.inccat i.year $indiv_varsreg ///
   transfershare ///
   || state: || stateyr:,  difficult stddev
estimates store m3
estimates restore m3
estat icc
	estadd scalar icc3 = r(icc3)
	estadd scalar icc2 = r(icc2)
estimates store m3
estimates save m3, replace

*model 4_3 add poveffort*
mixed finresstd ///
   i.inccat i.year $indiv_varsreg ///
   poveffort ///
   || state: || stateyr:,  difficult stddev
estimates store m4
estimates restore m4
estat icc
	estadd scalar icc3 = r(icc3)
	estadd scalar icc2 = r(icc2)
estimates store m4
estimates save m4, replace

esttab m1 m2 m3 m4, se bic aic mtitles stardetach noparentheses nolines nobase noobs replace transform(ln*: exp(@) (@)) ///
   eqlabels("" "state(_cons)" "stateyr(_cons)" "residual", none) scalars(icc3 icc2)

* generate similar two level models with stateyr
*model 1_3 base*
mixed finresstd ///
   i.inccat i.year ///
   || stateyr: ,  difficult stddev
estimates store m1_3
estimates restore m1_3
estat icc
	estadd scalar icc3 = r(icc3)
	estadd scalar icc2 = r(icc2)
estimates store m1_3
estimates save m1_3, replace

*model 2_3 add ind covariates*
mixed finresstd ///
   i.inccat i.year $indiv_varsreg ///
   || stateyr: ,  difficult stddev
estimates store m2_3
estimates restore m2_3
estat icc
	estadd scalar icc3 = r(icc3)
	estadd scalar icc2 = r(icc2)
estimates store m2_3
estimates save m2_3, replace

*model 3_3 add transfer share*
mixed finresstd ///
   i.inccat i.year $indiv_varsreg ///
   transfershare ///
   || stateyr: ,  difficult stddev
estimates store m3_3
estimates restore m3_3
estat icc
	estadd scalar icc3 = r(icc3)
	estadd scalar icc2 = r(icc2)
estimates store m3_3
estimates save m3_3, replace

*model 4_3 add poveffort*
mixed finresstd ///
   i.inccat i.year $indiv_varsreg ///
   poveffort ///
   || stateyr: ,  difficult stddev
estimates store m4_3
estimates restore m4_3
estat icc
	estadd scalar icc3 = r(icc3)
	estadd scalar icc2 = r(icc2)
estimates store m4_3
estimates save m4_3, replace

esttab m1_3 m2_3 m3_3 m4_3, se bic aic mtitles stardetach noparentheses nolines nobase noobs replace transform(ln*: exp(@) (@)) ///
   eqlabels("" "stateyr(_cons)" "residual", none) scalars(icc3 icc2)
   
*
*
* i.female i.agecat i.minority i.nonmarried
mixed finresstd ///
    i.year ///
   || state: ,  difficult stddev
estimates store cm1
mixed finresstd  ///
   i.year i.inccat  ///
   || state: ,  difficult stddev
estimates store cm2
mixed finresstd  ///
   i.year i.inccat i.female  ///
   || state: ,  difficult stddev
estimates store cm3
mixed finresstd  ///
   i.year i.inccat i.female i.agecat ///
   || state: ,  difficult stddev
estimates store cm4
mixed finresstd  ///
   i.year i.inccat i.female i.agecat i.minority ///
   || state: ,  difficult stddev
estimates store cm5
mixed finresstd  ///
   i.year i.inccat i.female i.agecat i.minority i.nonmarried ///
   || state: ,  difficult stddev
estimates store cm6
esttab cm1 cm2 cm3 cm4 cm5 cm6, se bic aic mtitles stardetach noparentheses nolines nobase noobs replace transform(ln*: exp(@) (@)) ///
   eqlabels("" "between group stateyr(_cons)" "within group residual", none) 

********************************************************************************
/* BELOW HERE EXPLORATORY ANALYSIS , ARCHIVED August 5, 2020 */

/* One way to do this is to compare state financial well-being scores after controlling for demographic characteristics. 
Some concern this may confound individual with group mechanisms */
 
// finresstd standardize variable for mean 3 items (j20 j5 j1)


* bivariate means by state
tabstat finresstd  [w=wgt_s3], by(stateq)

* Regression adjusted state
global covariates "i.female i.agecat i.minority i.nonmarried" 
regress finresstd  $covariates i.stateq i.year, vce(cluster stateq)
estpost margins stateq
eststo lpmstate
esttab using lpmstate_out.csv, label not nostar plain noobs replace



/* Another way is to do this in multilevel framework with a null model. How much ///
variation in outcome is explained by level 2 states? */
* first test residuals correlated with state, see robson multilevel book page 9
regress finresstd  $covariates 
predict L1_resid, residuals
anova L1_resid stateq
* run null model and calculate ICC
mixed finresstd  || state:, stddev
estat icc
* run null model and calculate ICC
mixed finresstd  if inccat==1 || state:, stddev
estat icc
* run null model and calculate ICC
mixed finresstd  if inccat==3 || state:, stddev
estat icc
* run null model and calculate ICC
mixed finresstd  if lowinc==1 || state:, stddev
estat icc
* finding low ICC, so exploring other outcome variables
mixed j1 || state:, stddev
estat icc
mixed j5 || state:, stddev
estat icc
mixed j20 || state:, stddev //.01
estat icc
mixed m1_1 if m1_1 <8 || state:, stddev
estat icc
mixed m4 if m4 <8 || state:, stddev
estat icc
* finding low ICC, so exploring age restriction
mixed finresstd  if agecat <6 & agecat>1 || state:, stddev
estat icc
mixed j20 if agecat <6 & agecat>1 || state:, stddev
estat icc
* finding low ICC, so exploring measurement - does age need to be restricted before calculating measure?
alpha j20 j1 j5 if agecat <6 & agecat>1, gen(finresstda) std
mixed finresstda if agecat <6 & agecat>1 || state:, stddev
estat icc
* finding low ICC, let's consider three levels with year-state, following hook et al 2020
egen yearstate = concat(year state) //153 year-state values
mixed finresstd  || yearstate: , stddev
estat icc
mixed finresstd  || state: || yearstate: [w=wgt_s3], nolog
estat icc
* regression adjust the scale to cancel out income? 
regress finresstd  i.inccat
predict finresstdinc, residuals
mixed finresstdinc  || state: 
estat icc
* j5, j1 alpha to include 2009
alpha j1 j5 if agecat <6 & agecat>1, gen(twofinresstda) std
mixed twofinresstda if agecat <6 & agecat>1 || state:, 
estat icc
* just binary j5 emergency savings
egen yearstateq = concat(track stateq) //153 year-state values
meprobit j5 if agecat <6 & agecat>1 || state:, 
keep if year == 2009 | year ==2012 | year ==2015 | year==2018
meprobit j5 if agecat <6 & agecat>1 [w=wgt_s3] || stateq: || yearstateq: 
// melogit example from Hook Paek 2020 base model 
// C:\Users\rothweld\Dropbox\Work\My Reference\Multilevel Analysis\Hook.Paek.2020.FampoliciesMotherEmploy
melogit j5  ///
   i.female i.agecat i.minority i.nonmarried if j5 !=. ///
   || state: || yearstate:, intmethod(laplace) difficult 
mat a=e(b)
// from Hook Paek 2020 model 1
melogit j5  ///
   i.female i.agecat i.minority i.nonmarried i.inccat if j5 !=. ///
   || state: || yearstate:, intmethod(laplace) difficult from(a)
estimates store m1
mat a2=e(b)
estimates restore m1
estat icc
	estadd scalar icc3 = r(icc3)
	estadd scalar icc2 = r(icc2)
estimates store m1
estimates save m1, replace




* proceeding despite low ICC
mixed finresstd  i.year || state: , nolog stddev
estat icc 
mixed finresstd  i.year || state: || yearstate: , nolog stddev
estat icc
mixed finresstd i.year i.female i.agecat i.minority i.nonmarried  || state: || yearstate: , nolog stddev
estat icc


* restart analysis check codes
* restrict to adults - remove young and old
* build off Montez 2016








*-------------------------------------------------------------------------------
*-------------------------------------------------------------------------------
keep if inccat<3 // keep only low and moderate income
fre female agecat minority nonmarried
tabstat finresstd [w=wgt_s3], by(stateq) save //state fin wellbeing
egen zfinreslm = std(finresstd) //standardize variable for mean of zero for low middle income
sum zfinreslm
tabstat zfinreslm [w=wgt_s3], by(stateq) save //state fin wellbeing

global covariates "i.female i.agecat i.minority i.nonmarried" 

regress zfinreslm $covariates i.stateq i.track, vce(cluster stateq)
regress zfinreslm $covariates i.stateq i.track, vce(cluster stateq)
predict finresp
sum finresp 

/*collapse finresp, by(track state_abbrev stateq state_fips) 
save collapsedfinresp, replace
sum finresp*/

collapse finresp [w=wgt_s3], by(track state_abbrev stateq state_fips TANFFSmean tanfratio inst6017_nom) 
save collapsedfinrespw, replace //create state level data set weighted at state
sum finresp

twoway (scatter finresp TANFFSmean)
twoway (scatter finresp tanfratio)
twoway (scatter finresp inst6017_nom)

regress finresp TANFFSmean tanfratio inst6017_nom i.track
margins , at(TANFFSmean=(700 1000 1300 1500))
margins , at(inst6017_nom =(20 40 60 80))

regress finresp inst6017_nom i.track //ideology no bivariate relationship but when controlling for welfare it becomes sig










********************************************************************************
********************************************************************************
* ARCHIVED CODE
*******
fre female agecat minority nonmarried
global covariates "i.female i.agecat i.minority i.nonmarried" 

******************************************************************
** Analysis
******************************************************************
* bivariate
tabstat finresstd [w=wgt_s3], by(stateq) save
tabstat finresstd if inccat==1 [w=wgt_s3], by(stateq)
tabstat finresstd if inccat==3 [w=wgt_s3], by(stateq)

tabstat finresstd if inccat==1 & track==1 [w=wgt_n2]
tabstat finresstd if inccat==1 & track==4 [w=wgt_n2]
tabstat finresstd if inccat==3 & track==1 [w=wgt_n2]
tabstat finresstd if inccat==3 & track==4 [w=wgt_n2]

* regression adjusted, informed by multilevel thinking 
* income gap
regress finresstd i.inccat
margins inccat

regress finresstd i.track
margins track

* income gap over time
regress finresstd i.inccat##i.track
margins track#inccat
marginsplot

* testing state effects, see robson multilevel book page 9
regress finresstd $covariates i.inccat i.track
predict L1_resid, residuals
anova L1_resid stateq

* full results 
regress finresstd $covariates i.stateq##i.inccat i.track, vce(cluster stateq)
margins stateq
margins inccat
margins stateq#inccat, post
estimates store modela
est table modela
esttab modela using modela.csv, not nostar plain

