***************************************************************************
* Name: Alice Sanarico
* Purpose: Article Who Gets a Voice Online
* Machine: Personal PC
* Start date: 24/05/2025
* End date: 10/12/2025
* Dataset: Flash Eurobarometer 2574 (European Parliament Youth Survey)
*          avaiable at https://search.gesis.org/research_data/ZA7991
***************************************************************************

*======================================
**# VARIABLES
*======================================

*-----------------------
* MAIN INDEPENDENT VARIABLE
*-----------------------

rename d2 gender
fre gender
*drop 3 "In another way / Prefer not to say" because it's only 0.61%
drop if gender==3
fre gender
*gen dummy
gen gender_dummy = .
replace gender_dummy = 0 if gender == 1
replace gender_dummy = 1 if gender == 2
tab gender gender_dummy

*-----------------------
* CONTROL VARIABLES
*-----------------------

//*----Z1 Discussion on political and social issues with friends and relatives (Frequently, Occasionally, Never) - Q2 --> pol_disc

fre q2
drop if q2 == 998

gen pol_disc = .
replace pol_disc = 1 if q2 == 3
replace pol_disc = 2 if q2 == 2
replace pol_disc = 3 if q2 == 1
tab pol_disc q2

lab var pol_disc "Discussion on political issues"
lab val pol_disc pol_disc
lab def pol_disc 1 "Never" ///
                 2 "Occasionally" ///
		         3 "Frequently", modify
fre pol_disc

codebook pol_disc, compact
fre pol_disc

//*----Z2 Parents' highest level of education (Lower secondary or less, Upper secondary, University) - Sd1 mum - Sd2 dad --> paredu

tab sd2, missing
tab paredu fina_sit, m

fre sd2
gen fedu=.

replace fedu=1 if sd2==1 | sd2==2 /*Lower than sec */
replace fedu=2 if sd2==3 | sd2==4 /*Secondary/Technical or vocational training*/
replace fedu=3 if sd2==5 /* University */

ta sd2 fedu, m

fre sd1
gen medu=.

replace medu=1 if sd1==1 | sd1==2 /*Lower than sec */
replace medu=2 if sd1==3 | sd1==4 /*Secondary/Technical or vocational training*/
replace medu=3 if sd1==5 /* University */

ta sd1 medu, m

* Parental educational 
gen paredu=.
replace paredu=fedu if fedu>=medu ///
        & fedu!=. & medu!=. /* Both parents: father higher */
replace paredu=medu if fedu<medu  ///
        & fedu!=. & medu!=. /* Both parents: mother higher */
replace paredu=fedu if fedu!=. & medu==. /* Only father */
replace paredu=medu if medu!=. & fedu==. /* Only mother */

tab paredu, m

lab var paredu "parental education (dominance)"
lab val paredu paredu
lab def paredu ///
    1 "Primary or noedu" ///
    2 "Secondary" ///
    3 "Tertiary", modify

drop if paredu==.

//*----Z3 Perceived financial situation (Poor, Average, Good) - SD3 --> fina_sit

fre sd3
drop if sd3 == 998 | sd3 == 999
gen fina_sit = .
replace fina_sit = 1 if sd3 == 1 | sd3 == 2 | sd3 == 3
replace fina_sit = 2 if sd3 == 4
replace fina_sit = 3 if sd3 == 5
tab fina_sit sd3

lab var fina_sit "Perceived financial situation"
lab val fina_sit fina_sit
lab def fina_sit 1 "Poor" ///
                 2 "Average" ///
		         3 "Good", modify
fre fina_sit

*Z2+Z3

egen z_paredu = std(paredu)
egen z_finasit = std(fina_sit)

gen socialbg_z = z_paredu + z_finasit
fre socialbg_z
tab socialbg_z fina_sit

gen socialbg = paredu + fina_sit
fre socialbg
fre paredu
tab socialbg fina_sit

//*----Z4 Ethnic/religious minority or migrant status (Yes/No) - SD4 1 e 2 --> minority

fre sd4_1 sd4_2
gen minority_mig = sd4_1 + sd4_2
fre minority_mig

gen minority = .
replace minority = 1 if minority_mig == 0
replace minority = 0 if minority_mig == 1 | minority_mig == 2

tab minority_mig minority

lab var minority "Being a minority (Ethnic/religious minority or migrant status)"
lab val minority minority
lab def minority 1 "No" ///
                 0 "Yes", modify
fre minority

//*----Z5 Geographical area of residence (Rural area, Small/medium town, Large town/city) - D13 --> area_res

fre d13
rename d13 area_res
lab def area_res 1 "Rural area/village" ///
                 2 "Small/medium town" ///
		         3 "Large town/city", modify
fre area_res

//*----Z6 Age 3 cat - d1r --> age2
*Millennians born before 1996 so in 2021 (year of the survey) they were at least 25 (gen z 16-24 - Millennials 25-30)

fre d1r
rename d1r age3
fre age3

fre d1
gen age2=.
replace age2 = 0 if d1<25
replace age2 = 1 if d1>24
tab d1 age2
fre age2

label define age2 0 "Gen Z" 1 "Millennials"
label values age2 age2

//*----Z7 Having a job (job, student, other) D5 --> work

fre d5

gen work=.
replace work = 0 if d5<17
replace work = 1 if d5==18
replace work = 2 if d5==17 | d5>18

lab var work "Occupation"
lab val work work
lab def work 0 "Working" ///
             1 "Student (full-time)" ///
			 2 "Other", modify
fre work
tab d5 work

//*----Z8 Country --> ipscntry

fre ipscntry
describe ipscntry
*Already numeric, no need to encode


*-----------------------
* DEPENDENT VARIABLES
*-----------------------

// FACTOR ANALYSIS

factor q7_1 q7_2 q7_3 q7_4 q7_5 q7_6 q7_7 q7_8 q7_9  q7_10, pcf mineigen (1)
rotate, varimax
screeplot, yline(1) xlabel(1(1)10)

*what theory mainly says:

*formal q7_1 q7_2 q7_6 q7_8
*informal q7_3 q7_4 q7_5 q7_7
*online q7_9 q7_10

*=====formal=====*

alpha q7_1 q7_2 q7_6 q7_8
*.0183295 very low
alpha q7_1 q7_6
*.0647068 better but not enough
correlate q7_1 q7_6
*0.2604 still low -> keek only vote

*=====informal=====*

alpha q7_3 q7_4 q7_5 q7_7
*.0164646 very low

*=====online=====*
alpha q7_9 q7_10
correlate q7_9 q7_10
*0.2304 very low

* Formal politics index
fre q7_1
gen formal_pol = q7_1
fre formal_pol
egen formal_pol_z = std(formal_pol)
fre formal_pol_z

* Informal politics index
fre q7_3 q7_4 q7_5 q7_7
gen informal_pol = q7_3 + q7_4 + q7_5 + q7_7
fre informal_pol
egen informal_pol_z = std(informal_pol)
fre informal_pol_z
su informal_pol_z

* Online politics index
fre q7_9  q7_10
gen online_pol = q7_9 + q7_10
fre online_pol
egen online_pol_z = std(online_pol)
fre online_pol_z
sum online_pol_z

// vars controls

//1. be sure to have correct labels for all vars

* Label key variables
label variable gender    "Gender"
label variable paredu    "Parental education"
label variable fina_sit  "Perceived financial situation"
label variable age2      "Cohort"
label variable pol_disc  "Family political discussion"
label variable work      "Employment status"
label variable minority  "Minority background"
label variable area_res  "Area of residence"
label variable ipscntry  "Country fixed effects"

* Label dependent variables (indices)
label variable formal_pol_z   "Electoral participation"
label variable informal_pol_z "Informal participation"
label variable online_pol_z   "Online participation"

//2. check all distributions

global allvars formal_pol_z informal_pol_z online_pol_z gender paredu fina_sit pol_disc age2 work minority area_res ipscntry
summarize $allvars, detail

estpost summarize $allvars, detail
esttab ., cells("mean sd min max N") label

//3. check for missings		

misstable summarize $allvars

//4. check if using both parentaledu and finasit

tabstat paredu fina_sit, stats(mean sd min max)
tabulate paredu fina_sit, chi2

// chi2 = 714,7119 pr = 0.000
// v cramer = 0.147

*======================================
**# DESCRIPTIVE ANALYSIS
*======================================

table country gender, statistic(mean formal_pol) statistic(sd formal_pol) statistic(count formal_pol)

table country gender, statistic(mean formal_pol)
table country gender, statistic(mean informal_pol)
table country gender, statistic(mean online_pol)

collapse (mean) formal_pol informal_pol online_pol, by(country gender)
reshape wide formal_pol informal_pol online_pol, i(country) j(gender)

gen gap_formal   = formal_pol2 - formal_pol1
gen gap_informal = informal_pol2 - informal_pol1
gen gap_online   = online_pol2 - online_pol1

graph hbar gap_formal, over(country, sort(1)) ///
    blabel(bar, format(%4.2f)) ///
    ytitle("Female – Male difference") ///
    title("Gender gap in formal political participation by country")

tabstat formal_pol [aw=w1], by(gender) statistics(mean sd N)
mean formal_pol, over(gender)

tabstat informal_pol [aw=w1], by(gender) statistics(mean sd N)
mean formal_pol, over(gender)

tabstat online_pol [aw=w1], by(gender) statistics(mean sd N)
mean formal_pol, over(gender)

input str15 type d
"Formal"    0.16
"Informal"  0.11
"Online"    0.11
end

graph bar d, over(type) ///
    yline(0, lpattern(dash) lcolor(gs8)) ///
    ytitle("Cohen's d") ///
    title("Gender differences in political participation") ///
    blabel(bar, format(%4.2f))


*======================================
//## FINAL MODELS (WEIGHTED) ##//
*======================================
	
ssc install estout, replace

// °°°°°°°°°°°°°°°°°°°°°
** MODEL 1: MAIN EFFECTS
// °°°°°°°°°°°°°°°°°°°°°

global controls1 "i.paredu i.fina_sit i.age2 i.pol_disc i.minority i.work i.area_res i.ipscntry"

* Electoral participation
eststo formal, title("Formal pol"): ///
	reg formal_pol_z i.gender $controls1 [pweight=w1], cl(ipscntry)

* Informal participation
eststo informal, title("Informal pol"): ///
	reg informal_pol_z i.gender $controls1 [pweight=w1], cl(ipscntry)
	
* Online participation
eststo online, title("Online pol"): ///
	reg online_pol_z i.gender $controls1 [pweight=w1], cl(ipscntry)
 
* Output table
esttab formal informal online, b(%9.3f) se(%9.3f) mti label stats(r2 N, fmt(%9.3f %9.0g) labels("R-squared" "Observations"))

* Coefplot with main effects
coefplot (formal, msymbol(circle) mcolor(blue) ciopts(lcolor(blue))) ///
         (informal, msymbol(circle) mcolor(forest_green) ciopts(lcolor(forest_green))) ///
         (online, msymbol(circle) mcolor(purple) ciopts(lcolor(purple))) ///
         , drop(*.ipscntry *.area_res *par_edu *.work *.minority *_cons) ///
           xline(0, lstyle(dash) lcolor(black))
		   
// °°°°°°°°°°°°°°°°°°°°°
** MODEL 2
// °°°°°°°°°°°°°°°°°°°°°

global controls2 "i.paredu i.fina_sit i.gender#i.paredu i.gender#i.fina_sit i.age2 i.pol_disc i.minority i.work i.area_res i.ipscntry"

*Electoral participation
eststo formal2, title("Formal pol"): reg formal_pol_z i.gender $controls2 [pweight=w1], cl(ipscntry)

*Informal participation
eststo informal2, title("Informal pol"):reg informal_pol_z i.gender $controls2 [pweight=w1], cl(ipscntry)

*Online participation
eststo online2, title("Online pol"):reg online_pol_z i.gender $controls2 [pweight=w1], cl(ipscntry)

* Output table
esttab formal2 informal2 online2, b(%9.3f) se(%9.3f) mti label stats(r2 N, fmt(%9.3f %9.0g) labels("R-squared" "Observations"))

//marginsplot

* Electoral
reg formal_pol_z i.gender i.paredu i.fina_sit i.gender#i.paredu i.gender#i.fina_sit i.age2 i.pol_disc i.minority i.work i.area_res i.ipscntry [pweight=w1], cl(ipscntry)
margins paredu, at(gender=(1 2))
marginsplot, name(m1, replace) title("Formal Participation") ///
    recast(line) ///
    plot1opts(lcolor(blue) mcolor(blue)) ///
    plot2opts(lcolor(forest_green) mcolor(forest_green)) ///
    plot3opts(lcolor(purple) mcolor(purple))

* Informal
reg informal_pol_z i.gender i.paredu i.fina_sit i.gender#i.paredu i.gender#i.fina_sit i.age2 i.pol_disc i.minority i.work i.area_res i.ipscntry [pweight=w1], cl(ipscntry)
margins paredu, at(gender=(1 2))
marginsplot, name(m2, replace) title("Informal Participation") ///
    recast(line) ///
    plot1opts(lcolor(blue) mcolor(blue)) ///
    plot2opts(lcolor(forest_green) mcolor(forest_green)) ///
    plot3opts(lcolor(purple) mcolor(purple))


* Online
reg online_pol_z i.gender i.paredu i.fina_sit i.gender#i.paredu i.gender#i.fina_sit i.age2 i.pol_disc i.minority i.work i.area_res i.ipscntry [pweight=w1], cl(ipscntry)
margins paredu, at(gender=(1 2))
marginsplot, name(m3, replace) title("Online Participation") ///
    recast(line) ///
    plot1opts(lcolor(blue) mcolor(blue)) ///
    plot2opts(lcolor(forest_green) mcolor(forest_green)) ///
    plot3opts(lcolor(purple) mcolor(purple))

* Combine
graph combine m1 m2 m3, col(3)

// °°°°°°°°°°°°°°°°°°°°°
** MODEL 3
// °°°°°°°°°°°°°°°°°°°°°

//3A interaction age and pol_disc
//3B interaction age and ses

// Model 3A:

eststo online1, title("Online1"): reg online_pol_z i.age2 i.pol_disc i.gender i.paredu i.fina_sit i.minority i.work i.area_res i.ipscntry, cl(ipscntry)

eststo online2, title("Online2"): reg online_pol_z i.age2 i.pol_disc i.age2#i.pol_disc i.gender i.paredu i.fina_sit i.minority i.work i.area_res i.ipscntry, cl(ipscntry)

eststo online3, title("Online3"): reg online_pol_z i.age2 i.pol_disc i.gender i.paredu i.fina_sit i.age2#i.paredu i.age2#fina_sit i.minority i.work i.area_res i.ipscntry, cl(ipscntry)
 
esttab online1 online2 online3, b(%9.3f) se(%9.3f) mti label stats(r2 N, fmt(%9.3f %9.0g) labels("R-squared" "Observations"))

coefplot (formal)(informal)(online), drop(*.ipscntry *.area_res *par_edu *.work *.age2 *.minority *_cons) xline(0, lstyle(dash) lcolor(red))


// GRAPHS //

* --- Formal Politics ---
reg formal_pol_z i.gender##i.pol_disc $controls3B, cl(ipscntry)
margins gender, at(pol_disc=(1 2 3))  // adjust pol_disc values to your scale
marginsplot, noci title("Formal Politics") name(formal_plot, replace)

* --- Informal Politics ---
reg informal_pol_z i.gender##i.pol_disc $controls3B, cl(ipscntry)
margins gender, at(pol_disc=(1 2 3))
marginsplot, noci title("Informal Politics") name(informal_plot, replace)

* --- Online Politics ---
reg online_pol_z i.gender##i.pol_disc $controls3B, cl(ipscntry)
margins gender, at(pol_disc=(1 2 3))
marginsplot, noci title("Online Politics") name(online_plot, replace)

* --- Combine plots ---
graph combine formal_plot informal_plot online_plot, cols(1) title("Predicted Participation by Gender and Family Discussion")

//// Combine marginsplot ////

ssc install schemepack, replace
ssc install combomarginsplot, replace

		// MODEL 1 //

global controls1 "i.pol_disc i.paredu i.fina_sit i.minority i.age2 i.work i.area_res i.ipscntry"

// gender
reg formal_pol_z i.gender $controls1, cl(ipscntry)
margins gender, saving(model1_formal, replace)
reg informal_pol_z i.gender $controls1, cl(ipscntry)
margins gender, saving(model1_informal, replace)
reg online_pol_z i.gender $controls1, cl(ipscntry)
margins gender, saving(model1_online, replace)
combomarginsplot model1_formal  model1_informal model1_online, ///
    legend(order(1 "Formal" 2 "Informal" 3 "Online")) ///
    title("")

// paredu
reg formal_pol_z i.gender $controls1, cl(ipscntry)
margins paredu, saving(model1b_formal, replace)
reg informal_pol_z i.gender $controls1, cl(ipscntry)
margins paredu, saving(model1b_informal, replace)
reg online_pol_z i.gender $controls1, cl(ipscntry)
margins paredu, saving(model1b_online, replace)
combomarginsplot model1b_formal  model1b_informal model1b_online, ///
    legend(order(1 "Formal" 2 "Informal" 3 "Online")) ///
    title("")		
	
// finasit
reg formal_pol_z i.gender $controls1, cl(ipscntry)
margins fina_sit, saving(model1c_formal, replace)
reg informal_pol_z i.gender $controls1, cl(ipscntry)
margins fina_sit, saving(model1c_informal, replace)
reg online_pol_z i.gender $controls1, cl(ipscntry)
margins fina_sit, saving(model1c_online, replace)
combomarginsplot model1c_formal  model1c_informal model1c_online, ///
    legend(order(1 "Formal" 2 "Informal" 3 "Online")) ///
    title("")
		
	// MODEL 2 //

global controls2 "i.gender#i.paredu i.pol_disc i.paredu i.fina_sit i.minority i.age2 i.work i.area_res i.ipscntry"

// GENDER
	reg formal_pol_z i.gender $controls1, cl(ipscntry)
margins gender, saving(model2_formal, replace)
reg informal_pol_z i.gender $controls1, cl(ipscntry)
margins gender, saving(model2_informal, replace)
reg online_pol_z i.gender $controls1, cl(ipscntry)
margins gender, saving(model2_online, replace)
combomarginsplot model2_formal  model2_informal model2_online, ///
    legend(order(1 "Formal" 2 "Informal" 3 "Online")) ///
    title("")	
	
	
	
	
