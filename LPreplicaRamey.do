***LP-IV 
***following Jordà and Ramey (2016) code
***Andrea Recine

***LPs instrumental variable, for estimating directly cumulative multipliers
***Based in -Jorda and Ramey 2016 codes

****This code replicates Ramey (2016)


drop _all
clear all

set more 1
set matsize 1000
set mem 400m

capture log close



import excel "C:\Users\andre\Downloads\FiscalM\BEA\DataRec.xlsx", sheet("Data") firstrow

keep Date GCE GDPDEF2012 GDP 

drop if Date > td(31dec2019)

gen quarter = qofd(Date)
format quarter %tq

order quarter
drop Date
tsset quarter

gen DEF12 = GDPDEF2012/100

***Define real variable

foreach var in GDP GCE {
    gen r`var' = `var' / DEF12
}

** generate period

gen postwwii = 1


****Define trasformation following Bernardini et al. 2020
hpfilter rGDP, trend(rGDPtrend) smooth(10000)


gen y = rGDP/L1.rGDPtrend
gen g = rGCE/L1.rGDPtrend

****Define shock

local shock g
local sample postwwii
gen t = _n


if `sample'==postwwii {
  gen h = t - 1 /* h is the horizon */
}

else {
   gen h = t - 1 - 28;
}


**max horizon and lags

local hmax = 20
local p = 4

* INITIALIZE SUM OF EFFECTS TO 0 AND PARAMETERS SERIES TO MISSING;

foreach var in y g {
    quietly gen b`var' = .
    quietly gen lo90`var' = .
    quietly gen up90`var' = .
}


gen ftest = .

****Define a list for controls variable

global `shock'xlist L(1/`p').`shock' L(1/`p').y L(1/`p').g

*  ESTIMATE IRFs LPs;

forvalues i = 0/20 {

  foreach var in y g {
  
    ivreg2 F`i'.`var' `shock' $`shock'xlist if `sample'==1, robust bw(auto)
    gen b`var'h`i' = _b[`shock']
    gen se`var'h`i' = _se[`shock']
  
  }
  

foreach var in y g   { 
   
  quietly replace b`var' = b`var'h`i' if h==`i'
  quietly replace up90`var' = b`var'h`i' + 1.68*se`var'h`i' if h==`i'
  quietly replace lo90`var' = b`var'h`i' - 1.68*se`var'h`i' if h==`i'  
 }
}



*outsheet by lo90y up90y bg lo90g up90g using "C:\Users\andre\Documents\LPs\Jordà\testirfBP.csv", comma replace

gen time = _n

twoway (rarea lo90g up90g time if time <= 20, color(gs14%50)) /// shading area with gray color and 50% transparency
       (line bg time if time <= 20, lcolor(blue) lwidth(medium))

twoway (rarea lo90y up90y time if time <= 20, color(gs14%50)) /// shading area with gray color and 50% transparency
       (line by time if time <= 20, lcolor(blue) lwidth(medium))
	   
	   
	   
	   

*****ONE STEP LPs-IV CUMULATIVE MULTIPLIERS

foreach var in y g  {
  gen cum_`var' = 0
  gen mult_`var' = 0
  gen semult_`var' = 0
  
  
  
  forvalues i = 0/20 {
  
   gen cum`i'_`var' = F`i'.`var' + cum_`var'
	replace cum_`var' = cum`i'_`var'
  }	
}

**IV-regression

 forvalues i = 0/20 { 
 
    ivreg2 cum`i'_y (cum`i'_g = `shock') $`shock'xlist , robust bw(auto)			
				
     gen multh`i'_y = _b[cum`i'_g]
     gen semulth`i'_y = _se[cum`i'_g]	 
	 gen ftesth`i'= e(widstat) /* Kleibergen-Paap rk Wald F statistic*/
	 
     weakivtest /* Run this to get the Montiel and Pflueger's critical values*/
	
     quietly replace mult_y = multh`i'_y if h==`i'
     quietly replace semult_y = semulth`i'_y if h==`i'
	 quietly replace ftest = ftesth`i' if h==`i'
  
  }

display as text "MULTIPLIERS, STANDARD ERRORS, EFFECTIVE F-STATISTICS"

list h mult_y semult_y ftest if h<=20

* Output results to a .csv file so they can be imported to Excel for better graphs;
*outsheet h mult_y semult_y ftest using "C:\Users\andre\Documents\LPs\Jordà\CUMbp.csv" if h<=20, comma replace


capture log close;