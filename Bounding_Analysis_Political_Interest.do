

/*------------------------------------------------------------------------------
                      0. Necessary requirements
------------------------------------------------------------------------------*/

* Setup
version 18            // Stata version control 
clear all             // clear working memory
macro drop _all       // clear macros

* Working directory
global wdir  "Insert Here"

* Define paths to subdirectories 
global data 		"$wdir/0_data"   		// folder for original data
global code 		"$wdir/1_dofiles"   	// folder for do-files
global posted 		"$wdir/2_posted"    	// data ready for analysis
global temp 		"$wdir/3_temp"   		// folder for temporary files
global table		"$wdir/4_tables" 		// folder for table output 
global graph		"$wdir/5_graphs" 		// folder for graph output 

/*------------------------------------------------------------------------------
 1. Data preparations
------------------------------------------------------------------------------*/
	
	set maxvar 10000 //setting variable limit for loading dataset
	use "$data/ppathl.dta", clear //main dataset
	sort pid syear //sorting by person-identifier and survey year
	keep pid syear gebjahr sex phrf psample // keeping only relevant variables 
	xtset pid syear // defining longitudinal data set
//Merging additional dataset
	merge 1:1 pid syear using "$data/pl.dta", /// contains political variable
	keepusing  (plh0007) /// keeping only politial interest variable
	keep (1 3) nogen

//Bysorting and taging first observation-year per person
	bys pid (syear): gen n = _n
	bys pid: gen N = _N
	egen pickone = tag(pid)
	tab N if pickone == 1

	distinct pid // 1.446.266 person-years and 198.137 unique pid 

//Generating and Recoding necessary variables:

//Cohort
	g cohort = gebjahr
	recode cohort (-1=.) (2006/2021=.)
	la var cohort "Cohort"
	//generating cohort variable to keep original var untouched

//Age
	g age = syear - gebjahr
	recode age (1/16=.) (2001/2021=.) 
	la var age "Age"
	//recoding very sparse and false entries to missings

//Period
	g period = syear
	la var period "survey year"
	
//Political Interest
	gen polint = plh0007
	recode polint (-8/0=.) (1/2=1) (3/4=0)
	la var polint "Political Interest"

//Drop if missing
	distinct pid // 1.446.266 person-years and 198.137 pid
	drop if missing(polint, age, syear, gebjahr)
	distinct pid // 766,082 person-years and 114,978 pid
//Saving prepared dataset for further analysis
	save "$data/prepared_data.dta", replace
	clear

/*------------------------------------------------------------------------------
 2. Sampling and cutoffs
------------------------------------------------------------------------------*/	
	u "$data/prepared_data.dta", clear

// Cross-Sectional Weights normalized over survey years
	egen mean_weight = mean(phrf), by(period)
	bys period: g weight = phrf / mean_weight
	bys period: sum weight
	la var weight "cross-sectional normalized weight"
	drop mean_weight

	
/* Analysis restricted to 1992+ to reflect unified Germany.
 Sampling heterogeneity (migration and refreshment samples)
is not restricted as they reflect the German population. */
 
	keep if period >= 1992

// Plotting the observation density of the sample 

	preserve
	// Collapse to weighted density
    collapse (count) pid [pw=weight], by(age cohort)
    rename pid freq

    // Check if heatplot is installed
    cap ssc install heatplot

    // Defines the style of the lines 
    global line lc(gs8) lp(dash) lw(medthin)

    // Defining the lines as functions, these are picked based on the density 
	//after observing the heatplot previously
    global ageLO function 18, ra(1900 2025) $line
    global ageHI function 84, ra(1900 2025) $line
    global cohortLO function 1923, hor ra(0 100) $line
    global cohortHI function 2004, hor ra(0 100) $line

    // Create the heatplot with the overlay lines
	heatplot freq age cohort if inrange(cohort, 1900, 2020), ///
		c(viridis) discr aspect(1) keyl(, format(%9.0f) size (small)) ///
		xlabel(1900(20)2020, labsize(small) grid glw(0.2)) ///
		xmtick(1900(5)2025, grid glw(0.1)) ///
		ylabel(0(5)100, labsize(small) grid glw(0.2)) ///
		ymtick(0(1)100, grid glw(0.1)) ///
		title("Observation density in the SOEP, 1992–2023", size(medsmall)) ///
		subtitle("Weighted counts by age and birth cohort", size(small)) ///
		addplot($ageLO || $ageHI || $cohortLO || $cohortHI) ///
		leg(region(lw(0)))

    // Export graph
    graph export "$graph/fig_obsdensity_age_cohort.png", replace width(2400)

	restore

/* After inspecting the age x cohort density, age cutoffs will be made to ensure 
sufficient observation counts. Therefore ages will be resticted from 18 to 84 
and cohorts will be restricted to 1923 to 2004.*/

	clear //clearing working memory for accurate calculation of the weights 
	// after sample cutoffs are applied
	
//keeping relevant cohort and age ranges and drop missings
	u "$data/prepared_data.dta", clear
	keep if inrange(age, 18, 84)
	keep if inrange(cohort, 1923, 2004)
	keep if period >= 1992
	
	distinct pid // 107.252 pid and 668.786 person-years remain

//Re-applying and normalizing weights for new sample
	egen mean_weight = mean(phrf), by(period)
	bys period: g weight = phrf / mean_weight
	bys period: sum weight
	la var weight "cross-sectional normalized weight"
	drop mean_weight

	save "$data/analysis_data.dta", replace //dataset used for further analysis
	clear
/*------------------------------------------------------------------------------
 3. Descriptive checks and graphs
------------------------------------------------------------------------------*/

	u "$data/analysis_data.dta", clear


// NAIVE AGE PLOT

	preserve
    collapse (mean) polint [pweight=weight], by(age)
    twoway connected polint age, ///
		yscale(range(0 1)) ///
        yla(0(.25)1) xla(15(10)85) ///
		title("AGE") ///
		xtitle("Age (Years)") ///
		ytitle("Proportion of People Interested in Politics in Germany") ///
		ysize(1) xsize(3) ///
		msymbol(O) msize(0.1) ///
		lwidth(thin) ///

		graph save "$graph/naive_age.gph", replace 
	restore
	
// NAIVE PERIOD PLOT

	preserve
    collapse (mean) polint [pweight=weight], by(period)
    twoway connected polint period, ///
		yscale(range(0 1)) ///
        yla(0(.25)1) xla(1990(5)2025) ///
		title("PERIOD") ///
		xtitle("Survey Year") ///
		ytitle("") ///
		msymbol(O) msize(0.1) ///
		lwidth(thin) ///
		ysize(1) xsize(3) ///
        title("{bf:PERIOD}")
		graph save "$graph/naive_period.gph", replace 
	restore
	
// NAIVE COHORT PLOT
	preserve
    collapse (mean) polint [pweight=weight], by(cohort)
    
    twoway connected polint cohort, ///
        yscale(range(0 1)) ///
        yla(0(.25)1) xla(1920(20)2010) ///
		title("COHORT") ///
		xtitle("Birth Year") ///
		ytitle("") ///
        ysize(1) xsize(3) ///
		msymbol(O) msize(0.1) ///
		lwidth(thin) ///    
    graph save "$graph/naive_cohort.gph", replace
	restore

// Combining all three into one figure 
	graph combine "$graph/naive_age.gph" "$graph/naive_period.gph" "$graph/naive_cohort.gph"  , ///
    col(3) imargin(0 0 0 0) iscale(1) ///
    graphregion(color(white))

	graph export "$graph/naive_combined.png", replace width(2400) height(500)

/*------------------------------------------------------------------------------
 4. APC Analysis - Model Testing and Lower Age Bound
------------------------------------------------------------------------------*/

/* Inferred from the naive distribution non-linearities will be tested using 
Bayesian Information Criterion to evaluate Model Fit for the APC-Logit Model*/

/* Period effects are held constant using single-year dummy variables in order
 to flexibly capture sudden, short-term shocks. In contrast, age and cohort 
 effects are assumed to evolve more smoothly and are therefore modeled using 
 polynomial specifications.*/
 
//Main Model with Age^4 and Cohort^3

*Main Model:
	apcest logit polint [pweight=weight], a(age^4) p(ib2004.period) c(cohort^3)
	di e(acenter) // age center
	di e(ccenter) // cohort center
	estat ic // BIC = 869612.3

 
//Model specification testet against Main Model

*Test Model 1:
	apcest logit polint [pweight=weight], a(age^3) p(ib2004.period) c(cohort^2)
	di e(acenter) // age center
	di e(ccenter) // cohort center
	estat ic // BIC = 870563.7

*Test Model 2:
	apcest logit polint [pweight=weight], a(age^4) p(ib2004.period) c(cohort^2)
	di e(acenter) // age center
	di e(ccenter) // cohort center
	estat ic // BIC = 870258.2

*Test Model 3:
	apcest logit polint [pweight=weight], a(age^2) p(ib2004.period) c(cohort^3)
	di e(acenter) // age center
	di e(ccenter) // cohort center
	estat ic // BIC = 870148
	
*Test Model 4:
 	apcest logit polint [pweight=weight], a(age^3) p(ib2004.period) c(cohort^3)
	di e(acenter) // age center
	di e(ccenter) // cohort center
	estat ic // BIC = 870027.6
 
*Test Model 5: 
	apcest logit polint [pweight=weight], a(age^5) p(ib2004.period) c(cohort^3)
	di e(acenter) // age center
	di e(ccenter) // cohort center
	estat ic // BIC = 869624.7
 
/* 
	Model comparisons
	
	Main Model: Age^4 / Cohort^3   - 	BIC: 869612.3
	Test Model 1: Age^3 / Cohort^2 - 	BIC: 870563.7
	Test Model 2: Age^4 / Cohort^2 - 	BIC: 870258.2
	Test Model 3: Age^2 / Cohort^3 - 	BIC: 870148
	Test Model 4: Age^3 / Cohort^3 -    BIC: 870027.6
	Test Model 5: Age^5 / Cohort^4 - 	BIC: 869624.7
	
	The Bayesian Information Criterion hints at the Main Model to suit 
	the data the most. The tested Models lead to a significant decline
	in BIC-Values leading to the exclusion of them.
																			*/
																			
	
//Main Model
	apcest logit polint [pweight=weight], a(age^4) p(ib2004.period) c(cohort^3)
	di e(acenter) // age center
	di e(ccenter) // cohort center
	estat ic // BIC = 869612.3
	
//Plotting the nonlinear shapes of A,P,C only 
	apcplot, comb ///
    apl(xti(AGE) yla(-2(.5)2) xla(15(5)85)) ///
    ppl(xti(PERIOD) yla(-2(.5)2) xla(1990(5)2025)) ///
    cpl(xti(COHORT) yla(-2(.5)2) xla(1920(10)2005)) ///
    combpl(l1("EFFECT SIZE (LOGIT)"))
	graph export "$graph/APC_nonlin_shapes.jpg", replace width(2000)

//Identifying necessary rotation to achieve smallest Alpha to suit the theory

* Based on f′(age)≥0∀age∈[30,50] the lowest value must be 0.00241 to 
*achieve thisrotation. 
	apcplot A, ///
    g(.00241  +1) ///
    gridlab(off) ///
    gridpal(red) ///
    gridline(lw(.5)) ///
    shapepl(lc(black) lw(.6)) ///
    apl( ///
        xti(AGE) xla(15(10)85) ///
        yti("EFFECT SIZE (LOGIT)") ///
        yla(-0.8(.2)0.8) ///
        text(.2 25 "{it:α} = 0", place(r)) ///
        text(-0.6 25 "{it:α} = 0.002", place(r) color(red)))
	graph export "$graph/APC_Alpha.jpg", replace width(2000)

//Using Alpha 0.002, and using it as lower age bound

//Plotting possible solution ranges using single lower age bound
	apcbound, a(0.00241 .)
	apcplot, comb a(0.00241) g(.0005 +50) anc gridlab(off) gridline(lp(solid)) ///
    gridpal(HCL reds) gridf(1) shapepl(lc(black) lw(.6)) apl( ///
	yti("EFFECT SIZE (LOGIT)") xla(15(5)85)) ppl(xla(1990(5)2025)) ///
	cpl (xla(1920(10)2005)) 
	graph export "$graph/APC_age_semibounded.png",name(Combined) width(2000) replace
	
/*------------------------------------------------------------------------------
 5.  APC Analysis - Lower Period Bound
------------------------------------------------------------------------------*/	
	u "$data/analysis_data.dta", clear
	
//Main Model
	apcest logit polint [pweight=weight], a(age^4) p(ib2004.period) c(cohort^3)
	di e(acenter) // age center
	di e(ccenter) // cohort center
	estat ic // BIC = 869612.3
		
// lower age-period bounds (fully bounded)	
	apcbound, a(.00241 .) p(0 .) ci(95) 
	apcplot, b comb grad(50) $style areacon(lc(black)) areapal(viridis) ///
	cipl(fc(gs12) lw(0)) ci(95) apl( ///
	yti("EFFECT SIZE (LOGIT)") xla(15(5)85)) ppl(xla(1990(5)2025)) ///
	cpl (xla(1920(10)2005)) 
	graph export "$graph/APC_ageperiod_fullybound.jpg", replace width(2000)	



/*------------------------------------------------------------------------------
 6. APC Analysis - Upper Age Bound
------------------------------------------------------------------------------*/
	u "$data/analysis_data.dta", clear
//Main Model
	apcest logit polint [pweight=weight], a(age^4) p(ib2004.period) c(cohort^3)
	di e(acenter) // age center
	di e(ccenter) // cohort center
	estat ic // BIC = 869612.3
		
	
// The age curve is non-declining between 30 to 50 however the increase in that
// age span must remain modest. Therefore no more than 0.15 logit points. 
// Which corresponds to alpha= 0.00611 to satisfy the assumption.
	apcplot A, ///
    g(.00661 +1) ///
    gridlab(off) ///
    gridpal(red) ///
    gridline(lw(.5)) ///
    shapepl(lc(black) lw(.6)) ///
    apl( ///
        xti(AGE) xla(15(5)85) ///
        yti("EFFECT SIZE (LOGIT)") ///
        yla(-0.6(.2)0.6) ///
        text(.2 25 "{it:α} = 0", place(r)) ///
        text(-0.6 25 "{it:α} = 0.0661", place(r) color(red)))
	graph export "$graph/APC_Alpha_experimental_upperage.jpg", replace width(2000)	

//Plotting fully-bounded solution 
	apcbound, a(.00241 0.00611) ci(95) 
	apcplot, b comb grad(50) $style areacon(lc(black)) areapal(viridis) ///
	cipl(fc(gs12) lw(0)) ci(95) apl( ///
	yti("EFFECT SIZE (LOGIT)") xla(15(5)85)) ppl(xla(1990(5)2025)) ///
	cpl (xla(1920(10)2005)) 
	graph export "$graph/APC_ageupper_fullybound.jpg", replace width(2000)	
	 	
	 
	 
	 
	 
/*------------------------------------------------------------------------------
 7. Appendix
------------------------------------------------------------------------------*/	 
	u "$data/analysis_data.dta", clear
//Main Model
	apcest logit polint [pweight=weight], a(age^4) p(ib2004.period) c(cohort^3)
	di e(acenter) // age center
	di e(ccenter) // cohort center
	estat ic // BIC = 869612.3


//Export publication-style table for Main Model
	esttab using "$table/main_model.rtf", replace ///
    title("Main Model") ///
    wide nomti ///
    b(%9.3f) ci(%9.3f) msign("–") align(l) ///
    order(age c.age#c.age c.age#c.age#c.age c.age#c.age#c.age#c.age ///
          cohort c.cohort#c.cohort c.cohort#c.cohort#c.cohort ///
          *.period _cons) ///
    coeflabels( ///
        age "Age" ///
        c.age#c.age "Age² (×1,000)" ///
        c.age#c.age#c.age "Age³ (×10,000)" ///
        c.age#c.age#c.age#c.age "Age⁴ (×100,000)" ///
        cohort "Cohort" ///
        c.cohort#c.cohort "Cohort² (×1,000)" ///
        c.cohort#c.cohort#c.cohort "Cohort³ (×10,000)" ///
        _cons "Constant") ///
    transform( ///
        c.age#c.age @*1000 @*1000 ///
        c.age#c.age#c.age @*10000 @*10000 ///
        c.age#c.age#c.age#c.age @*100000 @*100000 ///
        c.cohort#c.cohort @*1000 @*1000 ///
        c.cohort#c.cohort#c.cohort @*10000 @*10000) ///
    scalars("chi2 Χ²" "BIC BIC") ///
    addnotes("Dependent variable: Political Interest." ///
             "Reference category for survey year: 2004.")

//Further adjustments to the table are made in Word	 
	 

/* Testing wether the specification for a moderat increase to a medium 
increase during the mid-life still yields the same direction of effects 
for the cohort effects. The new deriveed value for alpha is calculated so 
that the increase should not exceed 0.3 logit points in increase during the 
age span of 30 to 50. Double the amount used in the calculation.
The corresponding value is 0.01361 to warrant this assumption.
*/

//Main Model
	apcest logit polint [pweight=weight], a(age^4) p(ib2004.period) c(cohort^3)
	di e(acenter) // age center
	di e(ccenter) // cohort center
	estat ic // BIC = 869612.3 
	 
//Plotting fully bounded solution 
	apcbound, a(.00241 0.01361) ci(95) 
	apcplot, b comb grad(50) $style areacon(lc(black)) areapal(viridis) ///
	cipl(fc(gs12) lw(0)) ci(95) apl( ///
	yti("EFFECT SIZE (LOGIT)") xla(15(5)85)) ppl(xla(1990(5)2025)) ///
	cpl (xla(1920(10)2005)) 
	graph export "$graph/APC_ageupper_fullybound_appendix.jpg", replace width(2000)	
		 
	 

	 
	 
	 
	 
	 
	 
	 
	 