*********************************************
* Candidate Number 80						*
* Date: 11/16/2020							*
* Stata Version Used: Stata 16 SE			*
*********************************************

set seed 12345678 // set seed for replicability of results

clear all

* How can you run this file?
* Just change the path of the working folder in the next line
global projdir "C:\Users\12345\Downloads\Stata\"

* raw data folder
global raw "$projdir\Raw Data"

* folder where clean data is saved
global final "$projdir\Clean Data"

* folder where ouptut graphs and tables are saved
global output "$projdir\Output"

* folder where log files are saved
global logfiles "$projdir\Log Files"

log using "$logfiles\log-file.log", replace

********************************************************************************
* 							DATA PREPARATION								   *

* a. Load the student test data.
import delimited "$raw\student_test_data.csv"


* b. Drop all variables that contain personally identifiable information (PII).
/*
A combination of school ID and date of birth can make the student identifiable. 
So, I drop date of birth since it is not needed in analysis.
*/
drop dob // drop personally identifiable information


* c. Check and confirm that the values of the variable pupilid uniquely identify observations
duplicates report pupilid 
// we can observe that all 5797 observations have only a unique copy


* d. Recode the arithmetic score variables as integers rather than as strings (“NONE” is 0.)
forvalues i=1/8{
	// replace 'NONE's with 0's and then destring all the 8 arithmetic score variables
	replace a`i'_correct = "0" if a`i'_correct == "NONE"
	destring a`i'_correct, replace // replace string with integer values
}

save "$final/student_test_data.dta", replace // save the data

/* 
Now I save all the observation where values are negative and do not make sense in the dataset. 
These observations will be missing from final analysis. 
Hence, I save them for future referece, if they are needed later.
*/

foreach var of varlist w_* s_*{
	keep if `var'<0
}

save "$final/student_test_missing_data.dta", replace // save the missing data

use "$final/student_test_data.dta", clear // use the whole of student data and drop the observation with missing values

* e. The Word Reading and/or Sentence Reading sections are missing for some pupils. Missing values have been coded as -99. Drop them.
foreach var of varlist w_* s_*{
		// From observation, there are observation with value -98 in word reading scores.
		// I drop all observations where there word reading scores have negative value from the data
	drop if `var' < 0
}


* f. Describe what you would do to check for outliers in this dataset
/*
To check for outliers, I would create boxplots for all the variables related to different scores. 
This will help me examine the distribution of variables. 
The dots outside the range between upper adjacent value (Q3 + 1.5*IQR) 
and lower adjacent value (Q1 – 1.5*IQR) will show outliers for each variable.
*/


* g. Generate a variable for each of the all the test scores
gen reading_correct = w_correct + s_correct // Total Reading Score
gen arithmetic_correct = 0 
forvalues i=1/8{
	// find Total Arithmetic Score by adding the variables a1_correct through a8_correct for each pupil
	replace arithmetic_correct = arithmetic_correct + a`i'_correct
}
gen tot_score = reading_correct + arithmetic_correct + spelling_correct // Total score for each pupil


* h Assign grade to each pupil as follows - 4: 80–98; 3: 60–79; 2: 40–59; 1: 20–39; 0: 0–19.
recode tot_score (0/19 = 0) (20/39 = 1) (40/59 = 2) (60/79 = 3) (80/98 = 4), gen(grade) 

* i Standardize all the pupil scores relative to the control(non-tracking) group.
foreach var of varlist tot_score reading_correct arithmetic_correct spelling_correct{
	// use the mean and standard deviation of the control group to generate the z-scores
	qui sum `var' if tracking == 0
	gen z_`var' = (`var' - r(mean))/r(sd)
}

* j. Assign labels to the standardized test scores
label var grade "Letter Grade"
label var z_tot_score "Standardized Total Score"
label var z_reading_correct "Standardized Reading Score"
label var z_arithmetic_correct "Standardized Arithmetic Score"
label var z_spelling_correct "Standardized Spelling Score"

* k. Save the student test data
save "$final/student_test_clean_data.dta", replace // save the data

clear all

* l. Load the teacher data
use "$raw\teacher_data.dta"

* m. Randomly select 40% of the teachers in this dataset and save a separate dataset
preserve // preserve the teacher data
sample 40 //Randomly select 40% of the teachers in this dataset
export delimited using "$output/teacher_sample_40", replace // Save as a .csv file
restore

* n. Compute the average years of experience by school.
* o. Reshape the data so that observations are uniquely identified by school
/*
I create a dummy for teachers which have missing teacher experience.
Then, I find the mean of this dummy and the teacher experience at the school level by collapsing.
For those schools where average of dummy is more than 0, I assign the average teacher experience as missing.
*/
gen flag = (yrstaught==.) // flag observation with missing values in yrstaught
collapse (mean) avg_exp = yrstaught flag, by(schoolid)
replace avg_exp = . if flag > 0
drop flag

// save the dataset for average teacher experience for each school
save "$final/school_teacher_avg_data.dta", replace

* p. Merge with the dataset saved in part k.
merge 1:n schoolid using "$final/student_test_clean_data.dta"

* q. Label all variables you will use in the analysis portion
label var schoolid "School ID"
label var avg_exp "School avg teacher experince"
label var pupilid "Pupil ID"
label var district "District Name"
label var zone "Zone Name"
label var tracking "Tracking treatment dummy"
label var girl "Female pupil dummy"
label var spelling_correct "Total Spelling Score"
label var tot_score "Total Score"
label var arithmetic_correct "Total Arithmetic Score"
label var reading_correct "Total Reading Score"
encode zone, gen(zones)


* r. Save the merged data
save "$output/merged_student_teacher_data.dta", replace

********************************************************************************
* 								ANALYSIS									   *

* a. Regress (with OLS) the standardized total score on the tracking dummy with zone fixed effects and standard errors clustered at school level
reg z_tot_score tracking i.zones, vce(cluster schoolid)
est store reg_a
/*
In the model of zone-fixed effects without any other controls: 
On average, tracking is associated with an increase of 0.154 in standardized total score 
keeping other factors constant. This result is statistically significant at 95% significance level. 
A p-value of 0.035 means that there is only a 3.5% chance of rejecting the null hypothesis (point estimate = 0) when it is true. 
That is, the probability that the point estimate is produced just by random change is only 0.035.
*/


* b. Regress each of the standardized subsection scores (reading, spelling, and arithmetic) on the tracking dummy individually
foreach var of varlist z_reading_correct z_arithmetic_correct z_spelling_correct{
	di "Regression of `var' against tracking" 
	reg `var' tracking i.zones, vce(cluster schoolid)
}


* d. Regress (with OLS) the standardized total score on the tracking dummy with control for gender of the pupil and for the average years of experience of teachers at the pupil’s school as well as zone fixed effects and standard errors clustered at school level
reg z_tot_score tracking girl avg_exp i.zones, vce(cluster schoolid)
est store reg_d

/*
In the model of zone-fixed effects and controlling for the gender of the pupil 
and for the average years of experience of teachers at the pupil’s school: 
On average, tracking is associated with an increase of 0.075 in standardized total score 
keeping other factors constant. However, this result is not statistically significant at 95% significance level. 
A p-value of 0.433 means that there is only a 43.3% chance of rejecting the null hypothesis (point estimate = 0) when it is true. 
That is, the probability that the point estimate is produced just by random change is as high as 0.433.

This model, however, runs only on observations which have 
average teacher experience data for pupil's school and data on pupil's gender.
So, it might be misleading to compare it with model in part a.

Hence, I re-run the regression in part a. with only those observations 
which have average teacher experience data for pupil's school and data on pupil's gender
*/

reg z_tot_score tracking i.zones if !missing(avg_exp) & !missing(girl), vce(cluster schoolid)
est store reg_a2

/*
From this restricted dataset, we find that 
On average, tracking is associated with an increase of 0.079 in standardized total score 
keeping other factors constant. However, this result is not statistically significant at 95% significance level. 
A p-value of 0.402 means that there is a 40.3% chance of rejecting the null hypothesis (point estimate = 0) when it is true. 
That is, the probability that the point estimate is produced just by random change is as high as 0.402.

There seems to be no effect of tracking on total standardized scores in this previous specification (in part a.) as well,
similar to when we control for pupil's gender and pupil's school average teacher experience (in part d.)

The null result obtained in part d. is robust to exclusion of controls for pupil’s gender 
and for average teacher experience at their school from the regression specification. 
*/

* e. Create a regression table suitable for publication from the results from parts a and d.
outreg2[reg_a reg_d reg_a2] using "$output/Regression_results.tex", label drop(i.zones) addtext(Zone FE, YES) replace


* f. Bar chart suitable for publication that summarizes the average NON-STANDARDIZED arithmetic score for each of the two districts in the sample, separated by treatment
graph bar arithmetic_correct, over(tracking) over(district) ytitle("")  title("Average Non-standardized Arithmetic Test Score") subtitle("Maximum Score Possible = 24") ascategory asyvars bar(1, fcolor(maroon)) bar(2, fcolor(orange)) blabel(total, format(%9.2f)) legend(label(1 "Non-Tracking") label(2 "Tracking")) bargap(5)
graph export "$output/bar_graph.png", replace


log close