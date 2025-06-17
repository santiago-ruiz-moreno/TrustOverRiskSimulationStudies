
* Power Simulations
* Controlling for Multiple Hypothesis Testing, and 

use "C:\Users\jruizmor\OneDrive - WU Wien\03 TrustOverRiskLLM\02 Design and implementation\02 Implementation code\00 Power Simulations\berg_1995_experiment_data.dta", clear

* Based on the Sample From Berg (1995), We used the same effect sizes
* of his Social History Treatment for our LLMsDecisionAid and increase the 
* effect size by 10% by keeping the same distribution for our Treatment AutonomousLLMs

preserve
drop sent_back
gen orig_id = _n 
gen treat2_flag = (Treatment == 1)
keep if treat2_flag
replace Treatment = 2
* Effect Size Expected for Treatment 2
replace sent = sent + 2 
tempfile treat2
save `treat2'
restore
drop sent_back
append using `treat2'
drop treat2_flag orig_id
* Step 1: Define the label
label define treatment_lbl 0 "Baseline" 1 "LLMsDecisionAid" 2 "AutonomousLLMs"

* Histogram by group
histogram sent, by(Treatment, total) ///
    title("Histogram of 'sent' by Treatment Group")

* Step 2: Apply the label to the variable
label values TreatmentOne treatment_lbl

* No observed results with Linear Regression. * The treatment, even when sihfted of one unit, does not seem to have
* any linear effect on the variable Trust.
table Treatment, statistic(mean sent)
regress sent i.Treatment

gen ln_sent = ln(sent)
regress ln_sent i.Treatment

ranksum sent by(i.Treatment) if i.Treatment in (0,1)