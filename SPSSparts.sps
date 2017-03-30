* Encoding: UTF-8.
*  code for A Gentle but Critical Introduction to Statistical Inference, Moderation, and Mediation.

* Section 2.2.2

* Load data: candies.sav.
DATASET NAME Candies WINDOW=FRONT.

* Exercise 1: Bootstrap different averages.
* Check data.
FREQUENCIES VARIABLES=colour weight
  /ORDER=ANALYSIS.
* Execute independent-samples t test with bootstrap.
BOOTSTRAP
  /SAMPLING METHOD=SIMPLE
  /VARIABLES TARGET=weight INPUT=colour 
  /CRITERIA CILEVEL=95 CITYPE=BCA  NSAMPLES=5000
  /MISSING USERMISSING=EXCLUDE.
T-TEST GROUPS=colour(4 5)
  /MISSING=ANALYSIS
  /VARIABLES=weight
  /CRITERIA=CI(.95).
* Set table output format to compact.
OUTPUT MODIFY
  /REPORT PRINTREPORT=NO
  /SELECT  TABLES
  /DELETEOBJECT DELETE=NO
  /OBJECTPROPERTIES   VISIBLE=ASIS
  /TABLE  TLOOK="Compact".
* EXPORT TABLES AS bootstrap.htm (CANNOT BE PASTED).

* Exercise 2.
* Bootstrap on median candy weight.
* Check data.
FREQUENCIES VARIABLES=weight
  /ORDER=ANALYSIS.
* Bootstrap the median.
BOOTSTRAP
  /SAMPLING METHOD=SIMPLE
  /VARIABLES INPUT=weight 
  /CRITERIA CILEVEL=95 CITYPE=BCA  NSAMPLES=5000
  /MISSING USERMISSING=EXCLUDE.
FREQUENCIES VARIABLES=weight
  /FORMAT=NOTABLE
  /STATISTICS=MEDIAN
  /ORDER=ANALYSIS.

* Section 2.4.2 

* Exercise 1.
* Exact test on the relation between candy colour and candy stickiness.
* Don't forget to deselect bootstrapping.
CROSSTABS
  /TABLES=colour BY sticky
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI 
  /CELLS=COUNT COLUMN 
  /COUNT ROUND CELL
  /METHOD=EXACT TIMER(5).
* Set table output format to compact.
OUTPUT MODIFY
  /REPORT PRINTREPORT=NO
  /SELECT  TABLES
  /DELETEOBJECT DELETE=NO
  /OBJECTPROPERTIES   VISIBLE=ASIS
  /TABLE  TLOOK="Compact".
* SAVE AS fisher.htm.

* Exercise 2.
* Binomial test.
NPAR TESTS
  /BINOMIAL (0.50)=sticky
  /MISSING ANALYSIS
  /METHOD=EXACT TIMER(5).

* Section 3.6.2.

* Exercise 1.
* Check data.
FREQUENCIES VARIABLES=weight
  /FORMAT=NOTABLE
  /HISTOGRAM NORMAL
  /ORDER=ANALYSIS.
* 95% CI.
T-TEST
  /TESTVAL=0
  /MISSING=ANALYSIS
  /VARIABLES=weight
  /CRITERIA=CI(.95).
* 99% CI.
T-TEST
  /TESTVAL=0
  /MISSING=ANALYSIS
  /VARIABLES=weight
  /CRITERIA=CI(.99).

* Exercise 2.
* Check data.
FREQUENCIES VARIABLES=weight
  /ORDER=ANALYSIS.
* Bootstrap on median candy weight.
BOOTSTRAP
  /SAMPLING METHOD=SIMPLE
  /VARIABLES INPUT=weight 
  /CRITERIA CILEVEL=95 CITYPE=BCA  NSAMPLES=5000
  /MISSING USERMISSING=EXCLUDE.
FREQUENCIES VARIABLES=weight
  /FORMAT=NOTABLE
  /STATISTICS=MEDIAN
  /ORDER=ANALYSIS.

* Exercise 3.
* Check data.
FREQUENCIES VARIABLES=colour_pre colour_post
  /FORMAT=NOTABLE
  /HISTOGRAM NORMAL
  /ORDER=ANALYSIS.
* Paired-samples t test.
T-TEST PAIRS=colour_pre WITH colour_post (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

*  Exercise 4.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT colour_post
  /METHOD=ENTER weight sweetness.

* Section 4.2.2.

* Load data: households.sav.
DATASET NAME Households WINDOW=FRONT.

*  Exercise 1.
* Check data.
FREQUENCIES VARIABLES=tv_reach
  /ORDER=ANALYSIS.
* Binomial test.
* Note: The test is one-sided if the test proprtion is not 0.50.
NPAR TESTS
  /BINOMIAL (0.40)=tv_reach
  /MISSING ANALYSIS.

*  Exercise 2.
* Check data.
FREQUENCIES VARIABLES=tv_reach
  /ORDER=ANALYSIS.
* Binomial test.
* Hint: Test the proportion of households not reached  because this is the first category: 1 - 0.55 = 0.45.
NPAR TESTS
  /BINOMIAL (0.45)=tv_reach
  /MISSING ANALYSIS.

* Exercise 3.
* Check data.
FREQUENCIES VARIABLES=income
  /ORDER=ANALYSIS.
* Binomial test.
* Use the cut of option in the binomial test.
NPAR TESTS
  /BINOMIAL (0.50)=income (40000)
  /MISSING ANALYSIS.

*  Exercise 4.
* Check data.
FREQUENCIES VARIABLES=income
  /ORDER=ANALYSIS.
* Recoding income into groups.
RECODE income (Lowest thru 30000=1) (30000  thru 50000=2) (50000 thru Highest=3) INTO income_group.
VARIABLE LABELS  income_group 'Grouped income'.
EXECUTE.
* Define Variable Properties.
*income_group.
VALUE LABELS income_group
  1.00 'low'
  2.00 'medium'
  3.00 'high'.
EXECUTE.
* one-sample chi-squared test.
NPAR TESTS
  /CHISQUARE=income_group
  /EXPECTED=20 50 30
  /MISSING ANALYSIS.

* Section 4.2.4.2.

* Load data: children.sav.
DATASET NAME Children WINDOW=FRONT.

*  Exercise 1.
* Check data.
FREQUENCIES VARIABLES=supervision
  /ORDER=ANALYSIS.
* Set imposible value (25) to missing.
* Define Variable Properties.
*supervision.
MISSING VALUES supervision(25.00).
EXECUTE.
* One-sample t test.
T-TEST
  /TESTVAL=5.5
  /MISSING=ANALYSIS
  /VARIABLES=supervision
  /CRITERIA=CI(.95).

*  Exercise 2.
* Check data.
FREQUENCIES VARIABLES=supervision
  /ORDER=ANALYSIS.
* Set imposible value (25) to missing.
* Define Variable Properties.
*supervision.
MISSING VALUES supervision(25.00).
EXECUTE.
* One-sample t test.
T-TEST
  /TESTVAL=4.5
  /MISSING=ANALYSIS
  /VARIABLES=supervision
  /CRITERIA=CI(.95).

* Section 4.2.6.2.

* Load data: voters.sav.
DATASET NAME Voters WINDOW=FRONT.

*  Exercise 1.
* Check data.
FREQUENCIES VARIABLES=age_group immigrant
  /HISTOGRAM NORMAL
  /ORDER=ANALYSIS.
* Independent-samples t test with Levene s test.
T-TEST GROUPS=age_group(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=immigrant
  /CRITERIA=CI(.95).

*  Exercise 2.
* Check data.
FREQUENCIES VARIABLES=age immigrant
  /ORDER=ANALYSIS.
* Group age.
RECODE age (Lowest thru 35=1) (36 thru 65=2) (66 thru Highest=3) INTO age3.
VARIABLE LABELS  age3 'Voter ages in three groups'.
EXECUTE.
* Define Variable Properties.
*age3.
VALUE LABELS age3
  1.00 '18-35'
  2.00 '36-65'
  3.00 '66+'.
EXECUTE.
* ANOVA with descriptives.
ONEWAY immigrant BY age3
  /STATISTICS DESCRIPTIVES HOMOGENEITY 
  /MISSING ANALYSIS.

* Section 4.2.5.

* Load data: donors.sav.
DATASET NAME Donors WINDOW=FRONT.

*  Exercise 1.
ONEWAY willing_post BY endorser
  /STATISTICS DESCRIPTIVES HOMOGENEITY 
  /PLOT MEANS
  /MISSING ANALYSIS
  /POSTHOC=BONFERRONI ALPHA(0.05).

*  Exercise 2.
T-TEST GROUPS=remember(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=willing_post
  /CRITERIA=CI(.95).
* The difference is significant but those who do NOT remember have higher average willingness.

*  Exercise 3.
T-TEST PAIRS=willing_pre WITH willing_post (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

* Section 4.2.6.

* Load data.
GET
  FILE='\data\consumers.sav'.
DATASET NAME Consumers WINDOW=FRONT.

*  Exercise 1.
CORRELATIONS
  /VARIABLES=ad_expo brand_aw
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.
NONPAR CORR
  /VARIABLES=ad_expo brand_aw
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

*  Exercise 2.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT brand_aw
  /METHOD=ENTER ad_expo wom gender.

*  Exercise 3.
CROSSTABS
  /TABLES=wom BY gender
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI LAMBDA 
  /CELLS=COUNT COLUMN 
  /COUNT ROUND CELL
  /BARCHART.

