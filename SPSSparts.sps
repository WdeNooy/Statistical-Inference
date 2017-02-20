* Encoding: UTF-8.
* SPSS for A Gentle but Critical Introduction to Statistical Inference.

* Section 2.1

GET
  FILE='\data\candies.sav'.
DATASET NAME Candies WINDOW=FRONT.

* SPSS output for bootstrapping.
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

* SPSS Exercise 2: Bootstrap on median candy weight.
BOOTSTRAP
  /SAMPLING METHOD=SIMPLE
  /VARIABLES INPUT=weight 
  /CRITERIA CILEVEL=95 CITYPE=BCA  NSAMPLES=5000
  /MISSING USERMISSING=EXCLUDE.
FREQUENCIES VARIABLES=weight
  /FORMAT=NOTABLE
  /STATISTICS=MEDIAN
  /ORDER=ANALYSIS.

* Section 2.2 

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

* SPSS Exercise 2.
NPAR TESTS
  /BINOMIAL (0.50)=sticky
  /MISSING ANALYSIS
  /METHOD=EXACT TIMER(5).

* Section 3.5.5.

* SPSS Exercise 1.
T-TEST
  /TESTVAL=0
  /MISSING=ANALYSIS
  /VARIABLES=weight
  /CRITERIA=CI(.95).
T-TEST
  /TESTVAL=0
  /MISSING=ANALYSIS
  /VARIABLES=weight
  /CRITERIA=CI(.99).

* SPSS Exercise 2.
BOOTSTRAP
  /SAMPLING METHOD=SIMPLE
  /VARIABLES INPUT=weight 
  /CRITERIA CILEVEL=95 CITYPE=BCA  NSAMPLES=5000
  /MISSING USERMISSING=EXCLUDE.
FREQUENCIES VARIABLES=weight
  /FORMAT=NOTABLE
  /STATISTICS=MEDIAN
  /ORDER=ANALYSIS.

* SPSS Exercise 3.
T-TEST PAIRS=colour_pre WITH colour_post (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

* SPSS Exercise 4.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT colour_post
  /METHOD=ENTER weight sweetness.

* Section 4.2.1.

GET
  FILE='\data\households.sav'.
DATASET NAME Households WINDOW=FRONT.

* SPSS Exercise 1.
* Note: The test is one-sided if the test proprtion is not 0.50.
NPAR TESTS
  /BINOMIAL (0.40)=tv_reach
  /MISSING ANALYSIS.

* SPSS Exercise 2.
* Hint: Test the proportion of households not reached  because this is the first category: 1 - 0.55 = 0.45.
NPAR TESTS
  /BINOMIAL (0.45)=tv_reach
  /MISSING ANALYSIS.

* SPSS Exercise 3.
* Use the cut of option in the binomial test.
NPAR TESTS
  /BINOMIAL (0.50)=income (40000)
  /MISSING ANALYSIS.

* SPSS Exercise 4.
* Recoding sweetness into groups.
RECODE income (Lowest thru 30000=1) (30000  thru 50000=2) (50000 thru Highest=3) INTO income_group.
VARIABLE LABELS  income_group 'Grouped income'.
EXECUTE.
* Define Variable Properties.
*sweet_group.
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

* Section 4.2.2.

* SPSS Exercise 1.
T-TEST
  /TESTVAL=2.8
  /MISSING=ANALYSIS
  /VARIABLES=weight
  /CRITERIA=CI(.95).

* Section 4.2.4.

GET
  FILE='\data\voters.sav'.
DATASET NAME Voters WINDOW=FRONT.

* SPSS Exercise 1.
* Group age.
RECODE age (Lowest thru 29=1) (30 thru 49=2) (50 thru 65=3) (66 thru Highest=4) INTO age4.
VARIABLE LABELS  age4 'Voter ages in four groups'.
EXECUTE.
* Define Variable Properties.
*age4.
VALUE LABELS age4
  1.00 '18-29'
  2.00 '30-49'
  3.00 '50-65'
  4.00 '66+'.
EXECUTE.
* ANOVA.
ONEWAY immigrant BY age4
  /STATISTICS HOMOGENEITY 
  /MISSING ANALYSIS.
* Note that the result is not significant, so we conclude that polarization is equal for all age groups.
* This contradicts the test result comparing only young to old voters.
* The polarization difference between the youngest and older age groups is hidden by the equal variances among the three groups of older voters.

* Section 4.2.5.

GET
  FILE='\data\donators.sav'.
DATASET NAME Donators WINDOW=FRONT.

* SPSS Exercise 1.
ONEWAY willing_post BY endorser
  /STATISTICS DESCRIPTIVES HOMOGENEITY 
  /PLOT MEANS
  /MISSING ANALYSIS
  /POSTHOC=BONFERRONI ALPHA(0.05).

* SPSS Exercise 2.
T-TEST GROUPS=remember(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=willing_post
  /CRITERIA=CI(.95).
* The difference is significant but those who do NOT remember have higher average willingness.

* SPSS Exercise 3.
T-TEST PAIRS=willing_pre WITH willing_post (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

* Section 4.2.6.

GET
  FILE='\data\consumers.sav'.
DATASET NAME Consumers WINDOW=FRONT.

* SPSS Exercise 1.
CORRELATIONS
  /VARIABLES=ad_expo brand_aw
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.
NONPAR CORR
  /VARIABLES=ad_expo brand_aw
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

* SPSS Exercise 2.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT brand_aw
  /METHOD=ENTER ad_expo wom gender.

* SPSS Exercise 3.
CROSSTABS
  /TABLES=wom BY gender
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI LAMBDA 
  /CELLS=COUNT COLUMN 
  /COUNT ROUND CELL
  /BARCHART.
