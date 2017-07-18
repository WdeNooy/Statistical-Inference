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
* Check data: Assumption checks in Chapter 8.
* CHeck for impossible values.
FREQUENCIES VARIABLES=weight sweetness colour_post
  /ORDER=ANALYSIS.
* Regression of colour_post on weight and sweetness. 
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

* Section 4.2.9.2.

* Load data: donors.sav.
DATASET NAME Donors WINDOW=FRONT.

*  Exercise 1.
* Check data.
FREQUENCIES VARIABLES=willing_post endorser
  /ORDER=ANALYSIS.
* One-way analysis of variance.
ONEWAY willing_post BY endorser
  /STATISTICS DESCRIPTIVES HOMOGENEITY 
  /PLOT MEANS
  /MISSING ANALYSIS
  /POSTHOC=BONFERRONI ALPHA(0.05).

*  Exercise 2.
* Check data.
FREQUENCIES VARIABLES=willing_post remember
  /ORDER=ANALYSIS.
* Independent-samples t test.
T-TEST GROUPS=remember(0 1)
  /MISSING=ANALYSIS
  /VARIABLES=willing_post
  /CRITERIA=CI(.95).
* The difference is significant but those who do NOT remember have higher average willingness.

*  Exercise 3.
* Check data.
FREQUENCIES VARIABLES=willing_post willing_pre
  /ORDER=ANALYSIS.
* Paired-samples t test.
T-TEST PAIRS=willing_pre WITH willing_post (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

* Section 4.2.11.2.

* Load data: consumers.sav.
DATASET NAME Consumers WINDOW=FRONT.

*  Exercise 1.
* Check data.
FREQUENCIES VARIABLES=ad_expo brand_aw
  /ORDER=ANALYSIS.
* Check if the association can be linear.
GRAPH
  /SCATTERPLOT(BIVAR)=ad_expo WITH brand_aw
  /MISSING=LISTWISE.
* Correlations.
CORRELATIONS
  /VARIABLES=ad_expo brand_aw
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.
NONPAR CORR
  /VARIABLES=ad_expo brand_aw
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

*  Exercise 2.
* Check data.
FREQUENCIES VARIABLES=ad_expo brand_aw wom gender
  /ORDER=ANALYSIS.
* Turn dichotomies into 0/1 variables.
RECODE wom gender (2=1) (1=0) INTO heard male.
EXECUTE.
* Multiple regression.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT brand_aw
  /METHOD=ENTER ad_expo heard male.

*  Exercise 3.
* Crosstab with chi-squared test and measure of association.
CROSSTABS
  /TABLES=wom BY gender
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI LAMBDA 
  /CELLS=COUNT COLUMN 
  /COUNT ROUND CELL
  /BARCHART.

* Section 5.2.4.

* Load data: voters.sav.
DATASET NAME Voters WINDOW=FRONT.

*  Exercise 1.
* Check data.
FREQUENCIES VARIABLES=immigrant 
  /ORDER=ANALYSIS.
* One-sample t test.
T-TEST 
  /TESTVAL=6.0 
  /MISSING=ANALYSIS 
  /VARIABLES=immigrant 
  /CRITERIA=CI(.95).

*  Exercise 3.
* Check data.
FREQUENCIES VARIABLES=age_group 
  /ORDER=ANALYSIS.
* Independent-samples t test.
T-TEST GROUPS=age_group(1 2) 
  /MISSING=ANALYSIS 
  /VARIABLES=immigrant 
  /CRITERIA=CI(.95).

* Section 7.2.2.

* Load data: donors.sav.
DATASET NAME Donors WINDOW=FRONT.

*  Exercise 1.
* Check data.
FREQUENCIES VARIABLES=willing_post endorser
  /ORDER=ANALYSIS.
* One-way analysis of variance.
ONEWAY willing_post BY endorser
  /STATISTICS DESCRIPTIVES HOMOGENEITY 
  /PLOT MEANS
  /MISSING ANALYSIS
  /POSTHOC=BONFERRONI ALPHA(0.05).

*  Exercise 2.
* Load data: smokers.sav.
DATASET NAME Smokers WINDOW=FRONT.
* Check data.
FREQUENCIES VARIABLES=attitude status3
  /ORDER=ANALYSIS.
* One-way analysis of variance.
ONEWAY attitude BY status3
  /STATISTICS DESCRIPTIVES HOMOGENEITY 
  /PLOT MEANS
  /MISSING ANALYSIS
  /POSTHOC=BONFERRONI ALPHA(0.05).

* Section 7.6.2.

* Load data: donors.sav.
DATASET NAME Donors WINDOW=FRONT.

*  Exercise 1.
* Check data.
FREQUENCIES VARIABLES=willing_post endorser sex
  /ORDER=ANALYSIS.
* Two-way analysis of variance.
UNIANOVA willing_post BY endorser sex
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /POSTHOC=endorser(BONFERRONI) 
  /PLOT=PROFILE(endorser*sex)
  /PRINT=HOMOGENEITY DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=endorser sex endorser*sex.

*  Exercise 2.
* Check data.
FREQUENCIES VARIABLES=willing_post endorser remember
  /ORDER=ANALYSIS.
* Two-way analysis of variance.
UNIANOVA willing_post BY endorser remember
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /POSTHOC=endorser(BONFERRONI) 
  /PLOT=PROFILE(endorser*remember)
  /PRINT=HOMOGENEITY DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=endorser remember endorser*remember.

*  Exercise 3.
* Load data: smokers.sav.
DATASET NAME Smokers WINDOW=FRONT.
* Check data.
FREQUENCIES VARIABLES=status3 exposure attitude
  /ORDER=ANALYSIS.
* Group exposure to anti-smoking campaign.
RECODE exposure (Lowest thru 3=1) (3 thru 7 = 2) (ELSE=3) INTO exposure3.
VARIABLE LABELS  exposure3 'Exposure to anti-smoking campaign'.
EXECUTE.
* Define Variable Properties.
*exposure3.
VALUE LABELS exposure3
  1.00 'Low exposure'
  2.00 'Medium exposure'
  3.00 'High exposure'.
EXECUTE.
* Two-way analysis of variance.
UNIANOVA attitude BY status3 exposure3
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /POSTHOC=status3 exposure3(BONFERRONI) 
  /PLOT=PROFILE(status3*exposure3)
  /PRINT=HOMOGENEITY DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=status3 exposure3 status3*exposure3.

* Section 8.2.2.

* Load data: smokers.sav.
DATASET NAME Smokers WINDOW=FRONT.

*  Exercise 1.
* Check data.
FREQUENCIES VARIABLES=exposure attitude
  /ORDER=ANALYSIS.
* Simple regression analysis with assumption checks.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT attitude
  /METHOD=ENTER exposure
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS HISTOGRAM(ZRESID).

*  Exercise 2.
* Check data.
FREQUENCIES VARIABLES=exposure status3 contact attitude
  /ORDER=ANALYSIS.
* Create dummy variables for status3.
* ENSURE THAT MEASUREMENT LEVEL IS SET TO ORDINAL.
* Define Variable Properties.
*status3.
VARIABLE LEVEL  status3(ORDINAL).
EXECUTE.
SPSSINC CREATE DUMMIES VARIABLE=status3 
ROOTNAME1=status 
/OPTIONS ORDER=A USEVALUELABELS=YES USEML=YES OMITFIRST=NO.
* Multiple regression analysis with assumption checks.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT attitude
  /METHOD=ENTER exposure status_2 status_3 contact
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS HISTOGRAM(ZRESID).

* Exercise 3.
* Load data: children.sav.
DATASET NAME Children WINDOW=FRONT.
* Check data.
FREQUENCIES VARIABLES=medliter supervision
  /ORDER=ANALYSIS.
* Set supervision 25 to missing.
* Define Variable Properties.
*supervision.
MISSING VALUES supervision(25.00).
EXECUTE.
* Undirected: correlation (linear?).
* Check scatterplot.
GRAPH
  /SCATTERPLOT(BIVAR)=supervision WITH medliter
  /MISSING=LISTWISE.
* Correlations.
CORRELATIONS
  /VARIABLES=medliter supervision
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.
* Simple regression: media literacy dependent.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT medliter
  /METHOD=ENTER supervision
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS HISTOGRAM(ZRESID).
* Simple regression: parental supervision dependent.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT supervision
  /METHOD=ENTER medliter
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS HISTOGRAM(ZRESID).

* Section 8.4.2.

* Load data: smokers.sav.
DATASET NAME Smokers WINDOW=FRONT.

*  Exercise 1.
* Check data.
FREQUENCIES VARIABLES=exposure status2 contact attitude
  /ORDER=ANALYSIS.
* Compute interaction variable.
COMPUTE expo_status=exposure * status2.
VARIABLE LABELS  expo_status 'Interaction exposure * smoker'.
EXECUTE.
* Multiple regression.
* Statistic Descriptives is added to get the means that we need
* to plug into the regression equation in the moderaiton plot.
REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT attitude
  /METHOD=ENTER exposure status2 expo_status contact
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS HISTOGRAM(ZRESID).

*  Exercise 2.
* Scatterplot with dots coloured by smoking status.
GRAPH
  /SCATTERPLOT(BIVAR)=exposure WITH attitude BY status2
  /MISSING=LISTWISE.

*  Exercise 3.
* Histogram of predictor (exposure) for each smoking status.
GRAPH
  /HISTOGRAM=exposure
  /PANEL ROWVAR=status2 ROWOP=CROSS.

*  Exercise 4.
* Check data.
FREQUENCIES VARIABLES=exposure status3 contact attitude
  /ORDER=ANALYSIS.
* Create dummies and iteraction variables.
* ENSURE THAT MEASUREMENT LEVEL IS SET TO ORDINAL.
* Define Variable Properties.
*status3.
VARIABLE LEVEL  status3(ORDINAL).
EXECUTE.
SPSSINC CREATE DUMMIES VARIABLE=exposure status3 
ROOTNAME1=exposure, status ROOTNAME2=expo_status 
/OPTIONS ORDER=A USEVALUELABELS=YES USEML=YES OMITFIRST=NO.
* Multiple regression.
* Statistic Descriptives is added to get the means that we need
* to plug into the regression equation in the moderaiton plot.
REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT attitude
  /METHOD=ENTER exposure status_3 status_4 expo_status_2_2 expo_status_2_3 contact
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS HISTOGRAM(ZRESID).
* Scatterplot with dots coloured by smoking status.
GRAPH
  /SCATTERPLOT(BIVAR)=exposure WITH attitude BY status3
  /MISSING=LISTWISE.
* Histogram of predictor (exposure) for each smoking status.
GRAPH
  /HISTOGRAM=exposure
  /PANEL ROWVAR=status3 ROWOP=CROSS.

* Section 8.7.2.

* Load data: smokers.sav.
DATASET NAME Smokers WINDOW=FRONT.

*  Exercise 1.
* Check data.
FREQUENCIES VARIABLES=exposure status2 contact attitude
  /ORDER=ANALYSIS.
* Compute interaction variable.
COMPUTE expo_contact=exposure * contact.
VARIABLE LABELS  expo_contact 'Interaction exposure * contact'.
EXECUTE.
* Multiple regression.
* Statistic Descriptives is added to get the means that we need
* to plug into the regression equation in the moderaiton plot.
REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT attitude
  /METHOD=ENTER exposure contact expo_contact status2
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS HISTOGRAM(ZRESID).

*  Exercise 2.
* Create scatterplot.
GRAPH
  /SCATTERPLOT(BIVAR)=exposure WITH attitude
  /MISSING=LISTWISE.
* Manually add three regression lines.

*  Exercise 3.
* Check data.
FREQUENCIES VARIABLES=exposure status2 contact attitude
  /ORDER=ANALYSIS.
* Mean-center predictor and moderator.
* Ask for means of predictor and exposure.
FREQUENCIES VARIABLES=exposure contact
  /FORMAT=NOTABLE
  /STATISTICS=MEAN
  /ORDER=ANALYSIS.
* Subtract mean from variable.
COMPUTE exposure_c=exposure - 4.866.
VARIABLE LABELS  exposure_c 'Exposure (mean-centered)'.
COMPUTE contact_c=contact - 5.091.
VARIABLE LABELS  contact_c 'Contact (mean-centered)'.
EXECUTE.
* Compute new interaction variable.
COMPUTE expo_contact_c=exposure_c * contact_c.
VARIABLE LABELS  expo_contact_c 'Interaction exposure * contact  (mean-centered)'.
EXECUTE.
* Multiple regression.
* Statistic Descriptives is added to get the means that we need
* to plug into the regression equation in the moderation plot.
REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT attitude
  /METHOD=ENTER exposure_c contact_c expo_contact_c status2
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS HISTOGRAM(ZRESID).

* Exercise 4.
* Group the moderator.
* Visual Binning.
*contact.
RECODE  contact (MISSING=COPY) (LO THRU 4.25076386584132=1) (LO THRU 5.83711577142397=2) (LO THRU 
    HI=3) (ELSE=SYSMIS) INTO contact_3.
VARIABLE LABELS  contact_3 'Contact with smokers (Binned)'.
FORMATS  contact_3 (F5.0).
VALUE LABELS  contact_3 1 '' 2 '' 3 ''.
VARIABLE LEVEL  contact_3 (ORDINAL).
EXECUTE.
* Histograms of the predictor for each moderator group.
GRAPH
  /HISTOGRAM=exposure
  /PANEL ROWVAR=contact_3 ROWOP=CROSS.

* Exercise 5.
* Load data: children.sav.
DATASET NAME Children WINDOW=FRONT.
* Check data.
FREQUENCIES VARIABLES=medliter sex age supervision
  /STATISTICS=MEAN
  /ORDER=ANALYSIS.
* Set impossible values to missing.
* Define Variable Properties.
*sex.
MISSING VALUES sex(1).
*supervision.
MISSING VALUES supervision(25.00).
EXECUTE.
* Turn sex into a 0/1 variable.
RECODE sex (2=0) (3=1) INTO girl.
VARIABLE LABELS  girl 'The child is a girl.'.
EXECUTE.
* Mean-center predictor and moderator.
* Ask for means of predictor and exposure.
FREQUENCIES VARIABLES=age supervision
  /FORMAT=NOTABLE
  /STATISTICS=MEAN
  /ORDER=ANALYSIS.
* Subtract mean from variable.
COMPUTE age_c=age - 8.609.
VARIABLE LABELS  age_c 'Age (mean-centered)'.
COMPUTE supervision_c=supervision - 5.358.
VARIABLE LABELS  supervision_c 'Supervision (mean-centered)'.
EXECUTE.
* Check mean centering.
FREQUENCIES VARIABLES=age_c supervision_c
  /FORMAT=NOTABLE
  /STATISTICS=MEAN
  /ORDER=ANALYSIS.
* Compute interaction variable.
COMPUTE age_supervision_c=age_c * supervision_c.
VARIABLE LABELS  age_supervision_c 'Interaction age * supervision (mean-centered)'.
EXECUTE.
* Multiple regression.
* Statistic Descriptives is added to get the means that we need
* to plug into the regression equation in the moderation plot.
REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT medliter
  /METHOD=ENTER girl age_c supervision_c age_supervision_c
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS HISTOGRAM(ZRESID).
* Create scatterplot for moderation plot.
* Use the mean-centered variable.
GRAPH
  /SCATTERPLOT(BIVAR)=supervision_c WITH medliter
  /MISSING=LISTWISE.
* Manually add three regression lines.

* Exercise 6.
* Check data.
FREQUENCIES VARIABLES=medliter sex supervision
  /STATISTICS=MEAN
  /ORDER=ANALYSIS.
* Set impossible values to missing.
* Define Variable Properties.
*sex.
MISSING VALUES sex(1).
*supervision.
MISSING VALUES supervision(25.00).
EXECUTE.
* Turn sex into a 0/1 variable.
RECODE sex (2=0) (3=1) INTO girl.
VARIABLE LABELS  girl 'The child is a girl.'.
EXECUTE.
* Mean-center the predictor.
* Ask for means of parental supervision.
FREQUENCIES VARIABLES=supervision
  /FORMAT=NOTABLE
  /STATISTICS=MEAN
  /ORDER=ANALYSIS.
* Subtract mean from variable.
COMPUTE supervision_c=supervision - 5.358.
VARIABLE LABELS  supervision_c 'Supervision (mean-centered)'.
EXECUTE.
* Compute interaction variable.
COMPUTE girl_supervision_c=girl * supervision_c.
VARIABLE LABELS  girl_supervision_c 'Interaction girl * supervision (mean-centered)'.
EXECUTE.
* Multiple regression.
* Statistic Descriptives is added to get the means that we need
* to plug into the regression equation in the moderation plot.
REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT medliter
  /METHOD=ENTER girl supervision_c girl_supervision_c
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS HISTOGRAM(ZRESID).
* Scatterplot with dots coloured by sex.
* Use the mean-centered predictor.
GRAPH
  /SCATTERPLOT(BIVAR)=supervision_c WITH medliter BY girl
  /MISSING=LISTWISE.
* Note: This model does not contain a covariate, so SPSS can draw the lines.
* Command: Graphs > Regression Variable Plots; Color by: sex..
* With options: Scatterplot Fit Lines: Linear, Grouping: Fit Line for each categorical colour group.
* Use the mean-centered or not mean-centered predictor.
STATS REGRESS PLOT YVARS=medliter XVARS=supervision_c COLOR=sex 
/OPTIONS CATEGORICAL=BARS GROUP=1 INDENT=15 YSCALE=75 
/FITLINES LINEAR APPLYTO=GROUP.

* Section 9.4.2.

* Load data: readers.sav.
DATASET NAME Readers WINDOW=FRONT.

*  Exercise 1.
* Check data.
FREQUENCIES VARIABLES=age education polinterest newssite readingtime
  /ORDER=ANALYSIS.
* Multiple regression.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT readingtime
  /METHOD=ENTER education
  /METHOD=ENTER polinterest
  /METHOD=ENTER newssite
  /METHOD=ENTER age
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS HISTOGRAM(ZRESID).

*  Exercise 2.
* Check data.
FREQUENCIES VARIABLES=age education polinterest newssite readingtime
  /ORDER=ANALYSIS.
* (Pearson) Correlations.
CORRELATIONS
  /VARIABLES=age education polinterest newssite readingtime
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

  Exercise 3.
* Check data.
FREQUENCIES VARIABLES=age education polcynic newssite readingtime
  /ORDER=ANALYSIS.
* Multiple regression.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT readingtime
  /METHOD=ENTER education
  /METHOD=ENTER polcynic
  /METHOD=ENTER newssite
  /METHOD=ENTER age
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS HISTOGRAM(ZRESID).

* Section 9.8.2.

* Load data: readers.sav.
DATASET NAME Readers WINDOW=FRONT.

*  Exercise 1.
* Check data.
FREQUENCIES VARIABLES=age education polinterest newssite readingtime
  /ORDER=ANALYSIS.
* Multiple regression for newspaper reading time.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT readingtime
  /METHOD=ENTER age education polinterest newssite
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS HISTOGRAM(ZRESID).
* Multiple regression for news site use.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT newssite
  /METHOD=ENTER age education polinterest
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS HISTOGRAM(ZRESID).

* Exercise 2-4.
* Don't paste PROCESS output.

* Exercise 5.
* Load data: children.sav.
DATASET NAME Children WINDOW=FRONT.
* Check data.
FREQUENCIES VARIABLES=age supervision medliter
  /ORDER=ANALYSIS.
* Set imposible value (25) to missing.
* Define Variable Properties.
*supervision.
MISSING VALUES supervision(25.00).
EXECUTE.
* Indirect effect test with PROCESS.
* Don't paste PROCESS output.
* Regression models for checking assumptions.
* Outcome: media literacy.
REGRESSION
  /MISSING LISTWISE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT medliter
  /METHOD=ENTER age supervision
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS HISTOGRAM(ZRESID).
* Outcome: parental supervision.
REGRESSION
  /MISSING LISTWISE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT supervision
  /METHOD=ENTER age
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS HISTOGRAM(ZRESID).

