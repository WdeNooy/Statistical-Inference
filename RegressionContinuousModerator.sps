* Encoding: UTF-8.
* Dataset smokers.sav.
* Regression with continuous moderator.

*Dummies for status3.
*Ensure that categorical variables are recognized as such by SPSS.
* Define Variable Properties.
*status3.
VARIABLE LEVEL  status3(ORDINAL).
EXECUTE. 
SPSSINC CREATE DUMMIES VARIABLE=status3 
ROOTNAME1=status 
/OPTIONS ORDER=A USEVALUELABELS=YES USEML=YES OMITFIRST=NO.

* Mean center contact (moderator) and exposure (predictor).
* Find mean contact (and check range).
FREQUENCIES VARIABLES=contact exposure
  /FORMAT=NOTABLE
  /NTILES=3
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN
  /ORDER=ANALYSIS.
* Mean center.
COMPUTE contact_c=contact - 5.091.
COMPUTE exposure_c=exposure - 4.866.
VARIABLE LABELS  contact_c 'Contact (centered)' exposure_c 'Exposure (centered)'.
EXECUTE.

* Calculate interaction variable. 
COMPUTE expo_contact_c=exposure_c * contact_c.
VARIABLE LABELS  expo_contact_c 'Exposure * contact (centered)'.
EXECUTE.

* Regression with descriptives (for means and SDs).
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

* Draw regression lines manually in scatterplot.
* Group predictor scores in terciles.
* Visual Binning.
*contact.
RECODE  contact (MISSING=COPY) (LO THRU 4.25076386584132=1) (LO THRU 5.83711577142397=2) (LO THRU 
    HI=3) (ELSE=SYSMIS) INTO contact_bin.
VARIABLE LABELS  contact_bin 'Contact with smokers (Binned)'.
FORMATS  contact_bin (F5.0).
VALUE LABELS  contact_bin 1 'Low contact' 2 'Average contact' 3 'High contact'.
VARIABLE LEVEL  contact_bin (ORDINAL).
EXECUTE.
* Draw scatterplot with observations marked by contact group.
GRAPH
  /SCATTERPLOT(BIVAR)=exposure WITH attitude BY contact_bin
  /MISSING=LISTWISE.
* Add reference line using the estimated regression coefficients and x for the predictor.
* Either calculate the regression equations for minus 1 SD and plus 1 SD (for contact) or recenter contact at M - SD and M + SD and repeat regression analysis.

* Check common support with histogram per group.
GRAPH
  /HISTOGRAM=exposure
  /PANEL ROWVAR=contact_bin ROWOP=CROSS.


* Regression analyses for contact at M - SD.
* Mean center (don't forget parentheses!).
COMPUTE contact_l=contact - (49.54 - 16.331).
VARIABLE LABELS  contact_l 'contact low'.
EXECUTE.
* Calculate interaction variable. 
COMPUTE expo_contact_l=exposure * contact_l.
VARIABLE LABELS  expo_contact_l 'Interaction neg. emotions * contact (low)'.
EXECUTE.
* Regression with descriptives (for means and SDs).
REGRESSION
  /DESCRIPTIVES MEAN STDDEV
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT attitude
  /METHOD=ENTER exposure contact_l expo_contact_l.

* Regression analyses for contact at M + SD.
* Mean center.
COMPUTE contact_h=contact - (49.54 + 16.331).
VARIABLE LABELS  contact_l 'contact high'.
EXECUTE.
* Calculate interaction variable. 
COMPUTE expo_contact_h=exposure * contact_h.
VARIABLE LABELS  expo_contact_h 'Interaction neg. emotions * contact (high)'.
EXECUTE.
* Regression with descriptives (for means and SDs).
REGRESSION
  /DESCRIPTIVES MEAN STDDEV
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT attitude
  /METHOD=ENTER exposure contact_h expo_contact_h.

* Regression per subgroup with split file?.
* Dummies with interaction.
SPSSINC CREATE DUMMIES VARIABLE=status3 exposure 
ROOTNAME1=status, exposure ROOTNAME2=int 
/OPTIONS ORDER=A USEVALUELABELS=YES USEML=YES OMITFIRST=NO.
* Regression per moderator group.
SORT CASES  BY contact_bin.
SPLIT FILE LAYERED BY contact_bin.
* Regression.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT attitude
  /METHOD=ENTER exposure contact_c status_2 status_3 int_4_2 int_4_3.
SPLIT FILE OFF.

* Regression per subgroup with mean-centering of moderator within each moderator group.
* Note: grouping variable contact_bin previously created.
* Add median contact value per bin.
AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=contact_bin
  /contact_median=MEDIAN(contact).
* Median-center contact within each bin.
COMPUTE contact_bincenter=contact - contact_median.
VARIABLE LABELS  contact_bincenter 'Contact media-centered per bin'.
EXECUTE.
* Create interaction variable exposure*contact_bincenter.
COMPUTE expo_contact_median=exposure * contact_bincenter.
VARIABLE LABELS  expo_contact_median 'Interaction between exposure and contact (median-centered '+
    'per bin)'.
EXECUTE.
* Note: Dummies for status3 and exposure*status3 interaction have already been made.
* Regression per moderator group.
SORT CASES  BY contact_bin.
SPLIT FILE LAYERED BY contact_bin.
* Regression.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT attitude
  /METHOD=ENTER exposure contact_bincenter expo_contact_median status_2 status_3 int_4_2 int_4_3.
SPLIT FILE OFF.

