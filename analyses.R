### This script runs all of the analyses for Ellyn's "Lifetime Depressive and Anxious
### Symptomatology: Differential Item Functioning by Gender and Informant Type in the
### Philadelphia Neurodevelopmental Cohort"
###
### Ellyn Butler
### July 22, 2019 - present

# Load packages
library('ggplot2')
library('gridExtra')
library('mirt')
library('lordif')
library('psych')

# Load data
df <- read.csv("/home/butellyn/parentchild_psychopathology/data/cleanitems.csv")

# Limit the sample to those who are part of complete proband/collateral pairs
df$Remove <- 0
for (i in 1:nrow(df)) {
	bblid <- df[i, "bblid"]
	if (length(df[df$bblid == bblid, "bblid"]) != 2) { df[i, "Remove"] <- 1 }
}
df <- df[df$Remove != 1,]
rownames(df) <- 1:nrow(df)

# Change bblid and informant to factors
df$bblid <- factor(df$bblid)
df$informant <- factor(df$informant)

# Change ITEM columns to numeric
items <- grep("ITEM", colnames(df), value=TRUE)
for (item in items) { df[,item] <- as.numeric(df[,item]) }

df$Remove <- NULL


######################################################################################

##### Hypothesis #1: There will be one primary dimension that explains a large portion of the variance in depressive and anxious symptoms in both the collaterals’ and the probands’ assessments of the probands’ symptoms, but there will also be noticeable group factors in a bifactor model. We expect this because depressive and anxiety disorders are highly comorbid, but do not always co-occur.  Further, previous IRT literature has found meaningful factors for general negative affectivity, anxiety and depression in a pediatric sample (Mineka, 1998; Irwin, 2010).

### a) Monotonicity
# i. Check that proportion of "yes" responses monotonically increase with trait score estimated using 2PL IRT model for each item for probands.


# ii. Check that proportion of "yes" responses monotonically increase with trait score estimated using 2PL IRT model for each item for collaterals.


### b) Dimensionality
# i. Fit bifactor and unidimensional factor models to probands (create table). Then, fit a 2PL IRT model and check that the residuals in the items are not correlated (local independence).
bifactor_proband <- fa(df[df$informant == "proband", grep("ITEM", colnames(df), value=TRUE)], nfactors=1, n.obs = NA, n.iter=1, rotate="bifactor", scores="regression", residuals=FALSE, SMC=TRUE, covar=FALSE,missing=FALSE, impute="median", min.err = 0.001, max.iter = 50, symmetric=TRUE, warnings=TRUE, fm="minres", alpha=.1, p=.05, oblique.scores=FALSE, np.obs=NULL, use="pairwise", cor="cor", correct=.5, weight=NULL,...)

# ii. Fit bifactor and unidimensional factor models to collaterals (create table). Then, fit a 2PL IRT model and check that the residuals in the items are not correlated (local independence).


##### Hypothesis #2 (DIF): Discrimination parameters will be able to be held constant within items across genders (), but not across informants. Collaterals will require smaller discrimination parameters, due to the guess-work involved in reporting on the proband’s internalizing symptoms. In addition, discrimination parameters based on collateral reports will vary systematically by the gender of the proband such that discrimination parameters will be higher for collaterals of female than male adolescents. 

##### Hypothesis #3 (DIF): Given a trait level, male probands and their collaterals will endorse “grouchy/irritable” more frequently and female probands and their collaterals will endorse “crying” more frequently(), but that these differences will be minimal enough to justify putting males and females on the same scale. 

### c) Identify items with linear and non-linear DIF using lordif package
# i. Identify items with DIF in probands. 
# Test for linear and non-linear DIF by computing Chi-Squared statistics for the nested models, matching on sequentially purified IRT-based estimates of the trait score. Flag subjects using Beta and alpha < .01
# Model 1: log(P(ui >= k)) = ak + B1*ability
# Model 2: log(P(ui >= k)) = ak + B1*ability + B2*group
# Model 3: log(P(ui >= k)) = ak + B1*ability + B2*group + B3*ability*group


# ii. Identify items with DIF in collaterals.
# Test for linear and non-linear DIF by computing Chi-Squared statistics for the nested models, matching on sequentially purified IRT-based estimates of the trait score. Flag subjects using Beta and alpha < .01
# Model 1: log(P(ui >= k)) = ak + B1*ability
# Model 2: log(P(ui >= k)) = ak + B1*ability + B2*group
# Model 3: log(P(ui >= k)) = ak + B1*ability + B2*group + B3*ability*group


# iii. Compare discrimination parameters across informants (How? T-tests with FDR correction?)




##### Hypothesis #4: There will be evidence for construct validity with convergent and discriminant validity, such that higher levels of lifetime internalizing psychopathology severity will be associated will greater probability of endorsing suicidal ideation and a history of physical or sexual assault, and there will be no association between lifetime internalizing psychopathology severity and finger tapping speed within sex.


##### Hypothesis #5: Females’ lifetime internalizing psychopathology will be worse, as reported by the proband and the collateral, than males’ ().
# i. Test the following hypothesis: Females will suffer from greater lifetime internalizing psychopathology than males, as reported by probands (T-test; too skewed for CLT?)
	# (Check distributions: Normal? Outliers? Homogeneous? Equal variances (if not, Welch)?)


# ii. Test the following hypothesis: Females will suffer from greater lifetime internalizing psychopathology than males, as reported by collaterals (T-test; too skewed for CLT?)
	# (Check distributions: Normal? Outliers? Homogeneous? Equal variances (if not, Welch)?)


##### Hypothesis #6:  Collaterals’ assessments of probands’ symptoms are insufficient for estimating probands’ internalizing psychopathology. In particular, collaterals of male probands will disagree more frequently with male probands about their symptoms, with a tendency to underestimate their presence. 
# i. Using the location parameters from the proband model, calculate traits scores for the probands based on the collaterals' reports


# ii. Test the following hypothesis: The difference between proband and collateral (p minus c) trait estimates is significantly greater than 0 (T-test; too skewed for CLT?)
	# (Check distributions: Normal? Outliers? Homogeneous? Equal variances (if not, Welch)?)
















