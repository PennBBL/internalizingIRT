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

first_proband_mod <- irt.fa(df[df$informant == "proband", grep("ITEM", colnames(df), value=TRUE)], nfactors=1, plot=FALSE)
#first_proband_mod <- irt.fa(df[df$informant == "proband", c("ITEM001", "ITEM002", "ITEM003", "ITEM004", "ITEM005")], nfactors=1, plot=FALSE)
df[df$informant == "proband", "FirstTraitEstimate"] <- scoreIrt(first_proband_mod, df[df$informant == "proband", c("ITEM001", "ITEM002", "ITEM003", "ITEM004", "ITEM005")])$theta1

summary_proband_df <- data.frame(matrix(NA, nrow=21, ncol=length(items)+1))
colnames(summary_proband_df) <- c("LessThan", items)
splits <- seq(0, 5, .25)
summary_proband_df$LessThan <- splits

prevspl <- -5
for (item in items) {
	prevspl <- -5
	for (spl in splits) {
		tmp_df <- df[df$informant == "proband" & df$FirstTraitEstimate < spl & df$FirstTraitEstimate > prevspl,]
		if (nrow(tmp_df) == 0) { summary_proband_df[summary_proband_df$LessThan == spl, item] <- 0 
		} else { summary_proband_df[summary_proband_df$LessThan == spl, item] <- round(nrow(tmp_df[tmp_df[,item] > 0,])/nrow(tmp_df), digits=3)*100
		}
		
		prevspl <- spl
	}
}

for (item in items) {
	tmp_plot <- ggplot(summary_proband_df, aes_string(x="LessThan", y=item)) + 
		geom_bar(stat="identity") + theme_minimal() + xlab("Trait Estimate Bin") +
		ylab("Yes (%)") + ggtitle(item) + scale_y_continuous(limits=c(0,100)) +
		theme(title = element_text(size=20))
	assign(paste0(item, "_proband_plot"), tmp_plot)
}


# ii. Check that proportion of "yes" responses monotonically increase with trait score estimated using 2PL IRT model for each item for collaterals.
first_collateral_mod <- irt.fa(df[df$informant == "collateral", grep("ITEM", colnames(df), value=TRUE)], nfactors=1, plot=FALSE)
#first_collateral_mod <- irt.fa(df[df$informant == "collateral", c("ITEM001", "ITEM002", "ITEM003", "ITEM004", "ITEM005")], nfactors=1, plot=FALSE)
df[df$informant == "collateral", "FirstTraitEstimate"] <- scoreIrt(first_collateral_mod, df[df$informant == "collateral", c("ITEM001", "ITEM002", "ITEM003", "ITEM004", "ITEM005")])$theta1

summary_collateral_df <- data.frame(matrix(NA, nrow=21, ncol=length(items)+1))
colnames(summary_collateral_df) <- c("LessThan", items)
splits <- seq(0, 5, .25)
summary_collateral_df$LessThan <- splits

prevspl <- -5
for (item in items) {
	prevspl <- -5
	for (spl in splits) {
		tmp_df <- df[df$informant == "collateral" & df$FirstTraitEstimate < spl & df$FirstTraitEstimate > prevspl,]
		if (nrow(tmp_df) == 0) { summary_collateral_df[summary_collateral_df$LessThan == spl, item] <- 0 
		} else { summary_collateral_df[summary_collateral_df$LessThan == spl, item] <- round(nrow(tmp_df[tmp_df[,item] > 0,])/nrow(tmp_df), digits=3)*100
		}
		
		prevspl <- spl
	}
}

for (item in items) {
	tmp_plot <- ggplot(summary_collateral_df, aes_string(x="LessThan", y=item)) + 
		geom_bar(stat="identity") + theme_minimal() + xlab("Trait Estimate Bin") +
		ylab("Yes (%)") + ggtitle(item) + scale_y_continuous(limits=c(0,100)) +
		theme(title = element_text(size=20))
	assign(paste0(item, "_collateral_plot"), tmp_plot)
}


pdf(file="/home/butellyn/parentchild_psychopathology/plots/monotonicEval.pdf", width=20, height=14)
grid.arrange(ITEM001_proband_plot, ITEM002_proband_plot, ITEM003_proband_plot, ITEM004_proband_plot, ITEM005_proband_plot, ITEM006_proband_plot, nrow=2, ncol=3)
grid.arrange(ITEM007_proband_plot, ITEM008_proband_plot, ITEM009_proband_plot, ITEM010_proband_plot, ITEM011_proband_plot, ITEM012_proband_plot, nrow=2, ncol=3)
grid.arrange(ITEM013_proband_plot, ITEM014_proband_plot, ITEM015_proband_plot, ITEM016_proband_plot, ITEM017_proband_plot, ITEM018_proband_plot, nrow=2, ncol=3)
grid.arrange(ITEM019_proband_plot, ITEM020_proband_plot, ITEM021_proband_plot, ITEM022_proband_plot, ITEM023_proband_plot, ITEM024_proband_plot, nrow=2, ncol=3)
grid.arrange(ITEM025_proband_plot, ITEM026_proband_plot, ITEM027_proband_plot, ITEM028_proband_plot, ITEM029_proband_plot, ITEM030_proband_plot, nrow=2, ncol=3)
grid.arrange(ITEM031_proband_plot, ITEM032_proband_plot, ITEM033_proband_plot, ITEM034_proband_plot, ITEM035_proband_plot, nrow=2, ncol=3)
grid.arrange(ITEM001_collateral_plot, ITEM002_collateral_plot, ITEM003_collateral_plot, ITEM004_collateral_plot, ITEM005_collateral_plot, ITEM006_collateral_plot, nrow=2, ncol=3)
grid.arrange(ITEM007_collateral_plot, ITEM008_collateral_plot, ITEM009_collateral_plot, ITEM010_collateral_plot, ITEM011_collateral_plot, ITEM012_collateral_plot, nrow=2, ncol=3)
grid.arrange(ITEM013_collateral_plot, ITEM014_collateral_plot, ITEM015_collateral_plot, ITEM016_collateral_plot, ITEM017_collateral_plot, ITEM018_collateral_plot, nrow=2, ncol=3)
grid.arrange(ITEM019_collateral_plot, ITEM020_collateral_plot, ITEM021_collateral_plot, ITEM022_collateral_plot, ITEM023_collateral_plot, ITEM024_collateral_plot, nrow=2, ncol=3)
grid.arrange(ITEM025_collateral_plot, ITEM026_collateral_plot, ITEM027_collateral_plot, ITEM028_collateral_plot, ITEM029_collateral_plot, ITEM030_collateral_plot, nrow=2, ncol=3)
grid.arrange(ITEM031_collateral_plot, ITEM032_collateral_plot, ITEM033_collateral_plot, ITEM034_collateral_plot, ITEM035_collateral_plot, nrow=2, ncol=3)
dev.off()

### b) Dimensionality
# i. Fit bifactor and unidimensional factor models to probands (create table). Then, fit a 2PL IRT model and check that the residuals in the items are not correlated (local independence).
bifactor_proband <- fa(df[df$informant == "proband", grep("ITEM", colnames(df), value=TRUE)], nfactors=1, n.obs = NA, n.iter=1, rotate="bifactor", scores="regression", residuals=FALSE, SMC=TRUE, covar=FALSE,missing=FALSE, impute="median", min.err = 0.001, max.iter = 50, symmetric=TRUE, warnings=TRUE, fm="minres", alpha=.1, p=.05, oblique.scores=FALSE, np.obs=NULL, use="pairwise", cor="cor", correct=.5, weight=NULL,...)

# ii. Fit bifactor and unidimensional factor models to collaterals (create table). Then, fit a 2PL IRT model and check that the residuals in the items are not correlated (local independence).



##### Hypothesis #2 (DIF): Given a trait level, male probands and their collaterals will endorse “grouchy/irritable” more frequently and female probands and their collaterals will endorse “crying” more frequently (), but that these differences will be minimal enough to justify putting males and females on the same scale. 

##### Hypothesis #3 (DIF): Discrimination parameters will be able to be held constant within items and across genders for probands (), but only within items for collaterals. Discrimination parameters based on collateral reports will vary systematically by the gender of the proband such that discrimination parameters will be higher for collaterals of female than male adolescents. Collaterals will require smaller discrimination parameters than probands due to the guess-work involved in reporting on the proband’s internalizing symptoms.


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




##### Hypothesis #4: There will be convergent and discriminant evidence for construct validity, such that higher levels of lifetime internalizing psychopathology severity will be associated will greater probability of endorsing suicidal ideation and a history of physical or sexual assault, and there will be no association between lifetime internalizing psychopathology severity and finger tapping speed within sex.


##### Hypothesis #5: Females’ lifetime internalizing psychopathology will be worse, as reported by the proband and the collateral, than males’ ().
# i. Test the following hypothesis: Females will suffer from greater lifetime internalizing psychopathology than males, as reported by probands (T-test; too skewed for CLT?)
	# (Check distributions: Normal? Outliers? Homogeneous? Equal variances (if not, Welch)?)


# ii. Test the following hypothesis: Females will suffer from greater lifetime internalizing psychopathology than males, as reported by collaterals (T-test; too skewed for CLT?)
	# (Check distributions: Normal? Outliers? Homogeneous? Equal variances (if not, Welch)?)


##### Hypothesis #6:  Collaterals’ assessments of probands’ symptoms are insufficient for estimating probands’ internalizing psychopathology. In particular, collaterals of male probands will disagree more frequently with male probands about their symptoms, with a tendency to underestimate their presence. 
# i. Using the location parameters from the proband model, calculate traits scores for the probands based on the collaterals' reports


# ii. Test the following hypothesis: The difference between proband and collateral (p minus c) trait estimates is significantly greater than 0 (T-test; too skewed for CLT?)
	# (Check distributions: Normal? Outliers? Homogeneous? Equal variances (if not, Welch)?)
















