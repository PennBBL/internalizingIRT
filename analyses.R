### This script runs all of the analyses for Ellyn's "Lifetime Depressive and Anxious
### Symptomatology: Psychometric Discrepancies by Gender and Informant Type in the
### Philadelphia Neurodevelopmental Cohort"
###
### Ellyn Butler
### July 22, 2019 - present

# Load packages
library('ggplot2')
library('gridExtra')
library('mirt')
library('psych')
library('car') 
library('lmPerm')
library('msm')
library('DAAG')

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

# Recode items with more than 8 response options
df$ITEM011 <- recode(df$ITEM011, "0=0;1=1;2=1;3=1;4=2;5=2;6=3;7=3;8=4;9=4;10=5")
df$ITEM012 <- recode(df$ITEM012, "0=0;1=1;2=1;3=1;4=2;5=2;6=3;7=3;8=4;9=4;10=5")

######################################################################################

proband_df <- df[df$informant == "proband",]
rownames(proband_df) <- 1:nrow(proband_df)

collateral_df <- df[df$informant == "collateral",]
rownames(collateral_df) <- 1:nrow(collateral_df)


##### Hypothesis #1: There will be one primary dimension that explains a large portion of the variance in depressive and anxious symptoms in both the collaterals’ and the probands’ assessments of the probands’ symptoms, but there will also be noticeable group factors in a bifactor model. We expect this because depressive and anxiety disorders are highly comorbid, but do not always co-occur.  Further, previous IRT literature has found meaningful factors for general negative affectivity, anxiety and depression in a pediatric sample (Mineka, 1998; Irwin, 2010).

### a) Monotonicity
# i. Check that proportion of "yes" responses monotonically increase with trait score estimated using 2PL IRT model for each item for probands.

first_proband_mod <- irt.fa(proband_df[, items], nfactors=1, plot=FALSE)
proband_df$FirstTraitEstimate <- scoreIrt(first_proband_mod, proband_df[, items])$theta1

summary_proband_df <- data.frame(matrix(NA, nrow=17, ncol=length(items)+1))
colnames(summary_proband_df) <- c("LessThan", items)
splits <- seq(0, 4, .25)
summary_proband_df$LessThan <- splits

prevspl <- -5
for (item in items) {
	prevspl <- -5
	for (spl in splits) {
		tmp_df <- proband_df[proband_df$FirstTraitEstimate < spl & proband_df$FirstTraitEstimate > prevspl,]
		if (nrow(tmp_df) == 0) { summary_proband_df[summary_proband_df$LessThan == spl, item] <- 0 
		} else { summary_proband_df[summary_proband_df$LessThan == spl, item] <- round(nrow(tmp_df[tmp_df[,item] > 0,])/nrow(tmp_df), digits=3)*100
		}
		
		prevspl <- spl
	}
}

for (item in items) {
	tmp_plot <- ggplot(summary_proband_df, aes_string(x="LessThan", y=item)) + 
		geom_bar(stat="identity") + theme_minimal() + xlab("Trait Estimate Bin") +
		ylab("Yes (%)") + ggtitle(paste0("Probands: ", item)) + scale_y_continuous(limits=c(0,100)) +
		theme(title = element_text(size=20))
	assign(paste0(item, "_proband_plot"), tmp_plot)
}


# ii. Check that proportion of "yes" responses monotonically increase with trait score estimated using 2PL IRT model for each item for collaterals.
first_collateral_mod <- irt.fa(collateral_df[, grep("ITEM", colnames(df), value=TRUE)], nfactors=1, plot=FALSE)
collateral_df$FirstTraitEstimate <- scoreIrt(first_collateral_mod, collateral_df[, items])$theta1

summary_collateral_df <- data.frame(matrix(NA, nrow=17, ncol=length(items)+1))
colnames(summary_collateral_df) <- c("LessThan", items)
splits <- seq(0, 4, .25)
summary_collateral_df$LessThan <- splits

prevspl <- -5
for (item in items) {
	prevspl <- -5
	for (spl in splits) {
		tmp_df <- collateral_df[collateral_df$FirstTraitEstimate < spl & collateral_df$FirstTraitEstimate > prevspl,]
		if (nrow(tmp_df) == 0) { summary_collateral_df[summary_collateral_df$LessThan == spl, item] <- 0 
		} else { summary_collateral_df[summary_collateral_df$LessThan == spl, item] <- round(nrow(tmp_df[tmp_df[,item] > 0,])/nrow(tmp_df), digits=3)*100
		}
		
		prevspl <- spl
	}
}

for (item in items) {
	tmp_plot <- ggplot(summary_collateral_df, aes_string(x="LessThan", y=item)) + 
		geom_bar(stat="identity") + theme_minimal() + xlab("Trait Estimate Bin") +
		ylab("Yes (%)") + ggtitle(paste0("Collaterals: ", item)) + scale_y_continuous(limits=c(0,100)) +
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
# i. Examine scree. Fit bifactor and unidimensional factor models to probands (create table). Then, fit a 2PL IRT model and check that the residuals in the items are not correlated (local independence).
pdf(NULL)
dev.control(displaylist="enable")
VSS.scree(proband_df[,items], main="Proband Scree Plot")
proband_scree <- recordPlot()
invisible(dev.off())

# -> RESULT: Scree plot indicated that a unidimensional model was not sufficient, so the other criteria were not evaluated.


# ii. Examine scree. Fit bifactor and unidimensional factor models to collaterals (create table). Then, fit a 2PL IRT model and check that the residuals in the items are not correlated (local independence).
pdf(NULL)
dev.control(displaylist="enable")
VSS.scree(collateral_df[,items], main="Collateral Scree Plot")
collateral_scree <- recordPlot()
invisible(dev.off())

# -> RESULT: Scree plot indicated that a unidimensional model was not sufficient, so the other criteria were not evaluated.


pdf(file="/home/butellyn/parentchild_psychopathology/plots/scree.pdf", width=6, height=6)
proband_scree
collateral_scree
dev.off()

##### Hypothesis #2 (DIF): Given a trait level, female probands and their collaterals will endorse “crying” more frequently than male probands and their collaterals (Gelin, 2003; Carleton, 2013; van Beek, 2012).

##### Hypothesis #3 (DIF): DIF effects will be larger in probands than collaterals due to the guess-work involved in reporting on the proband’s internalizing symptoms and the subsequent increased error variance. 


### c) Identify items with linear and non-linear DIF using mirt package
# i. _________________ Check for all DIF in probands. _________________ 
loadings <- c(rep(1, 13), rep(2, 15), rep(3, 7)) # Correspond to screener DEP, other DEP, GAD and SOC items ####

# DRF https://groups.google.com/forum/#!topic/mirt-package/WVc6nM-76LM
# https://groups.google.com/forum/#!searchin/mirt-package/bifactor$20correlated%7Csort:date/mirt-package/tRP13GRlQhI/mUi3H_rIBgAJ
proband_results <- data.frame(matrix(NA, nrow=35, ncol=6)) 
colnames(proband_results) <- c("Item", "Main", "Interaction", "DiffAIC", "ChiSq", "ChiSqP") 
#fun <- function(Theta, ...) msm::dtnorm(Theta, mean = 0, sd = 1, lower = -2, upper = 5)
#scores <- fscores(tmp_proband_mod, custom_den = fun, method = 'MAP', full.scores = FALSE)
i=1
for (item in items) {
	# Determine index of item for testing
	item_ind <- which(items %in% item)
	print(item_ind)

	# Fit bifactor without this item
	tmp_proband_mod <- bfactor(proband_df[,items[!(items %in% item)]], loadings[-item_ind], technical=list(NCYCLES=2000))
	write.csv(summary(tmp_proband_mod)[[1]], file=paste0("/home/butellyn/parentchild_psychopathology/data/loadings/proband_bi_", item, "_ex.csv"), row.names=FALSE) ####	

	assign(paste0(item, "_proband_bifactor"), tmp_proband_mod)
	# Score probands using this model
	scores <- fscores(tmp_proband_mod, QMC=TRUE) ### Q: defaults okay?
	proband_df[,paste0("general_", item, "_ex")] <- scores[,1] 
	proband_df[,paste0("dep_", item, "_ex")] <- scores[,2] ####
	proband_df[,paste0("gad_", item, "_ex")] <- scores[,3] ####
	proband_df[,paste0("soc_", item, "_ex")] <- scores[,4] ####

	# Dichotomize item, if necessary 
	if (length(unique(proband_df[,item])) > 2) {
		proband_df[proband_df[,item] == 0, paste0(item, "_dich")] <- 0
		proband_df[proband_df[,item] != 0, paste0(item, "_dich")] <- 1
		# Test for DIF in item
		tmp_log_mod <- glm(proband_df[,paste0(item, "_dich")] ~ proband_df[,paste0("general_", item, "_ex")] + proband_df$sex + proband_df[,paste0("general_", item, "_ex")]*proband_df$sex, family="binomial")
		assign(paste0(item, "_mod"), tmp_log_mod)

		tmp_simp_log_mod <- glm(proband_df[,paste0(item, "_dich")] ~ proband_df[,paste0("general_", item, "_ex")], family="binomial")
		assign(paste0(item, "_simp_mod"), tmp_simp_log_mod)
	} else {
		# Test for DIF in item
		tmp_log_mod <- glm(proband_df[,item] ~ proband_df[,paste0("general_", item, "_ex")] + proband_df$sex + proband_df[,paste0("general_", item, "_ex")]*proband_df$sex, family="binomial")
		assign(paste0(item, "_mod"), tmp_log_mod)

		tmp_simp_log_mod <- glm(proband_df[,item] ~ proband_df[,paste0("general_", item, "_ex")], family="binomial")
		assign(paste0(item, "_simp_mod"), tmp_simp_log_mod)
	}

	# Run a ChiSq Test 
	chisq <- anova(tmp_simp_log_mod, tmp_log_mod, test="Chisq")

	# Put relevant statistics in results dataframe
	proband_results[i, "Item"] <- item
	proband_results[i, "Main"] <- summary(tmp_log_mod)$coefficients[3,4]
	proband_results[i, "Interaction"] <- summary(tmp_log_mod)$coefficients[4,4]
	proband_results[i, "DiffAIC"] <- summary(tmp_log_mod)$aic - summary(tmp_simp_log_mod)$aic
	proband_results[i, "ChiSq"] <- chisq$Deviance[2] 
	proband_results[i, "ChiSqP"] <- chisq[[5]][2] 

	i=i+1
}

# Write out logistic model results and bifactor loadings
write.csv(proband_results, file=paste0("/home/butellyn/parentchild_psychopathology/data/proband_dif_", Sys.Date(), ".csv"), row.names=FALSE)


# Split ITEM002
proband_df$ITEM002_F <- proband_df$ITEM002
proband_df$ITEM002_M <- proband_df$ITEM002
proband_df[proband_df$sex == "Male", "ITEM002_F"] <- NA
proband_df[proband_df$sex == "Female", "ITEM002_M"] <- NA

items2 <- c("ITEM001", "ITEM002_F", "ITEM002_M", "ITEM003", "ITEM004", "ITEM005", "ITEM006", "ITEM007", "ITEM008", "ITEM009", "ITEM010", "ITEM011", "ITEM012", "ITEM013", "ITEM014", "ITEM015", "ITEM016", "ITEM017", "ITEM018", "ITEM019", "ITEM020", "ITEM021", "ITEM022", "ITEM023", "ITEM024", "ITEM025", "ITEM026", "ITEM027", "ITEM028", "ITEM029", "ITEM030", "ITEM031", "ITEM032", "ITEM033", "ITEM034", "ITEM035")

loadings <- c(rep(1, 14), rep(2, 15), rep(3, 7)) ####
proband_results2 <- data.frame(matrix(NA, nrow=34, ncol=6))
colnames(proband_results2) <- c("Item", "Main", "Interaction", "DiffAIC", "ChiSq", "ChiSqP")
i=1
for (item in items2) {
	if (item != "ITEM002_F" & item != "ITEM002_M") {
		# Determine index of item for testing
		item_ind <- which(items2 %in% item)

		# Fit bifactor without this item
		tmp_proband_mod <- bfactor(proband_df[,items2[!(items2 %in% item)]], loadings[-item_ind], technical=list(NCYCLES=2000))
		write.csv(summary(tmp_proband_mod)[[1]], file=paste0("/home/butellyn/parentchild_psychopathology/data/loadings/proband_bi_", item, "_ex2.csv"), row.names=FALSE) 

		assign(paste0(item, "_proband_bifactor2"), tmp_proband_mod)
		# Score probands using this model
		scores <- fscores(tmp_proband_mod, QMC=TRUE) ### Q: defaults okay?
		proband_df[,paste0("general_", item, "_ex2")] <- scores[,1] 
		proband_df[,paste0("dep_", item, "_ex2")] <- scores[,2] ####
		proband_df[,paste0("gad_", item, "_ex2")] <- scores[,3] ####
		proband_df[,paste0("soc_", item, "_ex2")] <- scores[,4] ####

		# Dichotomize item, if necessary 
		if (length(unique(proband_df[,item])) > 2) {
			# Test for DIF in item
			tmp_log_mod <- glm(proband_df[,paste0(item, "_dich")] ~ proband_df[,paste0("general_", item, "_ex2")] + proband_df$sex + proband_df[,paste0("general_", item, "_ex2")]*proband_df$sex, family="binomial")
			assign(paste0(item, "_mod2"), tmp_log_mod)

			tmp_simp_log_mod <- glm(proband_df[,paste0(item, "_dich")] ~ proband_df[,paste0("general_", item, "_ex2")], family="binomial")
			assign(paste0(item, "_simp_mod2"), tmp_simp_log_mod)
		} else {
			# Test for DIF in item
			tmp_log_mod <- glm(proband_df[,item] ~ proband_df[,paste0("general_", item, "_ex2")] + proband_df$sex + proband_df[,paste0("general_", item, "_ex2")]*proband_df$sex, family="binomial")
			assign(paste0(item, "_mod2"), tmp_log_mod)

			tmp_simp_log_mod <- glm(proband_df[,item] ~ proband_df[,paste0("general_", item, "_ex2")], family="binomial")
			assign(paste0(item, "_simp_mod2"), tmp_simp_log_mod)
		}

		# Run a ChiSq Test 
		chisq <- anova(tmp_simp_log_mod, tmp_log_mod, test="Chisq")

		# Put relevant statistics in results dataframe
		proband_results2[i, "Item"] <- item
		proband_results2[i, "Main"] <- summary(tmp_log_mod)$coefficients[3,4]
		proband_results2[i, "Interaction"] <- summary(tmp_log_mod)$coefficients[4,4]
		proband_results2[i, "DiffAIC"] <- summary(tmp_log_mod)$aic - summary(tmp_simp_log_mod)$aic
		proband_results2[i, "ChiSq"] <- chisq$Deviance[2] ####
		proband_results2[i, "ChiSqP"] <- chisq[[5]][2] ####

		i=i+1
	}
}


write.csv(proband_results2, file=paste0("/home/butellyn/parentchild_psychopathology/data/proband_dif2_", Sys.Date(), ".csv"), row.names=FALSE)


# If there are no DiffAIC outliers, then re-calcualte the bifactor model and scores (internalizing, depression, generalized, social)
final_proband_mod <- bfactor(proband_df[,items2], loadings, technical=list(NCYCLES=2000)) #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
write.csv(summary(final_proband_mod)[[1]], file="/home/butellyn/parentchild_psychopathology/data/loadings/proband_final_bi.csv", row.names=FALSE)

s <- "F1 = ITEM001, ITEM002_F, ITEM002_M, ITEM003, ITEM004, ITEM005, ITEM006, ITEM007, ITEM008, ITEM009, ITEM010, ITEM011, ITEM012, ITEM013, ITEM014, ITEM015, ITEM016, ITEM017, ITEM018, ITEM019, ITEM020, ITEM021, ITEM022, ITEM023, ITEM024, ITEM025, ITEM026, ITEM027, ITEM028, ITEM029, ITEM030, ITEM031, ITEM032, ITEM033, ITEM034, ITEM035"
one_proband_mod <- mirt(proband_df[,items2], s, method="MHRM", technical=list(NCYCLES=3000)) #MHRM brings it down... tried SANN (doesn't work with stochastic method), NR doesn't converge within 3000 iterations, still very high with NR1, L-BFGS-B brings it down SUBSTANTIALLY


write.csv(summary(one_proband_mod)$rotF, file="/home/butellyn/parentchild_psychopathology/data/loadings/proband_one.csv", row.names=FALSE)

# Score probands using this model
scores <- fscores(final_proband_mod, QMC=TRUE) ### Q: defaults okay?
proband_df$internal_bifactor <- scores[,1] 
proband_df$depress_bifactor <- scores[,2] 
proband_df$genanx_bifactor <- scores[,3] 
proband_df$socanx_bifactor <- scores[,4] 


# Write out the parameters for each model, the results table(s), and proband_df with the final general and specific trait estimates
write.csv(proband_df, file=paste0("/home/butellyn/parentchild_psychopathology/data/proband_", Sys.Date(), ".csv"), row.names=FALSE)

# ii. _________________ Check for all DIF in collaterals. _________________ 
loadings <- c(rep(1, 13), rep(2, 15), rep(3, 7)) 
collateral_results <- data.frame(matrix(NA, nrow=35, ncol=6))
colnames(collateral_results) <- c("Item", "Main", "Interaction", "DiffAIC", "ChiSq", "ChiSqP")
i=1
for (item in items) {
	# Determine index of item for testing
	item_ind <- which(items %in% item)

	# Fit bifactor without this item
	tmp_collateral_mod <- bfactor(collateral_df[,items[!(items %in% item)]], loadings[-item_ind], technical=list(NCYCLES=2000))
	write.csv(summary(tmp_collateral_mod)[[1]], file=paste0("/home/butellyn/parentchild_psychopathology/data/loadings/collateral_bi_", item, "_ex.csv"), row.names=FALSE)	

	assign(paste0(item, "_collateral_bifactor"), tmp_collateral_mod)
	# Score collaterals using this model
	scores <- fscores(tmp_collateral_mod, QMC=TRUE) ### Q: defaults okay?
	collateral_df[,paste0("general_", item, "_ex")] <- scores[,1] 
	collateral_df[,paste0("dep_", item, "_ex")] <- scores[,2]
	collateral_df[,paste0("gad_", item, "_ex")] <- scores[,3] 
	collateral_df[,paste0("soc_", item, "_ex")] <- scores[,4]

	# Dichotomize item, if necessary 
	if (length(unique(collateral_df[,item])) > 2) {
		collateral_df[collateral_df[,item] == 0, paste0(item, "_dich")] <- 0
		collateral_df[collateral_df[,item] != 0, paste0(item, "_dich")] <- 1
		# Test for DIF in item
		tmp_log_mod <- glm(collateral_df[,paste0(item, "_dich")] ~ collateral_df[,paste0("general_", item, "_ex")] + collateral_df$sex + collateral_df[,paste0("general_", item, "_ex")]*collateral_df$sex, family="binomial")
		assign(paste0(item, "_mod"), tmp_log_mod)

		tmp_simp_log_mod <- glm(collateral_df[,paste0(item, "_dich")] ~ collateral_df[,paste0("general_", item, "_ex")], family="binomial")
		assign(paste0(item, "_simp_mod"), tmp_simp_log_mod)
	} else {
		# Test for DIF in item
		tmp_log_mod <- glm(collateral_df[,item] ~ collateral_df[,paste0("general_", item, "_ex")] + collateral_df$sex + collateral_df[,paste0("general_", item, "_ex")]*collateral_df$sex, family="binomial")
		assign(paste0(item, "_mod"), tmp_log_mod)

		tmp_simp_log_mod <- glm(collateral_df[,item] ~ collateral_df[,paste0("general_", item, "_ex")], family="binomial")
		assign(paste0(item, "_simp_mod"), tmp_simp_log_mod)
	}

	# Run a ChiSq Test 
	chisq <- anova(tmp_simp_log_mod, tmp_log_mod, test="Chisq")

	# Put relevant statistics in results dataframe
	collateral_results[i, "Item"] <- item
	collateral_results[i, "Main"] <- summary(tmp_log_mod)$coefficients[3,4]
	collateral_results[i, "Interaction"] <- summary(tmp_log_mod)$coefficients[4,4]
	collateral_results[i, "DiffAIC"] <- summary(tmp_log_mod)$aic - summary(tmp_simp_log_mod)$aic
	collateral_results[i, "ChiSq"] <- chisq$Deviance[2] 
	collateral_results[i, "ChiSqP"] <- chisq[[5]][2] 

	i=i+1
}

write.csv(collateral_results, file=paste0("/home/butellyn/parentchild_psychopathology/data/collateral_dif_", Sys.Date(), ".csv"), row.names=FALSE)

# Split DIF items by sex, and then recalculate DiffAIC for the remaining items

# Split ITEM002
collateral_df$ITEM002_F <- collateral_df$ITEM002
collateral_df$ITEM002_M <- collateral_df$ITEM002
collateral_df[collateral_df$sex == "Male", "ITEM002_F"] <- NA
collateral_df[collateral_df$sex == "Female", "ITEM002_M"] <- NA

items2 <- c("ITEM001", "ITEM002_F", "ITEM002_M", "ITEM003", "ITEM004", "ITEM005", "ITEM006", "ITEM007", "ITEM008", "ITEM009", "ITEM010", "ITEM011", "ITEM012", "ITEM013", "ITEM014", "ITEM015", "ITEM016", "ITEM017", "ITEM018", "ITEM019", "ITEM020", "ITEM021", "ITEM022", "ITEM023", "ITEM024", "ITEM025", "ITEM026", "ITEM027", "ITEM028", "ITEM029", "ITEM030", "ITEM031", "ITEM032", "ITEM033", "ITEM034", "ITEM035")

loadings <- c(rep(1, 14), rep(2, 15), rep(3, 7))
collateral_results2 <- data.frame(matrix(NA, nrow=34, ncol=6))
colnames(collateral_results2) <- c("Item", "Main", "Interaction", "DiffAIC", "ChiSq", "ChiSqP")
i=1
for (item in items2) {
	if (item != "ITEM002_F" & item != "ITEM002_M") {
		# Determine index of item for testing
		item_ind <- which(items2 %in% item)

		# Fit bifactor without this item
		tmp_collateral_mod <- bfactor(collateral_df[,items2[!(items2 %in% item)]], loadings[-item_ind], technical=list(NCYCLES=2000))
		write.csv(summary(tmp_collateral_mod)[[1]], file=paste0("/home/butellyn/parentchild_psychopathology/data/loadings/collateral_bi_", item, "_ex2.csv"), row.names=FALSE)	

		assign(paste0(item, "_collateral_bifactor2"), tmp_collateral_mod)
		# Score collaterals using this model
		scores <- fscores(tmp_collateral_mod, QMC=TRUE) ### Q: defaults okay?
		collateral_df[,paste0("general_", item, "_ex2")] <- scores[,1] 
		collateral_df[,paste0("dep_", item, "_ex2")] <- scores[,2] 
		collateral_df[,paste0("gad_", item, "_ex2")] <- scores[,3] 
		collateral_df[,paste0("soc_", item, "_ex2")] <- scores[,4]

		# Dichotomize item, if necessary 
		if (length(unique(collateral_df[,item])) > 2) {
			# Test for DIF in item
			tmp_log_mod <- glm(collateral_df[,paste0(item, "_dich")] ~ collateral_df[,paste0("general_", item, "_ex2")] + collateral_df$sex + collateral_df[,paste0("general_", item, "_ex2")]*collateral_df$sex, family="binomial")
			assign(paste0(item, "_mod2"), tmp_log_mod)

			tmp_simp_log_mod <- glm(collateral_df[,paste0(item, "_dich")] ~ collateral_df[,paste0("general_", item, "_ex2")], family="binomial")
			assign(paste0(item, "_simp_mod2"), tmp_simp_log_mod)
		} else {
			# Test for DIF in item
			tmp_log_mod <- glm(collateral_df[,item] ~ collateral_df[,paste0("general_", item, "_ex2")] + collateral_df$sex + collateral_df[,paste0("general_", item, "_ex2")]*collateral_df$sex, family="binomial")
			assign(paste0(item, "_mod2"), tmp_log_mod)

			tmp_simp_log_mod <- glm(collateral_df[,item] ~ collateral_df[,paste0("general_", item, "_ex2")], family="binomial")
			assign(paste0(item, "_simp_mod2"), tmp_simp_log_mod)
		}

		# Run a ChiSq Test 
		chisq <- anova(tmp_simp_log_mod, tmp_log_mod, test="Chisq")

		# Put relevant statistics in results dataframe
		collateral_results2[i, "Item"] <- item
		collateral_results2[i, "Main"] <- summary(tmp_log_mod)$coefficients[3,4]
		collateral_results2[i, "Interaction"] <- summary(tmp_log_mod)$coefficients[4,4]
		collateral_results2[i, "DiffAIC"] <- summary(tmp_log_mod)$aic - summary(tmp_simp_log_mod)$aic
		collateral_results2[i, "ChiSq"] <- chisq$Deviance[2] 
		collateral_results2[i, "ChiSqP"] <- chisq[[5]][2] 

		i=i+1
	}
}

write.csv(collateral_results2, file=paste0("/home/butellyn/parentchild_psychopathology/data/collateral_dif2_", Sys.Date(), ".csv"), row.names=FALSE)


# If there are no DiffAIC outliers, then re-calcualte the bifactor model and scores (internalizing, depression, generalized, social)
final_collateral_mod <- bfactor(collateral_df[,items2], loadings, technical=list(NCYCLES=2000)) 
write.csv(summary(final_collateral_mod)[[1]], file="/home/butellyn/parentchild_psychopathology/data/loadings/collateral_final_bi.csv", row.names=FALSE)

s <- "F1 = ITEM001, ITEM002_F, ITEM002_M, ITEM003, ITEM004, ITEM005, ITEM006, ITEM007, ITEM008, ITEM009, ITEM010, ITEM011, ITEM012, ITEM013, ITEM014, ITEM015, ITEM016, ITEM017, ITEM018, ITEM019, ITEM020, ITEM021, ITEM022, ITEM023, ITEM024, ITEM025, ITEM026, ITEM027, ITEM028, ITEM029, ITEM030, ITEM031, ITEM032, ITEM033, ITEM034, ITEM035"
one_collateral_mod <- mirt(collateral_df[,items2], s, method="MHRM", technical=list(NCYCLES=3000)) #MHRM brings it down; EM with more quadpts?
write.csv(summary(one_collateral_mod)$rotF, file="/home/butellyn/parentchild_psychopathology/data/loadings/collateral_one.csv", row.names=FALSE)

# Score collaterals using this model
scores <- fscores(final_collateral_mod, QMC=TRUE) 
collateral_df$internal_bifactor_coll <- scores[,1] 
collateral_df$depress_bifactor_coll <- scores[,2] 
collateral_df$genanx_bifactor_coll <- scores[,3]
collateral_df$socanx_bifactor_coll <- scores[,4]

# Score collaterals using proband-derived parameters
scores2 <- fscores(final_proband_mod, response.pattern=collateral_df[,items2], QMC=TRUE)
collateral_df$internal_bifactor <- scores2[,37] 
collateral_df$depress_bifactor <- scores2[,38]
collateral_df$genanx_bifactor <- scores2[,39]
collateral_df$socanx_bifactor <- scores2[,40]

# Write out the parameters for each model, the results table(s), and collateral_df with the final general and specific trait estimates
write.csv(collateral_df, file=paste0("/home/butellyn/parentchild_psychopathology/data/collateral_", Sys.Date(), ".csv"), row.names=FALSE)










#_________________________________________________________________________________________#

proband_df <- read.csv("/home/butellyn/parentchild_psychopathology/data/proband_2019-08-15.csv")
collateral_df <- read.csv("/home/butellyn/parentchild_psychopathology/data/collateral_2019-08-15.csv")


##### Hypothesis #4: There will be convergent and discriminant evidence for construct validity, such that higher levels of lifetime proband- and collateral-reported internalizing psychopathology severity will be associated will greater probability of endorsing lifetime suicidal ideation, physical assault, and sexual assault, and there will be no association between lifetime internalizing psychopathology severity and finger tapping speed, all controlling for sex and age.

# Load the data
item_df <- read.csv("/home/butellyn/parentchild_psychopathology/data/GOA_itemwise_for_reporter_agreement.csv") 
item_df <- item_df[,c("PROBAND_BBLID", "INTERVIEW_TYPE", "PTD003", "PTD004", "SUI002")]
colnames(item_df) <- c("bblid", "informant", "PTD003", "PTD004", "SUI002")
item_df$informant <- as.character(item_df$informant)
item_df[item_df$informant == "MP", "informant"] <- "proband"
item_df[item_df$informant == "MI", "informant"] <- "collateral"
item_df <- item_df[item_df$informant %in% c("proband", "collateral"),]
# Remove subjects with ".", change 9 to 0, and then filter for pairs remaining
item_df <- item_df[item_df$PTD003 != "." & item_df$PTD004 != "." & item_df$SUI002 != ".",]
item_df[item_df$PTD003 == 9, "PTD003"] <- 0
item_df[item_df$PTD004 == 9, "PTD004"] <- 0
item_df[item_df$SUI002 == 9, "SUI002"] <- 0
item_df$Remove <- 0
for (i in 1:nrow(item_df)) {
	bblid <- item_df[i, "bblid"]
	if (length(item_df[item_df$bblid == bblid, "bblid"]) != 2) { item_df[i, "Remove"] <- 1 }
}
item_df <- item_df[item_df$Remove != 1,]
rownames(item_df) <- 1:nrow(item_df)
item_df$PTD003 <- as.numeric(as.character(item_df$PTD003))
item_df$PTD004 <- as.numeric(as.character(item_df$PTD004))
item_df$SUI002 <- as.numeric(as.character(item_df$SUI002))
item_df$bblid <- factor(item_df$bblid)

cnb_df <- read.csv("/home/butellyn/predLongLabels/data/n9498_cnb_zscores_fr_20170202.csv")
cnb_df <- cnb_df[,c("bblid", "mot_s_z")]
cnb_df$bblid <- factor(cnb_df$bblid)

# Probands
proband_item_df <- item_df[item_df$informant == "proband",]
proband_item_df$bblid <- factor(proband_item_df$bblid)
proband_df2 <- merge(proband_df, proband_item_df, all=TRUE)
proband_df2 <- merge(proband_df2, cnb_df)
names(proband_df2)[names(proband_df2) == "internal_bifactor"] <- "internal_bifactor_P"
proband_df2$internal_sum_P <- rowSums(proband_df2[,items])  ###


collateral_df$internal_sum_C <- rowSums(collateral_df[,items])
collateral_df2 <- collateral_df[, c("bblid", "internal_bifactor", "internal_sum_C")]
names(collateral_df2)[names(collateral_df2) == "internal_bifactor"] <- "internal_bifactor_C"
comb_df <- merge(collateral_df2, proband_df2) ####
comb_df$IntDiff <- comb_df$internal_bifactor_P - comb_df$internal_bifactor_C
comb_df <- comb_df[,c("bblid", "informant", "sex", "ageAtClinicalAssess1", "internal_bifactor_P", "internal_bifactor_C", "IntDiff", "SUI002", "PTD003", "PTD004", "mot_s_z", "internal_sum_C", "internal_sum_P")]

write.csv(comb_df, file="/home/butellyn/parentchild_psychopathology/data/comb.csv", row.names=FALSE)

proband_sui_mod <- glm(comb_df$SUI002 ~ comb_df$ageAtClinicalAssess1 + factor(comb_df$sex) + comb_df$internal_bifactor_P, family="binomial")
proband_phys_mod <- glm(comb_df$PTD003 ~ comb_df$ageAtClinicalAssess1 + factor(comb_df$sex) + comb_df$internal_bifactor_P, family="binomial")
proband_sex_mod <- glm(comb_df$PTD004 ~ comb_df$ageAtClinicalAssess1 + factor(comb_df$sex) + comb_df$internal_bifactor_P, family="binomial")
proband_speed_mod <- glm(comb_df$mot_s_z ~ comb_df$ageAtClinicalAssess1 + factor(comb_df$sex) + comb_df$internal_bifactor_P)

write.csv(summary(proband_sui_mod)$coefficients, file="/home/butellyn/parentchild_psychopathology/data/mods/proband_sui_mod.csv")
write.csv(summary(proband_phys_mod)$coefficients, file="/home/butellyn/parentchild_psychopathology/data/mods/proband_phys_mod.csv")
write.csv(summary(proband_sex_mod)$coefficients, file="/home/butellyn/parentchild_psychopathology/data/mods/proband_sex_mod.csv")
write.csv(summary(proband_speed_mod)$coefficients, file="/home/butellyn/parentchild_psychopathology/data/mods/proband_speed_mod.csv")


# Collaterals
collateral_sui_mod <- glm(comb_df$SUI002 ~ comb_df$ageAtClinicalAssess1 + factor(comb_df$sex) + comb_df$internal_bifactor_C, family="binomial")
collateral_phys_mod <- glm(comb_df$PTD003 ~ comb_df$ageAtClinicalAssess1 + factor(comb_df$sex) + comb_df$internal_bifactor_C, family="binomial")
collateral_sex_mod <- glm(comb_df$PTD004 ~ comb_df$ageAtClinicalAssess1 + factor(comb_df$sex) + comb_df$internal_bifactor_C, family="binomial")
collateral_speed_mod <- glm(comb_df$mot_s_z ~ comb_df$ageAtClinicalAssess1 + factor(comb_df$sex) + comb_df$internal_bifactor_C)

write.csv(summary(collateral_sui_mod)$coefficients, file="/home/butellyn/parentchild_psychopathology/data/mods/collateral_sui_mod.csv")
write.csv(summary(collateral_phys_mod)$coefficients, file="/home/butellyn/parentchild_psychopathology/data/mods/collateral_phys_mod.csv")
write.csv(summary(collateral_sex_mod)$coefficients, file="/home/butellyn/parentchild_psychopathology/data/mods/collateral_sex_mod.csv")
write.csv(summary(collateral_speed_mod)$coefficients, file="/home/butellyn/parentchild_psychopathology/data/mods/collateral_speed_mod.csv")


##### Hypothesis #5: Females’ lifetime internalizing psychopathology will be worse, as reported by the proband and the collateral, than males’ (Kessler, 1993).
# i. Test the following hypothesis: Females will suffer from greater lifetime internalizing psychopathology than males, as reported by probands 
	# (Check distributions: Normal? Outliers? Homogeneous? Equal variances (if not, permutation test)?)
proband_internalizing <- aovp(proband_df$internal_bifactor ~ factor(proband_df$sex))

write.csv(summary(proband_internalizing)[[1]], file="/home/butellyn/parentchild_psychopathology/data/mods/proband_internalizing.csv")


# ii. Test the following hypothesis: Females will suffer from greater lifetime internalizing psychopathology than males, as reported by collaterals (T-test?)
	# (Check distributions: Normal? Outliers? Homogeneous? Equal variances (if not, permutation test)?)

collateral_internalizing <- aovp(collateral_df$internal_bifactor ~ factor(collateral_df$sex))

write.csv(summary(collateral_internalizing)[[1]], file="/home/butellyn/parentchild_psychopathology/data/mods/collateral_internalizing.csv")




##### Hypothesis #6:  Collaterals’ assessments of probands’ symptoms will be moderately correlated with probands’ assessments of their own symptoms

# i. Correlation significantly greater than .29
	# (Check distributions: Normal? Outliers? Homogeneous? Equal variances (if not, permutation test)?)

both_int_corr <- cor.test(comb_df$internal_bifactor_P, comb_df$internal_bifactor_C)
both_int_df <- data.frame(matrix(NA, nrow=1, ncol=5))
colnames(both_int_df) <- c("Sex", "cor", "df", "LCI", "UCI")
both_int_df[1,] <- c("Both", both_int_corr$estimate[[1]], both_int_corr$parameter[[1]], both_int_corr$conf.int[1], both_int_corr$conf.int[2])
write.csv(both_int_df, file="/home/butellyn/parentchild_psychopathology/data/mods/both_int_corr.csv")

female_int_corr <- cor.test(comb_df[comb_df$sex == "Female", "internal_bifactor_P"], comb_df[comb_df$sex == "Female", "internal_bifactor_C"])
female_int_df <- data.frame(matrix(NA, nrow=1, ncol=5))
colnames(female_int_df) <- c("Sex", "cor", "df", "LCI", "UCI")
female_int_df[1,] <- c("Female", female_int_corr$estimate[[1]], female_int_corr$parameter[[1]], female_int_corr$conf.int[1], female_int_corr$conf.int[2])
write.csv(female_int_df, file="/home/butellyn/parentchild_psychopathology/data/mods/female_int_corr.csv")

male_int_corr <- cor.test(comb_df[comb_df$sex == "Male", "internal_bifactor_P"], comb_df[comb_df$sex == "Male", "internal_bifactor_C"])
male_int_df <- data.frame(matrix(NA, nrow=1, ncol=5))
colnames(male_int_df) <- c("Sex", "cor", "df", "LCI", "UCI")
male_int_df[1,] <- c("Male", male_int_corr$estimate[[1]], male_int_corr$parameter[[1]], male_int_corr$conf.int[1], male_int_corr$conf.int[2])
write.csv(male_int_df, file="/home/butellyn/parentchild_psychopathology/data/mods/male_int_corr.csv")


diff_int <- t.test(comb_df$IntDiff, mu=0) #### Not normal.... 
diff_int_df <- data.frame(matrix(NA, nrow=1, ncol=6))
colnames(diff_int_df) <- c("Sex", "Mean", "T", "df", "LCI", "UCI")
diff_int_df[1,] <- c("Both", diff_int$estimate[[1]], diff_int$statistic[[1]], diff_int$parameter[[1]], diff_int$conf.int[1], diff_int$conf.int[2])
write.csv(diff_int_df, file="/home/butellyn/parentchild_psychopathology/data/mods/diff_int.csv")

# ii. (Exploratory) Test for sex differences in disagreement

# Not controlling for proband-reported lifetime internalizing severity
sex_diff_int <- t.test(comb_df$IntDiff ~ factor(comb_df$sex))
sex_diff_int_df <- data.frame(matrix(NA, nrow=1, ncol=7))
colnames(sex_diff_int_df) <- c("F Mean", "M Mean", "T", "df", "p", "LCI", "UCI")
sex_diff_int_df[1,] <- c(sex_diff_int$estimate[[1]], sex_diff_int$estimate[[2]], sex_diff_int$statistic[[1]], sex_diff_int$parameter[[1]], sex_diff_int$p.value[[1]], sex_diff_int$conf.int[1], sex_diff_int$conf.int[2])
write.csv(sex_diff_int_df, file="/home/butellyn/parentchild_psychopathology/data/mods/sex_diff_int.csv")


# Controlling for proband-reported lifetime internalizing severity
sex_diff_int_con <- glm(comb_df$IntDiff ~ factor(comb_df$sex) + comb_df$internal_bifactor_P)
write.csv(summary(sex_diff_int_con)$coefficients, file="/home/butellyn/parentchild_psychopathology/data/mods/sex_diff_int_con.csv")


##### Hypothesis #7: Given that collaterals and probands disagree on a given symptom, it will be more likely that the collateral denies its presence than the proband. As an exploratory analysis, we tested to see if this effect differed as a function of sex.

# i. Proportion tests with FDR correction
items <- c("ITEM001", "ITEM002", "ITEM003", "ITEM004", "ITEM005", "ITEM006", "ITEM007", "ITEM008", "ITEM009", "ITEM010", "ITEM011", "ITEM012", "ITEM013", "ITEM014", "ITEM015", "ITEM016", "ITEM017", "ITEM018", "ITEM019", "ITEM020", "ITEM021", "ITEM022", "ITEM023", "ITEM024", "ITEM025", "ITEM026", "ITEM027", "ITEM028", "ITEM029", "ITEM030", "ITEM031", "ITEM032", "ITEM033", "ITEM034", "ITEM035")
# Make agreement dataframe
agree_df <- data.frame(matrix(NA, nrow=nrow(df)/2, ncol=length(items)+2))
colnames(agree_df) <- c("bblid", "sex", items)
agree_df$bblid <- df[df$informant == "proband", "bblid"]
agree_df$sex <- df[df$informant == "proband", "sex"]
for (i in 1:nrow(agree_df)) {
	bblid <- agree_df[i, "bblid"]
	for (item in items) {
		pans <- df[df$bblid == bblid & df$informant == "proband", item]
		cans <- df[df$bblid == bblid & df$informant == "collateral", item]
		if (pans == 0) {
			if (cans == 0) {
				agree_df[i, item] <- "PNCN"
			} else {
				agree_df[i, item] <- "PNCY"
			}
		} else {
			if (cans == 0) {
				agree_df[i, item] <- "PYCN"
			} else {
				agree_df[i, item] <- "PYCY"
			}
		}
	}
}
write.csv(agree_df, file="/home/butellyn/parentchild_psychopathology/data/agreement.csv", row.names=FALSE)

agree_df <- read.csv("/home/butellyn/parentchild_psychopathology/data/agreement.csv")

results_df <- data.frame(matrix(NA, nrow=35, ncol=4))
colnames(results_df) <- c("Item", "ChiSq", "P", "BON_P")
results_df$Item <- c("ITEM001", "ITEM002", "ITEM003", "ITEM004", "ITEM005", "ITEM006", "ITEM007", "ITEM008", "ITEM009", "ITEM010", "ITEM011", "ITEM012", "ITEM013", "ITEM014", "ITEM015", "ITEM016", "ITEM017", "ITEM018", "ITEM019", "ITEM020", "ITEM021", "ITEM022", "ITEM023", "ITEM024", "ITEM025", "ITEM026", "ITEM027", "ITEM028", "ITEM029", "ITEM030", "ITEM031", "ITEM032", "ITEM033", "ITEM034", "ITEM035")
i=1
for (item in items) {
	tmp_df <- agree_df[, c("bblid", item)]
	py <- length(tmp_df[,item][tmp_df[,item] == "PYCN"])
	cy <- length(tmp_df[,item][tmp_df[,item] == "PNCY"])
	res <- prop.test(x=c(py,cy), n=c(py+cy, py+cy))
	results_df[i, "Item"] <- item
	results_df[i, "ChiSq"] <- res$statistic[[1]]
	results_df[i, "P"] <- res$p.value[[1]]

	i=i+1
}

results_df$BON_P <- results_df$P*35
write.csv(results_df, file="/home/butellyn/parentchild_psychopathology/data/mods/prop_disagree.csv", row.names=FALSE)


# ii. Proportion tests with FDR correction, split by sex
# Females
results_df_F <- data.frame(matrix(NA, nrow=35, ncol=4))
colnames(results_df_F) <- c("Item", "ChiSq", "P", "BON_P")
results_df_F$Item <- c("ITEM001", "ITEM002", "ITEM003", "ITEM004", "ITEM005", "ITEM006", "ITEM007", "ITEM008", "ITEM009", "ITEM010", "ITEM011", "ITEM012", "ITEM013", "ITEM014", "ITEM015", "ITEM016", "ITEM017", "ITEM018", "ITEM019", "ITEM020", "ITEM021", "ITEM022", "ITEM023", "ITEM024", "ITEM025", "ITEM026", "ITEM027", "ITEM028", "ITEM029", "ITEM030", "ITEM031", "ITEM032", "ITEM033", "ITEM034", "ITEM035")
i=1
for (item in items) {
	tmp_df_F <- agree_df[agree_df$sex == "Female", c("bblid", item)]
	py <- length(tmp_df_F[,item][tmp_df_F[,item] == "PYCN"])
	cy <- length(tmp_df_F[,item][tmp_df_F[,item] == "PNCY"])
	res <- prop.test(x=c(py,cy), n=c(py+cy, py+cy))
	results_df_F[i, "Item"] <- item
	results_df_F[i, "ChiSq"] <- res$statistic[[1]]
	results_df_F[i, "P"] <- res$p.value[[1]]

	i=i+1
}

results_df_F$BON_P <- results_df_F$P*35
write.csv(results_df_F, file="/home/butellyn/parentchild_psychopathology/data/mods/prop_disagree_F.csv", row.names=FALSE)

# Males
results_df_M <- data.frame(matrix(NA, nrow=35, ncol=4))
colnames(results_df_M) <- c("Item", "ChiSq", "P", "BON_P")
results_df_M$Item <- c("ITEM001", "ITEM002", "ITEM003", "ITEM004", "ITEM005", "ITEM006", "ITEM007", "ITEM008", "ITEM009", "ITEM010", "ITEM011", "ITEM012", "ITEM013", "ITEM014", "ITEM015", "ITEM016", "ITEM017", "ITEM018", "ITEM019", "ITEM020", "ITEM021", "ITEM022", "ITEM023", "ITEM024", "ITEM025", "ITEM026", "ITEM027", "ITEM028", "ITEM029", "ITEM030", "ITEM031", "ITEM032", "ITEM033", "ITEM034", "ITEM035")
i=1
for (item in items) {
	tmp_df_M <- agree_df[agree_df$sex == "Male", c("bblid", item)]
	py <- length(tmp_df_M[,item][tmp_df_M[,item] == "PYCN"])
	cy <- length(tmp_df_M[,item][tmp_df_M[,item] == "PNCY"])
	res <- prop.test(x=c(py,cy), n=c(py+cy, py+cy))
	results_df_M[i, "Item"] <- item
	results_df_M[i, "ChiSq"] <- res$statistic[[1]]
	results_df_M[i, "P"] <- res$p.value[[1]]

	i=i+1
}

results_df_M$BON_P <- results_df_M$P*35
write.csv(results_df_M, file="/home/butellyn/parentchild_psychopathology/data/mods/prop_disagree_M.csv", row.names=FALSE)




# iii. (Exploratory) Create a table of disagreement/agreement proportions, each type, by sex, with and without people for whom neither the proband nor the collateral endorsed any symptoms

agree_table <- data.frame(matrix(NA, ncol=9, nrow=37))
colnames(agree_table) <- c("", "Male Probands", "", "", "", "Female Probands", "", "", "")
agree_table[1,] <- c("", "Agree", "", "Disagree", "", "Agree", "", "Disagree", "")
agree_table[2,] <- c("Item", "P N & C N", "P Y & C Y", "P Y & C N", "P N & C Y", "P N & C N", "P Y & C Y", "P Y & C N", "P N & C Y")

i=3
n_males <- nrow(agree_df[agree_df$sex == "Male",])
n_females <- nrow(agree_df[agree_df$sex == "Female",])
for (item in items) {
	agree_table[i, 1] <- item
	#### Males
	# PNCN
	agree_table[i, 2] <- paste0(round(nrow(agree_df[agree_df$sex == "Male" & agree_df[,item] == "PNCN",])/n_males, digits=3)*100, "%")
	# PYCY
	agree_table[i, 3] <- paste0(round(nrow(agree_df[agree_df$sex == "Male" & agree_df[,item] == "PYCY",])/n_males, digits=3)*100, "%")
	# PYCN
	agree_table[i, 4] <- paste0(round(nrow(agree_df[agree_df$sex == "Male" & agree_df[,item] == "PYCN",])/n_males, digits=3)*100, "%")
	# PNCY
	agree_table[i, 5] <- paste0(round(nrow(agree_df[agree_df$sex == "Male" & agree_df[,item] == "PNCY",])/n_males, digits=3)*100, "%")

	#### Females
	# PNCN
	agree_table[i, 6] <- paste0(round(nrow(agree_df[agree_df$sex == "Female" & agree_df[,item] == "PNCN",])/n_females, digits=3)*100, "%")
	# PYCY
	agree_table[i, 7] <- paste0(round(nrow(agree_df[agree_df$sex == "Female" & agree_df[,item] == "PYCY",])/n_females, digits=3)*100, "%")
	# PYCN
	agree_table[i, 8] <- paste0(round(nrow(agree_df[agree_df$sex == "Female" & agree_df[,item] == "PYCN",])/n_females, digits=3)*100, "%")
	# PNCY
	agree_table[i, 9] <- paste0(round(nrow(agree_df[agree_df$sex == "Female" & agree_df[,item] == "PNCY",])/n_females, digits=3)*100, "%")

	i=i+1
}

rownames(agree_table) <- c()
pdf(file="/home/butellyn/parentchild_psychopathology/plots/agree_table.pdf", width=10, height=12)
grid.arrange(tableGrob(agree_table)) # Figure out how to get rid of rownames in table
dev.off()

write.csv(agree_table, file="/home/butellyn/parentchild_psychopathology/data/agree_table.csv", row.names=FALSE)













