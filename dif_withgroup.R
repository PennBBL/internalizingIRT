### This script reruns the DIF analyses in analyses.R, this time including
### the group factors in each model
###
### Ellyn Butler
### August 20, 2019


# Load packages
library('ggplot2')
library('gridExtra')

# Load the data
proband_df <- read.csv("~/Documents/internalizingIRT/data/proband_2019-08-15.csv")
#collateral_df <- read.csv("~/Documents/internalizingIRT/data/collateral_2019-08-15.csv")
collateral_df <- read.csv("~/Documents/internalizingIRT/data/collateral_2019-08-15.csv")

# i. _________________ Check for all DIF in probands. _________________
items <- c("ITEM001", "ITEM002", "ITEM003", "ITEM004", "ITEM005", "ITEM006", "ITEM007", "ITEM008", "ITEM009", "ITEM010", "ITEM011", "ITEM012", "ITEM013", "ITEM014", "ITEM015", "ITEM016", "ITEM017", "ITEM018", "ITEM019", "ITEM020", "ITEM021", "ITEM022", "ITEM023", "ITEM024", "ITEM025", "ITEM026", "ITEM027", "ITEM028", "ITEM029", "ITEM030", "ITEM031", "ITEM032", "ITEM033", "ITEM034", "ITEM035")
loadings <- c(rep(1, 13), rep(2, 15), rep(3, 7))
proband_results <- data.frame(matrix(NA, nrow=35, ncol=7))
colnames(proband_results) <- c("Item", "MainEffectMale", "Main", "Interaction", "DiffAIC", "ChiSq", "ChiSqP")
i=1
for (item in items) {
	# Determine index of item for testing
	item_ind <- which(items %in% item)

	# Dichotomize item, if necessary
	if (length(unique(proband_df[,item])) > 2) {
		proband_df[proband_df[,item] == 0, paste0(item, "_dich")] <- 0
		proband_df[proband_df[,item] != 0, paste0(item, "_dich")] <- 1
		# Test for DIF in item
		tmp_log_mod <- glm(proband_df[,paste0(item, "_dich")] ~ proband_df[,paste0("general_", item, "_ex")] + proband_df[,paste0("dep_", item, "_ex")] + proband_df[,paste0("gad_", item, "_ex")] + proband_df[,paste0("soc_", item, "_ex")] + proband_df$sex + proband_df[,paste0("general_", item, "_ex")]*proband_df$sex, family="binomial")
		assign(paste0(item, "_mod"), tmp_log_mod)

		tmp_simp_log_mod <- glm(proband_df[,paste0(item, "_dich")] ~ proband_df[,paste0("general_", item, "_ex")] + proband_df[,paste0("dep_", item, "_ex")] + proband_df[,paste0("gad_", item, "_ex")] + proband_df[,paste0("soc_", item, "_ex")], family="binomial")
		assign(paste0(item, "_simp_mod"), tmp_simp_log_mod)
	} else {
		# Test for DIF in item
		tmp_log_mod <- glm(proband_df[,item] ~ proband_df[,paste0("general_", item, "_ex")] + proband_df[,paste0("dep_", item, "_ex")] + proband_df[,paste0("gad_", item, "_ex")] + proband_df[,paste0("soc_", item, "_ex")] + proband_df$sex + proband_df[,paste0("general_", item, "_ex")]*proband_df$sex, family="binomial")
		assign(paste0(item, "_mod"), tmp_log_mod)

		tmp_simp_log_mod <- glm(proband_df[,item] ~ proband_df[,paste0("general_", item, "_ex")] + proband_df[,paste0("dep_", item, "_ex")] + proband_df[,paste0("gad_", item, "_ex")] + proband_df[,paste0("soc_", item, "_ex")], family="binomial")
		assign(paste0(item, "_simp_mod"), tmp_simp_log_mod)
	}

	# Run a ChiSq Test
	chisq <- anova(tmp_simp_log_mod, tmp_log_mod, test="Chisq")

	# Put relevant statistics in results dataframe
	proband_results[i, "Item"] <- item
	proband_results[i, "MainEffectMale"] <- summary(tmp_log_mod)$coefficients[[6]]
	proband_results[i, "Main"] <- summary(tmp_log_mod)$coefficients[6,4]
	proband_results[i, "Interaction"] <- summary(tmp_log_mod)$coefficients[7,4]
	proband_results[i, "DiffAIC"] <- summary(tmp_log_mod)$aic - summary(tmp_simp_log_mod)$aic
	proband_results[i, "ChiSq"] <- chisq$Deviance[2]
	proband_results[i, "ChiSqP"] <- chisq[[5]][2]

	i=i+1
}

# Write out logistic model results and bifactor loadings
write.csv(proband_results, file=paste0("~/Documents/internalizingIRT/data/proband_dif_withgroups_", Sys.Date(), ".csv"), row.names=FALSE)

##########################
items2 <- c("ITEM001", "ITEM002_F", "ITEM002_M", "ITEM003", "ITEM004", "ITEM005", "ITEM006", "ITEM007", "ITEM008", "ITEM009", "ITEM010", "ITEM011", "ITEM012", "ITEM013", "ITEM014", "ITEM015", "ITEM016", "ITEM017", "ITEM018", "ITEM019", "ITEM020", "ITEM021", "ITEM022", "ITEM023", "ITEM024", "ITEM025", "ITEM026", "ITEM027", "ITEM028", "ITEM029", "ITEM030", "ITEM031", "ITEM032", "ITEM033", "ITEM034", "ITEM035")

loadings <- c(rep(1, 14), rep(2, 15), rep(3, 7)) ####
proband_results2 <- data.frame(matrix(NA, nrow=34, ncol=7))
colnames(proband_results2) <- c("Item", "MainEffectMale", "Main", "MainEffectMale", "Interaction", "DiffAIC", "ChiSq", "ChiSqP")
i=1
for (item in items2) {
	if (item != "ITEM002_F" & item != "ITEM002_M") {
		# Determine index of item for testing
		item_ind <- which(items2 %in% item)

		# Dichotomize item, if necessary
		if (length(unique(proband_df[,item])) > 2) {
			# Test for DIF in item
			tmp_log_mod <- glm(proband_df[,paste0(item, "_dich")] ~ proband_df[,paste0("general_", item, "_ex2")] + proband_df[,paste0("dep_", item, "_ex2")] + proband_df[,paste0("gad_", item, "_ex2")] + proband_df[,paste0("soc_", item, "_ex2")] + proband_df$sex + proband_df[,paste0("general_", item, "_ex2")]*proband_df$sex, family="binomial")
			assign(paste0(item, "_mod2"), tmp_log_mod)

			tmp_simp_log_mod <- glm(proband_df[,paste0(item, "_dich")] ~ proband_df[,paste0("general_", item, "_ex2")] + proband_df[,paste0("dep_", item, "_ex2")] + proband_df[,paste0("gad_", item, "_ex2")] + proband_df[,paste0("soc_", item, "_ex2")], family="binomial")
			assign(paste0(item, "_simp_mod2"), tmp_simp_log_mod)
		} else {
			# Test for DIF in item
			tmp_log_mod <- glm(proband_df[,item] ~ proband_df[,paste0("general_", item, "_ex2")] + proband_df[,paste0("dep_", item, "_ex2")] + proband_df[,paste0("gad_", item, "_ex2")] + proband_df[,paste0("soc_", item, "_ex2")] + proband_df$sex + proband_df[,paste0("general_", item, "_ex2")]*proband_df$sex, family="binomial")
			assign(paste0(item, "_mod2"), tmp_log_mod)

			tmp_simp_log_mod <- glm(proband_df[,item] ~ proband_df[,paste0("general_", item, "_ex2")] + proband_df[,paste0("dep_", item, "_ex2")] + proband_df[,paste0("gad_", item, "_ex2")] + proband_df[,paste0("soc_", item, "_ex2")], family="binomial")
			assign(paste0(item, "_simp_mod2"), tmp_simp_log_mod)
		}

		# Run a ChiSq Test
		chisq <- anova(tmp_simp_log_mod, tmp_log_mod, test="Chisq")

		# Put relevant statistics in results dataframe
		proband_results2[i, "Item"] <- item
		proband_results2[i, "MainEffectMale"] <- summary(tmp_log_mod)$coefficients[[6]]
		proband_results2[i, "Main"] <- summary(tmp_log_mod)$coefficients[6,4]
		proband_results2[i, "Interaction"] <- summary(tmp_log_mod)$coefficients[7,4]
		proband_results2[i, "DiffAIC"] <- summary(tmp_log_mod)$aic - summary(tmp_simp_log_mod)$aic
		proband_results2[i, "ChiSq"] <- chisq$Deviance[2]
		proband_results2[i, "ChiSqP"] <- chisq[[5]][2]

		i=i+1
	}
}


write.csv(proband_results2, file=paste0("~/Documents/internalizingIRT/data/proband_dif2_withgroups_", Sys.Date(), ".csv"), row.names=FALSE)

# i. _________________ Check for all DIF in collaterals. _________________
items <- c("ITEM001", "ITEM002", "ITEM003", "ITEM004", "ITEM005", "ITEM006", "ITEM007", "ITEM008", "ITEM009", "ITEM010", "ITEM011", "ITEM012", "ITEM013", "ITEM014", "ITEM015", "ITEM016", "ITEM017", "ITEM018", "ITEM019", "ITEM020", "ITEM021", "ITEM022", "ITEM023", "ITEM024", "ITEM025", "ITEM026", "ITEM027", "ITEM028", "ITEM029", "ITEM030", "ITEM031", "ITEM032", "ITEM033", "ITEM034", "ITEM035")
loadings <- c(rep(1, 13), rep(2, 15), rep(3, 7))
collateral_results <- data.frame(matrix(NA, nrow=35, ncol=7))
colnames(collateral_results) <- c("Item", "MainEffectMale", "Main", "Interaction", "DiffAIC", "ChiSq", "ChiSqP")
i=1
for (item in items) {
	# Determine index of item for testing
	item_ind <- which(items %in% item)

	# Dichotomize item, if necessary
	if (length(unique(collateral_df[,item])) > 2) {
		collateral_df[collateral_df[,item] == 0, paste0(item, "_dich")] <- 0
		collateral_df[collateral_df[,item] != 0, paste0(item, "_dich")] <- 1
		# Test for DIF in item
		tmp_log_mod <- glm(collateral_df[,paste0(item, "_dich")] ~ collateral_df[,paste0("general_", item, "_ex")] + collateral_df[,paste0("dep_", item, "_ex")] + collateral_df[,paste0("gad_", item, "_ex")] + collateral_df[,paste0("soc_", item, "_ex")] + collateral_df$sex + collateral_df[,paste0("general_", item, "_ex")]*collateral_df$sex, family="binomial")
		assign(paste0(item, "_mod"), tmp_log_mod)

		tmp_simp_log_mod <- glm(collateral_df[,paste0(item, "_dich")] ~ collateral_df[,paste0("general_", item, "_ex")] + collateral_df[,paste0("dep_", item, "_ex")] + collateral_df[,paste0("gad_", item, "_ex")] + collateral_df[,paste0("soc_", item, "_ex")], family="binomial")
		assign(paste0(item, "_simp_mod"), tmp_simp_log_mod)
	} else {
		# Test for DIF in item
		tmp_log_mod <- glm(collateral_df[,item] ~ collateral_df[,paste0("general_", item, "_ex")] + collateral_df[,paste0("dep_", item, "_ex")] + collateral_df[,paste0("gad_", item, "_ex")] + collateral_df[,paste0("soc_", item, "_ex")] + collateral_df$sex + collateral_df[,paste0("general_", item, "_ex")]*collateral_df$sex, family="binomial")
		assign(paste0(item, "_mod"), tmp_log_mod)

		tmp_simp_log_mod <- glm(collateral_df[,item] ~ collateral_df[,paste0("general_", item, "_ex")] + collateral_df[,paste0("dep_", item, "_ex")] + collateral_df[,paste0("gad_", item, "_ex")] + collateral_df[,paste0("soc_", item, "_ex")], family="binomial")
		assign(paste0(item, "_simp_mod"), tmp_simp_log_mod)
	}

	# Run a ChiSq Test
	chisq <- anova(tmp_simp_log_mod, tmp_log_mod, test="Chisq")

	# Put relevant statistics in results dataframe
	collateral_results[i, "Item"] <- item
	collateral_results[i, "MainEffectMale"] <- summary(tmp_log_mod)$coefficients[[6]]
	collateral_results[i, "Main"] <- summary(tmp_log_mod)$coefficients[6,4]
	collateral_results[i, "Interaction"] <- summary(tmp_log_mod)$coefficients[7,4]
	collateral_results[i, "DiffAIC"] <- summary(tmp_log_mod)$aic - summary(tmp_simp_log_mod)$aic
	collateral_results[i, "ChiSq"] <- chisq$Deviance[2]
	collateral_results[i, "ChiSqP"] <- chisq[[5]][2]

	i=i+1
}

# Write out logistic model results and bifactor loadings
write.csv(collateral_results, file=paste0("~/Documents/internalizingIRT/data/collateral_dif_withgroups_", Sys.Date(), ".csv"), row.names=FALSE)

##########################
items2 <- c("ITEM001", "ITEM002_F", "ITEM002_M", "ITEM003", "ITEM004", "ITEM005", "ITEM006", "ITEM007", "ITEM008", "ITEM009", "ITEM010", "ITEM011", "ITEM012", "ITEM013", "ITEM014", "ITEM015", "ITEM016", "ITEM017", "ITEM018", "ITEM019", "ITEM020", "ITEM021", "ITEM022", "ITEM023", "ITEM024", "ITEM025", "ITEM026", "ITEM027", "ITEM028", "ITEM029", "ITEM030", "ITEM031", "ITEM032", "ITEM033", "ITEM034", "ITEM035")

loadings <- c(rep(1, 14), rep(2, 15), rep(3, 7)) ####
collateral_results2 <- data.frame(matrix(NA, nrow=34, ncol=7))
colnames(collateral_results2) <- c("Item", "MainEffectMale", "Main", "Interaction", "DiffAIC", "ChiSq", "ChiSqP")
i=1
for (item in items2) {
	if (item != "ITEM002_F" & item != "ITEM002_M") {
		# Determine index of item for testing
		item_ind <- which(items2 %in% item)

		# Dichotomize item, if necessary
		if (length(unique(collateral_df[,item])) > 2) {
			# Test for DIF in item
			tmp_log_mod <- glm(collateral_df[,paste0(item, "_dich")] ~ collateral_df[,paste0("general_", item, "_ex2")] + collateral_df[,paste0("dep_", item, "_ex2")] + collateral_df[,paste0("gad_", item, "_ex2")] + collateral_df[,paste0("soc_", item, "_ex2")] + collateral_df$sex + collateral_df[,paste0("general_", item, "_ex2")]*collateral_df$sex, family="binomial")
			assign(paste0(item, "_mod2"), tmp_log_mod)

			tmp_simp_log_mod <- glm(collateral_df[,paste0(item, "_dich")] ~ collateral_df[,paste0("general_", item, "_ex2")] + collateral_df[,paste0("dep_", item, "_ex2")] + collateral_df[,paste0("gad_", item, "_ex2")] + collateral_df[,paste0("soc_", item, "_ex2")], family="binomial")
			assign(paste0(item, "_simp_mod2"), tmp_simp_log_mod)
		} else {
			# Test for DIF in item
			tmp_log_mod <- glm(collateral_df[,item] ~ collateral_df[,paste0("general_", item, "_ex2")] + collateral_df[,paste0("dep_", item, "_ex2")] + collateral_df[,paste0("gad_", item, "_ex2")] + collateral_df[,paste0("soc_", item, "_ex2")] + collateral_df$sex + collateral_df[,paste0("general_", item, "_ex2")]*collateral_df$sex, family="binomial")
			assign(paste0(item, "_mod2"), tmp_log_mod)

			tmp_simp_log_mod <- glm(collateral_df[,item] ~ collateral_df[,paste0("general_", item, "_ex2")] + collateral_df[,paste0("dep_", item, "_ex2")] + collateral_df[,paste0("gad_", item, "_ex2")] + collateral_df[,paste0("soc_", item, "_ex2")], family="binomial")
			assign(paste0(item, "_simp_mod2"), tmp_simp_log_mod)
		}

		# Run a ChiSq Test
		chisq <- anova(tmp_simp_log_mod, tmp_log_mod, test="Chisq")

		# Put relevant statistics in results dataframe
		collateral_results2[i, "Item"] <- item
		collateral_results2[i, "MainEffectMale"] <- summary(tmp_log_mod)$coefficients[[6]]
		collateral_results2[i, "Main"] <- summary(tmp_log_mod)$coefficients[6,4]
		collateral_results2[i, "Interaction"] <- summary(tmp_log_mod)$coefficients[7,4]
		collateral_results2[i, "DiffAIC"] <- summary(tmp_log_mod)$aic - summary(tmp_simp_log_mod)$aic
		collateral_results2[i, "ChiSq"] <- chisq$Deviance[2]
		collateral_results2[i, "ChiSqP"] <- chisq[[5]][2]

		i=i+1
	}
}


write.csv(collateral_results2, file=paste0("~/Documents/internalizingIRT/data/collateral_dif2_withgroups_", Sys.Date(), ".csv"), row.names=FALSE)
