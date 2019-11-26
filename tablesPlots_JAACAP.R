### Script to produce csvs for tables and graphs (Modified for JAACAP)
###
### Ellyn Butler
### August 15, 2019 - November 26, 2019


# Load packages
library('ggplot2')
library('gridExtra')
library('dplyr')
library('psych')

# Load the data
proband_df <- read.csv("/home/butellyn/parentchild_psychopathology/data/proband_2019-08-15.csv")
collateral_df <- read.csv("/home/butellyn/parentchild_psychopathology/data/collateral_2019-08-15.csv")

items <- c(paste0("ITEM00", 1:9), paste0("ITEM0", 10:35))

################################ Eigenvalue plots ################################

proband_eigenvalues <- eigen(polychoric(proband_df[,items])$rho)$values
peigen_df <- data.frame(matrix(NA, nrow=length(proband_eigenvalues), ncol=2))
colnames(peigen_df) <- c("compnum", "eigen")
peigen_df$compnum <- 1:35
peigen_df$eigen <- proband_eigenvalues

pro_eigen_plot <- ggplot(peigen_df, aes(x=compnum, y=eigen)) + geom_line(stat="identity") + geom_point() + theme_minimal() + 
	xlab("Component Number") + ylab("Eigenvalues of Components") + scale_y_continuous(limits=c(0, 20)) +
	ggtitle("Proband Scree Plot") + theme(plot.title = element_text(size=25), axis.title = element_text(size=20), axis.text = element_text(size=15))

collateral_eigenvalues <- eigen(polychoric(collateral_df[,items])$rho)$values
ceigen_df <- data.frame(matrix(NA, nrow=length(collateral_eigenvalues), ncol=2))
colnames(ceigen_df) <- c("compnum", "eigen")
ceigen_df$compnum <- 1:35
ceigen_df$eigen <- collateral_eigenvalues

coll_eigen_plot <- ggplot(ceigen_df, aes(x=compnum, y=eigen)) + geom_line(stat="identity") + geom_point() + theme_minimal() + 
	xlab("Component Number") + ylab("Eigenvalues of Components") + scale_y_continuous(limits=c(0, 20)) +
	ggtitle("Collateral Scree Plot") + theme(plot.title = element_text(size=25), axis.title = element_text(size=20), axis.text = element_text(size=15))

pdf(file="/home/butellyn/parentchild_psychopathology/plots/JAACAP/supplement_figure1.pdf", width=14, height=6)
grid.arrange(pro_eigen_plot, coll_eigen_plot, ncol=2)
dev.off()


################################ DIF Tables and plots ################################
# Probands
# Round #1
proband_results <- read.csv("/home/butellyn/parentchild_psychopathology/data/proband_dif_withgroups_2019-08-20.csv")
proband_diffaic_hist <- ggplot(proband_results, aes(x=DiffAIC)) + geom_histogram() + theme_minimal() + 
	ggtitle("Probands: Differences in AIC (1)") + theme(plot.title = element_text(size=30), axis.title = element_text(size=25), axis.text = element_text(size=20)) #+ scale_x_continuous(limits=c(-130,40), breaks=seq(-130, 40, 10))

proband_results_final <- data.frame(matrix(NA, nrow=nrow(proband_results), ncol=ncol(proband_results)+1)) ###
colnames(proband_results_final) <- c("Item", "Main-P", "Interaction-P", "AIC", "Chi-Sq", "Chi-Sq-P", "Chi-Sq-P-Bon") ##
proband_results[, "ChiSqPBon"] <- proband_results[, "ChiSqP"]*35
proband_results_final$Item <- proband_results$Item
proband_results_final$AIC <- round(proband_results$DiffAIC, digits=2)
proband_results_final[,"Chi-Sq"] <- round(proband_results$ChiSq, digits=2)
for (i in 1:nrow(proband_results)) {
	# Main
	if (proband_results[i, "Main"] < .00001) { proband_results_final[i, "Main-P"] <- "< .00001"
	} else if (proband_results[i, "Main"] < .0001) { proband_results_final[i, "Main-P"] <- "< .0001"
	} else if (proband_results[i, "Main"] < .001) { proband_results_final[i, "Main-P"] <- "< .001"
	} else if (proband_results[i, "Main"] < .01) { proband_results_final[i, "Main-P"] <- "< .01"
	} else if (proband_results[i, "Main"] < .05) { proband_results_final[i, "Main-P"] <- "< .05"
	} else if (proband_results[i, "Main"] > .05) { proband_results_final[i, "Main-P"] <- "> .05"
	}

	# Interaction
	if (proband_results[i, "Interaction"] < .00001) { proband_results_final[i, "Interaction-P"] <- "< .00001"
	} else if (proband_results[i, "Interaction"] < .0001) { proband_results_final[i, "Interaction-P"] <- "< .0001"
	} else if (proband_results[i, "Interaction"] < .001) { proband_results_final[i, "Interaction-P"] <- "< .001"
	} else if (proband_results[i, "Interaction"] < .01) { proband_results_final[i, "Interaction-P"] <- "< .01"
	} else if (proband_results[i, "Interaction"] < .05) { proband_results_final[i, "Interaction-P"] <- "< .05"
	} else if (proband_results[i, "Interaction"] > .05) { proband_results_final[i, "Interaction-P"] <- "> .05"
	}

	# ChiSqP
	if (proband_results[i, "ChiSqP"] < .00001) { proband_results_final[i, "Chi-Sq-P"] <- "< .00001"
	} else if (proband_results[i, "ChiSqP"] < .0001) { proband_results_final[i, "Chi-Sq-P"] <- "< .0001"
	} else if (proband_results[i, "ChiSqP"] < .001) { proband_results_final[i, "Chi-Sq-P"] <- "< .001"
	} else if (proband_results[i, "ChiSqP"] < .01) { proband_results_final[i, "Chi-Sq-P"] <- "< .01"
	} else if (proband_results[i, "ChiSqP"] < .05) { proband_results_final[i, "Chi-Sq-P"] <- "< .05"
	} else if (proband_results[i, "ChiSqP"] > .05) { proband_results_final[i, "Chi-Sq-P"] <- "> .05"
	}

	# ChiSqPBon
	if (proband_results[i, "ChiSqPBon"] < .00001) { proband_results_final[i, "Chi-Sq-P-Bon"] <- "< .00001"
	} else if (proband_results[i, "ChiSqPBon"] < .0001) { proband_results_final[i, "Chi-Sq-P-Bon"] <- "< .0001"
	} else if (proband_results[i, "ChiSqPBon"] < .001) { proband_results_final[i, "Chi-Sq-P-Bon"] <- "< .001"
	} else if (proband_results[i, "ChiSqPBon"] < .01) { proband_results_final[i, "Chi-Sq-P-Bon"] <- "< .01"
	} else if (proband_results[i, "ChiSqPBon"] < .05) { proband_results_final[i, "Chi-Sq-P-Bon"] <- "< .05"
	} else if (proband_results[i, "ChiSqPBon"] > .05) { proband_results_final[i, "Chi-Sq-P-Bon"] <- "> .05"
	}
}

proband_results_final <- proband_results_final[,c("Item", "AIC", "Chi-Sq", "Chi-Sq-P", "Chi-Sq-P-Bon", "Main-P", "Interaction-P")]
write.csv(proband_results_final, "/home/butellyn/parentchild_psychopathology/data/forTables/proband_dif_withgroups.csv", row.names=FALSE)

# Round #2
proband_results2 <- read.csv("/home/butellyn/parentchild_psychopathology/data/proband_dif2_withgroups_2019-08-20.csv")
proband_diffaic_hist2 <- ggplot(proband_results2, aes(x=DiffAIC)) + geom_histogram() + theme_minimal() + 
	ggtitle("Probands: Differences in AIC (2)") + theme(plot.title = element_text(size=30), axis.title = element_text(size=25), axis.text = element_text(size=20)) #+ scale_x_continuous(limits=c(-130,10), breaks=seq(-130, 10, 10)) 

proband_results2_final <- data.frame(matrix(NA, nrow=nrow(proband_results2), ncol=ncol(proband_results2)+1))
colnames(proband_results2_final) <- c("Item", "Main-P", "Interaction-P", "AIC", "Chi-Sq", "Chi-Sq-P", "Chi-Sq-P-Bon") ##
proband_results2[, "ChiSqPBon"] <- proband_results2[, "ChiSqP"]*34
proband_results2_final$Item <- proband_results2$Item
proband_results2_final$AIC <- round(proband_results2$DiffAIC, digits=2)
proband_results2_final[,"Chi-Sq"] <- round(proband_results2$ChiSq, digits=2)
for (i in 1:nrow(proband_results2)) {
	# Main
	if (proband_results2[i, "Main"] < .00001) { proband_results2_final[i, "Main-P"] <- "< .00001"
	} else if (proband_results2[i, "Main"] < .0001) { proband_results2_final[i, "Main-P"] <- "< .0001"
	} else if (proband_results2[i, "Main"] < .001) { proband_results2_final[i, "Main-P"] <- "< .001"
	} else if (proband_results2[i, "Main"] < .01) { proband_results2_final[i, "Main-P"] <- "< .01"
	} else if (proband_results2[i, "Main"] < .05) { proband_results2_final[i, "Main-P"] <- "< .05"
	} else if (proband_results2[i, "Main"] > .05) { proband_results2_final[i, "Main-P"] <- "> .05"
	}

	# Interaction
	if (proband_results2[i, "Interaction"] < .00001) { proband_results2_final[i, "Interaction-P"] <- "< .00001"
	} else if (proband_results2[i, "Interaction"] < .0001) { proband_results2_final[i, "Interaction-P"] <- "< .0001"
	} else if (proband_results2[i, "Interaction"] < .001) { proband_results2_final[i, "Interaction-P"] <- "< .001"
	} else if (proband_results2[i, "Interaction"] < .01) { proband_results2_final[i, "Interaction-P"] <- "< .01"
	} else if (proband_results2[i, "Interaction"] < .05) { proband_results2_final[i, "Interaction-P"] <- "< .05"
	} else if (proband_results2[i, "Interaction"] > .05) { proband_results2_final[i, "Interaction-P"] <- "> .05"
	}

	# ChiSqP
	if (proband_results2[i, "ChiSqP"] < .00001) { proband_results2_final[i, "Chi-Sq-P"] <- "< .00001"
	} else if (proband_results2[i, "ChiSqP"] < .0001) { proband_results2_final[i, "Chi-Sq-P"] <- "< .0001"
	} else if (proband_results2[i, "ChiSqP"] < .001) { proband_results2_final[i, "Chi-Sq-P"] <- "< .001"
	} else if (proband_results2[i, "ChiSqP"] < .01) { proband_results2_final[i, "Chi-Sq-P"] <- "< .01"
	} else if (proband_results2[i, "ChiSqP"] < .05) { proband_results2_final[i, "Chi-Sq-P"] <- "< .05"
	} else if (proband_results2[i, "ChiSqP"] > .05) { proband_results2_final[i, "Chi-Sq-P"] <- "> .05"
	}

	# ChiSqPBon
	if (proband_results2[i, "ChiSqPBon"] < .00001) { proband_results2_final[i, "Chi-Sq-P-Bon"] <- "< .00001"
	} else if (proband_results2[i, "ChiSqPBon"] < .0001) { proband_results2_final[i, "Chi-Sq-P-Bon"] <- "< .0001"
	} else if (proband_results2[i, "ChiSqPBon"] < .001) { proband_results2_final[i, "Chi-Sq-P-Bon"] <- "< .001"
	} else if (proband_results2[i, "ChiSqPBon"] < .01) { proband_results2_final[i, "Chi-Sq-P-Bon"] <- "< .01"
	} else if (proband_results2[i, "ChiSqPBon"] < .05) { proband_results2_final[i, "Chi-Sq-P-Bon"] <- "< .05"
	} else if (proband_results2[i, "ChiSqPBon"] > .05) { proband_results2_final[i, "Chi-Sq-P-Bon"] <- "> .05"
	}
}

proband_results2_final <- proband_results2_final[,c("Item", "AIC", "Chi-Sq", "Chi-Sq-P", "Chi-Sq-P-Bon", "Main-P", "Interaction-P")]
write.csv(proband_results2_final, "/home/butellyn/parentchild_psychopathology/data/forTables/proband_dif2_withgroups.csv", row.names=FALSE)

# Collaterals
# Round #1
collateral_results <- read.csv("/home/butellyn/parentchild_psychopathology/data/collateral_dif_withgroups_2019-08-20.csv")
collateral_diffaic_hist <- ggplot(collateral_results, aes(x=DiffAIC)) + geom_histogram() + theme_minimal() + 
	ggtitle("Collaterals: Differences in AIC (1)") + theme(plot.title = element_text(size=30), axis.title = element_text(size=25), axis.text = element_text(size=20))#+ scale_x_continuous(limits=c(-130,10), breaks=seq(-130, 10, 10))

collateral_results_final <- data.frame(matrix(NA, nrow=nrow(collateral_results), ncol=ncol(collateral_results)+1))
colnames(collateral_results_final) <- c("Item", "Main-P", "Interaction-P", "AIC", "Chi-Sq", "Chi-Sq-P", "Chi-Sq-P-Bon") ##
collateral_results[, "ChiSqPBon"] <- collateral_results[, "ChiSqP"]*35
collateral_results_final$Item <- collateral_results$Item
collateral_results_final$AIC <- round(collateral_results$DiffAIC, digits=2)
collateral_results_final[,"Chi-Sq"] <- round(collateral_results$ChiSq, digits=2)
for (i in 1:nrow(collateral_results)) {
	# Main
	if (collateral_results[i, "Main"] < .00001) { collateral_results_final[i, "Main-P"] <- "< .00001"
	} else if (collateral_results[i, "Main"] < .0001) { collateral_results_final[i, "Main-P"] <- "< .0001"
	} else if (collateral_results[i, "Main"] < .001) { collateral_results_final[i, "Main-P"] <- "< .001"
	} else if (collateral_results[i, "Main"] < .01) { collateral_results_final[i, "Main-P"] <- "< .01"
	} else if (collateral_results[i, "Main"] < .05) { collateral_results_final[i, "Main-P"] <- "< .05"
	} else if (collateral_results[i, "Main"] > .05) { collateral_results_final[i, "Main-P"] <- "> .05"
	}

	# Interaction
	if (collateral_results[i, "Interaction"] < .00001) { collateral_results_final[i, "Interaction-P"] <- "< .00001"
	} else if (collateral_results[i, "Interaction"] < .0001) { collateral_results_final[i, "Interaction-P"] <- "< .0001"
	} else if (collateral_results[i, "Interaction"] < .001) { collateral_results_final[i, "Interaction-P"] <- "< .001"
	} else if (collateral_results[i, "Interaction"] < .01) { collateral_results_final[i, "Interaction-P"] <- "< .01"
	} else if (collateral_results[i, "Interaction"] < .05) { collateral_results_final[i, "Interaction-P"] <- "< .05"
	} else if (collateral_results[i, "Interaction"] > .05) { collateral_results_final[i, "Interaction-P"] <- "> .05"
	}

	# ChiSqP
	if (collateral_results[i, "ChiSqP"] < .00001) { collateral_results_final[i, "Chi-Sq-P"] <- "< .00001"
	} else if (collateral_results[i, "ChiSqP"] < .0001) { collateral_results_final[i, "Chi-Sq-P"] <- "< .0001"
	} else if (collateral_results[i, "ChiSqP"] < .001) { collateral_results_final[i, "Chi-Sq-P"] <- "< .001"
	} else if (collateral_results[i, "ChiSqP"] < .01) { collateral_results_final[i, "Chi-Sq-P"] <- "< .01"
	} else if (collateral_results[i, "ChiSqP"] < .05) { collateral_results_final[i, "Chi-Sq-P"] <- "< .05"
	} else if (collateral_results[i, "ChiSqP"] > .05) { collateral_results_final[i, "Chi-Sq-P"] <- "> .05"
	}

	# ChiSqPBon
	if (collateral_results[i, "ChiSqPBon"] < .00001) { collateral_results_final[i, "Chi-Sq-P-Bon"] <- "< .00001"
	} else if (collateral_results[i, "ChiSqPBon"] < .0001) { collateral_results_final[i, "Chi-Sq-P-Bon"] <- "< .0001"
	} else if (collateral_results[i, "ChiSqPBon"] < .001) { collateral_results_final[i, "Chi-Sq-P-Bon"] <- "< .001"
	} else if (collateral_results[i, "ChiSqPBon"] < .01) { collateral_results_final[i, "Chi-Sq-P-Bon"] <- "< .01"
	} else if (collateral_results[i, "ChiSqPBon"] < .05) { collateral_results_final[i, "Chi-Sq-P-Bon"] <- "< .05"
	} else if (collateral_results[i, "ChiSqPBon"] > .05) { collateral_results_final[i, "Chi-Sq-P-Bon"] <- "> .05"
	}
}

collateral_results_final <- collateral_results_final[,c("Item", "AIC", "Chi-Sq", "Chi-Sq-P", "Chi-Sq-P-Bon", "Main-P", "Interaction-P")]
write.csv(collateral_results_final, "/home/butellyn/parentchild_psychopathology/data/forTables/collateral_dif_withgroups.csv", row.names=FALSE)

# Round #2
collateral_results2 <- read.csv("/home/butellyn/parentchild_psychopathology/data/collateral_dif2_2019-08-14.csv")
collateral_diffaic_hist2 <- ggplot(collateral_results2, aes(x=DiffAIC)) + geom_histogram() + theme_minimal() + 
	ggtitle("Collaterals: Differences in AIC (2)") + theme(plot.title = element_text(size=30), axis.title = element_text(size=25), axis.text = element_text(size=20)) #+ scale_x_continuous(limits=c(-130,10), breaks=seq(-130, 10, 10))

collateral_results2_final <- data.frame(matrix(NA, nrow=nrow(collateral_results2), ncol=ncol(collateral_results2)+1))
colnames(collateral_results2_final) <- c("Item", "Main-P", "Interaction-P", "AIC", "Chi-Sq", "Chi-Sq-P", "Chi-Sq-P-Bon") ##
collateral_results2[, "ChiSqPBon"] <- collateral_results2[, "ChiSqP"]*34
collateral_results2_final$Item <- collateral_results2$Item
collateral_results2_final$AIC <- round(collateral_results2$DiffAIC, digits=2)
collateral_results2_final[,"Chi-Sq"] <- round(collateral_results2$ChiSq, digits=2)
for (i in 1:nrow(collateral_results2)) {
	# Main
	if (collateral_results2[i, "Main"] < .00001) { collateral_results2_final[i, "Main-P"] <- "< .00001"
	} else if (collateral_results2[i, "Main"] < .0001) { collateral_results2_final[i, "Main-P"] <- "< .0001"
	} else if (collateral_results2[i, "Main"] < .001) { collateral_results2_final[i, "Main-P"] <- "< .001"
	} else if (collateral_results2[i, "Main"] < .01) { collateral_results2_final[i, "Main-P"] <- "< .01"
	} else if (collateral_results2[i, "Main"] < .05) { collateral_results2_final[i, "Main-P"] <- "< .05"
	} else if (collateral_results2[i, "Main"] > .05) { collateral_results2_final[i, "Main-P"] <- "> .05"
	}

	# Interaction
	if (collateral_results2[i, "Interaction"] < .00001) { collateral_results2_final[i, "Interaction-P"] <- "< .00001"
	} else if (collateral_results2[i, "Interaction"] < .0001) { collateral_results2_final[i, "Interaction-P"] <- "< .0001"
	} else if (collateral_results2[i, "Interaction"] < .001) { collateral_results2_final[i, "Interaction-P"] <- "< .001"
	} else if (collateral_results2[i, "Interaction"] < .01) { collateral_results2_final[i, "Interaction-P"] <- "< .01"
	} else if (collateral_results2[i, "Interaction"] < .05) { collateral_results2_final[i, "Interaction-P"] <- "< .05"
	} else if (collateral_results2[i, "Interaction"] > .05) { collateral_results2_final[i, "Interaction-P"] <- "> .05"
	}

	# ChiSqP
	if (collateral_results2[i, "ChiSqP"] < .00001) { collateral_results2_final[i, "Chi-Sq-P"] <- "< .00001"
	} else if (collateral_results2[i, "ChiSqP"] < .0001) { collateral_results2_final[i, "Chi-Sq-P"] <- "< .0001"
	} else if (collateral_results2[i, "ChiSqP"] < .001) { collateral_results2_final[i, "Chi-Sq-P"] <- "< .001"
	} else if (collateral_results2[i, "ChiSqP"] < .01) { collateral_results2_final[i, "Chi-Sq-P"] <- "< .01"
	} else if (collateral_results2[i, "ChiSqP"] < .05) { collateral_results2_final[i, "Chi-Sq-P"] <- "< .05"
	} else if (collateral_results2[i, "ChiSqP"] > .05) { collateral_results2_final[i, "Chi-Sq-P"] <- "> .05"
	}

	# ChiSqPBon
	if (collateral_results2[i, "ChiSqPBon"] < .00001) { collateral_results2_final[i, "Chi-Sq-P-Bon"] <- "< .00001"
	} else if (collateral_results2[i, "ChiSqPBon"] < .0001) { collateral_results2_final[i, "Chi-Sq-P-Bon"] <- "< .0001"
	} else if (collateral_results2[i, "ChiSqPBon"] < .001) { collateral_results2_final[i, "Chi-Sq-P-Bon"] <- "< .001"
	} else if (collateral_results2[i, "ChiSqPBon"] < .01) { collateral_results2_final[i, "Chi-Sq-P-Bon"] <- "< .01"
	} else if (collateral_results2[i, "ChiSqPBon"] < .05) { collateral_results2_final[i, "Chi-Sq-P-Bon"] <- "< .05"
	} else if (collateral_results2[i, "ChiSqPBon"] > .05) { collateral_results2_final[i, "Chi-Sq-P-Bon"] <- "> .05"
	}
}

collateral_results2_final <- collateral_results2_final[,c("Item", "AIC", "Chi-Sq", "Chi-Sq-P", "Chi-Sq-P-Bon", "Main-P", "Interaction-P")]
write.csv(collateral_results2_final, "/home/butellyn/parentchild_psychopathology/data/forTables/collateral_dif2_withgroups.csv", row.names=FALSE)

pdf(file="/home/butellyn/parentchild_psychopathology/plots/diffaic.pdf", width=16, height=12)
grid.arrange(proband_diffaic_hist, tableGrob(proband_results_final), ncol=2)
grid.arrange(proband_diffaic_hist2, tableGrob(proband_results2_final), ncol=2)
grid.arrange(collateral_diffaic_hist, tableGrob(collateral_results_final), ncol=2)
grid.arrange(collateral_diffaic_hist2, tableGrob(collateral_results2_final), ncol=2)
dev.off()

# Make table of combined tables
presf <- proband_results_final
for (i in 1:nrow(presf)) {
	# ChiSqPBon
	if (presf[i, "Chi-Sq-P-Bon"] %in% c("< .00001", "< .0001", "< .001", "< .01", "< .05")) {
		presf[i, "Chi-Sq"] <- paste0(presf[i, "Chi-Sq"], "*")
	}
}
presf <- presf[,c("Item", "Chi-Sq")]

presf2 <- proband_results2_final
for (i in 1:nrow(presf2)) {
	# ChiSqPBon
	if (presf2[i, "Chi-Sq-P-Bon"] %in% c("< .00001", "< .0001", "< .001", "< .01", "< .05")) {
		presf2[i, "Chi-Sq"] <- paste0(presf2[i, "Chi-Sq"], "*")
	}
}
presf2 <- presf2[,c("Item", "Chi-Sq")]

cresf <- collateral_results_final
for (i in 1:nrow(cresf)) {
	# ChiSqPBon
	if (cresf[i, "Chi-Sq-P-Bon"] %in% c("< .00001", "< .0001", "< .001", "< .01", "< .05")) {
		cresf[i, "Chi-Sq"] <- paste0(cresf[i, "Chi-Sq"], "*")
	}
}
cresf <- cresf[,c("Item", "Chi-Sq")]

cresf2 <- collateral_results2_final
for (i in 1:nrow(cresf2)) {
	# ChiSqPBon
	if (cresf2[i, "Chi-Sq-P-Bon"] %in% c("< .00001", "< .0001", "< .001", "< .01", "< .05")) {
		cresf2[i, "Chi-Sq"] <- paste0(cresf2[i, "Chi-Sq"], "*")
	}
}
cresf2 <- cresf2[,c("Item", "Chi-Sq")]

table3 <- data.frame(matrix(NA, nrow=37, ncol=5))
table3[1,] <- c("", "Probands", "", "Collaterals", "")
table3[2,] <- c("", "Chi-Sq #1", "Chi-Sq #2", "Chi-Sq #1", "Chi-Sq #2")
table3[3:37,1] <- as.character(presf$Item)
table3[3:37,2] <- presf[,"Chi-Sq"]
table3[3:37,3] <- c(presf2[1,"Chi-Sq"], "", presf2[2:34,"Chi-Sq"])
table3[3:37,4] <- cresf[,"Chi-Sq"]
table3[3:37,5] <- c(cresf2[1,"Chi-Sq"], "", cresf2[2:34,"Chi-Sq"])

write.csv(table3, "/home/butellyn/parentchild_psychopathology/data/forTables/Table3/table3.csv", row.names=FALSE)








################################ Convergent and Discriminant Validity ################################
library('plyr')

comb_df <- read.csv("/home/butellyn/parentchild_psychopathology/data/comb.csv")
# NOTE: Plotting residuals may be misleading. FIXED in Old Figure 2 to be coefficients from linear model
comb_df$internal_bifactor_P <- glm(comb_df$internal_bifactor_P ~ factor(comb_df$sex) + comb_df$ageAtClinicalAssess1)$residuals
comb_df$internal_bifactor_C <- glm(comb_df$internal_bifactor_C ~ factor(comb_df$sex) + comb_df$ageAtClinicalAssess1)$residuals

## Probands
proband_sui_mod <- read.csv("/home/butellyn/parentchild_psychopathology/data/mods/proband_sui_mod.csv")
proband_phys_mod <- read.csv("/home/butellyn/parentchild_psychopathology/data/mods/proband_phys_mod.csv")
proband_sex_mod <- read.csv("/home/butellyn/parentchild_psychopathology/data/mods/proband_sex_mod.csv")
proband_speed_mod <- read.csv("/home/butellyn/parentchild_psychopathology/data/mods/proband_speed_mod.csv")

# Suicidal ideation
proband_sui_mod2 <- data.frame(matrix(NA, nrow=4, ncol=3))
colnames(proband_sui_mod2) <- c("Variable", "Z-Score", "P(Z != 0)")
proband_sui_mod2$Variable <- c("(Intercept)", "Age", "Sex", "INT")
proband_sui_mod2[,"Z-Score"] <- round(proband_sui_mod[,"z.value"], digits=2)
for (i in 1:nrow(proband_sui_mod2)) {
	if (proband_sui_mod[i,"Pr...z.."] < .0001) { proband_sui_mod2[i,"P(Z != 0)"] <- "< .0001"
	} else if (proband_sui_mod[i,"Pr...z.."] < .001) { proband_sui_mod2[i,"P(Z != 0)"] <- "< .001"
	} else if (proband_sui_mod[i,"Pr...z.."] < .01) { proband_sui_mod2[i,"P(Z != 0)"] <- "< .01"
	} else if (proband_sui_mod[i,"Pr...z.."] < .05) { proband_sui_mod2[i,"P(Z != 0)"] <- "< .05"
	} else if (proband_sui_mod[i,"Pr...z.."] > .05) { proband_sui_mod2[i,"P(Z != 0)"] <- "> .05"
	}
}

comb_df$SUI002 <- as.character(comb_df$SUI002)
comb_df$SUI002 <- revalue(comb_df$SUI002, c("0"="No", "1"="Yes"))
comb_df$SUI002 <- factor(comb_df$SUI002)

proband_sui_plot <- ggplot(comb_df[!is.na(comb_df$SUI002),], aes_string(x="internal_bifactor_P", fill="SUI002", color="SUI002")) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5) + labs(fill = "Suicidal Ideation", color="Suicidal Ideation") +
	labs(title="Lifetime Internalizing Severity (Probands)", x="Internalizing Severity", y="# of Probands") +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), legend.position="top") 

write.csv(proband_sui_mod2, "/home/butellyn/parentchild_psychopathology/data/forTables/proband_sui_mod.csv", row.names=FALSE)


# Physical assault
proband_phys_mod2 <- data.frame(matrix(NA, nrow=4, ncol=3))
colnames(proband_phys_mod2) <- c("Variable", "Z-Score", "P(Z != 0)")
proband_phys_mod2$Variable <- c("(Intercept)", "Age", "Sex", "INT")
proband_phys_mod2[,"Z-Score"] <- round(proband_phys_mod[,"z.value"], digits=2)
for (i in 1:nrow(proband_phys_mod2)) {
	if (proband_phys_mod[i,"Pr...z.."] < .0001) { proband_phys_mod2[i,"P(Z != 0)"] <- "< .0001"
	} else if (proband_phys_mod[i,"Pr...z.."] < .001) { proband_phys_mod2[i,"P(Z != 0)"] <- "< .001"
	} else if (proband_phys_mod[i,"Pr...z.."] < .01) { proband_phys_mod2[i,"P(Z != 0)"] <- "< .01"
	} else if (proband_phys_mod[i,"Pr...z.."] < .05) { proband_phys_mod2[i,"P(Z != 0)"] <- "< .05"
	} else if (proband_phys_mod[i,"Pr...z.."] > .05) { proband_phys_mod2[i,"P(Z != 0)"] <- "> .05"
	}
}

comb_df$PTD003 <- as.character(comb_df$PTD003)
comb_df$PTD003 <- revalue(comb_df$PTD003, c("0"="No", "1"="Yes"))
comb_df$PTD003 <- factor(comb_df$PTD003)

proband_phys_plot <- ggplot(comb_df[!is.na(comb_df$PTD003),], aes_string(x="internal_bifactor_P", fill="PTD003", color="PTD003")) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5) + labs(fill = "Physical Assault", color="Physical Assault") +
	labs(title="Lifetime Internalizing Severity (Probands)", x="Internalizing Severity", y="# of Probands") +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), legend.position="top") 

write.csv(proband_phys_mod2, "/home/butellyn/parentchild_psychopathology/data/forTables/proband_phys_mod.csv", row.names=FALSE)

# Sexual assault
proband_sex_mod2 <- data.frame(matrix(NA, nrow=4, ncol=3))
colnames(proband_sex_mod2) <- c("Variable", "Z-Score", "P(Z != 0)")
proband_sex_mod2$Variable <- c("(Intercept)", "Age", "Sex", "INT")
proband_sex_mod2[,"Z-Score"] <- round(proband_sex_mod[,"z.value"], digits=2)
for (i in 1:nrow(proband_sex_mod2)) {
	if (proband_sex_mod[i,"Pr...z.."] < .0001) { proband_sex_mod2[i,"P(Z != 0)"] <- "< .0001"
	} else if (proband_sex_mod[i,"Pr...z.."] < .001) { proband_sex_mod2[i,"P(Z != 0)"] <- "< .001"
	} else if (proband_sex_mod[i,"Pr...z.."] < .01) { proband_sex_mod2[i,"P(Z != 0)"] <- "< .01"
	} else if (proband_sex_mod[i,"Pr...z.."] < .05) { proband_sex_mod2[i,"P(Z != 0)"] <- "< .05"
	} else if (proband_sex_mod[i,"Pr...z.."] > .05) { proband_sex_mod2[i,"P(Z != 0)"] <- "> .05"
	}
}

comb_df$PTD004 <- as.character(comb_df$PTD004)
comb_df$PTD004 <- revalue(comb_df$PTD004, c("0"="No", "1"="Yes"))
comb_df$PTD004 <- factor(comb_df$PTD004)

proband_sex_plot <- ggplot(comb_df[!is.na(comb_df$PTD004),], aes_string(x="internal_bifactor_P", fill="PTD004", color="PTD004")) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5) + labs(fill = "Sexual Assault", color="Sexual Assault") +
	labs(title="Lifetime Internalizing Severity (Probands)", x="Internalizing Severity", y="# of Probands") +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), legend.position="top") 

write.csv(proband_sex_mod2, "/home/butellyn/parentchild_psychopathology/data/forTables/proband_sex_mod.csv", row.names=FALSE)

# Motor speed
proband_speed_mod2 <- data.frame(matrix(NA, nrow=4, ncol=3))
colnames(proband_speed_mod2) <- c("Variable", "T-Value", "P(T != 0)")
proband_speed_mod2$Variable <- c("(Intercept)", "Age", "Sex", "INT")
proband_speed_mod2[,"T-Value"] <- round(proband_speed_mod[,"t.value"], digits=2)
for (i in 1:nrow(proband_speed_mod2)) {
	if (proband_speed_mod[i,"Pr...t.."] < .0001) { proband_speed_mod2[i,"P(T != 0)"] <- "< .0001"
	} else if (proband_speed_mod[i,"Pr...t.."] < .001) { proband_speed_mod2[i,"P(T != 0)"] <- "< .001"
	} else if (proband_speed_mod[i,"Pr...t.."] < .01) { proband_speed_mod2[i,"P(T != 0)"] <- "< .01"
	} else if (proband_speed_mod[i,"Pr...t.."] < .05) { proband_speed_mod2[i,"P(T != 0)"] <- "< .05"
	} else if (proband_speed_mod[i,"Pr...t.."] > .05) { proband_speed_mod2[i,"P(T != 0)"] <- "> .05"
	}
}

proband_speed_plot <- ggplot(comb_df, aes_string(y="internal_bifactor_P", x="mot_s_z")) + theme_minimal() +
	geom_point() + labs(title="Discriminant Validity", y="Internalizing Severity", x="Motor Speed") +
	theme(plot.title = element_text(size=25), axis.title = element_text(size=20), axis.text = element_text(size=15))

write.csv(proband_speed_mod2, "/home/butellyn/parentchild_psychopathology/data/forTables/proband_speed_mod.csv", row.names=FALSE)


## Collaterals
collateral_sui_mod <- read.csv("/home/butellyn/parentchild_psychopathology/data/mods/collateral_sui_mod.csv")
collateral_phys_mod <- read.csv("/home/butellyn/parentchild_psychopathology/data/mods/collateral_phys_mod.csv")
collateral_sex_mod <- read.csv("/home/butellyn/parentchild_psychopathology/data/mods/collateral_sex_mod.csv")
collateral_speed_mod <- read.csv("/home/butellyn/parentchild_psychopathology/data/mods/collateral_speed_mod.csv")

# Suicidal ideation
collateral_sui_mod2 <- data.frame(matrix(NA, nrow=4, ncol=3))
colnames(collateral_sui_mod2) <- c("Variable", "Z-Score", "P(Z != 0)")
collateral_sui_mod2$Variable <- c("(Intercept)", "Age", "Sex", "INT")
collateral_sui_mod2[,"Z-Score"] <- round(collateral_sui_mod[,"z.value"], digits=2)
for (i in 1:nrow(collateral_sui_mod2)) {
	if (collateral_sui_mod[i,"Pr...z.."] < .0001) { collateral_sui_mod2[i,"P(Z != 0)"] <- "< .0001"
	} else if (collateral_sui_mod[i,"Pr...z.."] < .001) { collateral_sui_mod2[i,"P(Z != 0)"] <- "< .001"
	} else if (collateral_sui_mod[i,"Pr...z.."] < .01) { collateral_sui_mod2[i,"P(Z != 0)"] <- "< .01"
	} else if (collateral_sui_mod[i,"Pr...z.."] < .05) { collateral_sui_mod2[i,"P(Z != 0)"] <- "< .05"
	} else if (collateral_sui_mod[i,"Pr...z.."] > .05) { collateral_sui_mod2[i,"P(Z != 0)"] <- "> .05"
	}
}

collateral_sui_plot <- ggplot(comb_df[!is.na(comb_df$SUI002),], aes_string(x="internal_bifactor_C", fill="SUI002", color="SUI002")) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5) + labs(fill = "Suicidal Ideation", color="Suicidal Ideation") +
	labs(title="Lifetime Internalizing Severity (Collaterals)", x="Internalizing Severity", y="# of Collaterals") +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), legend.position="top")

write.csv(collateral_sui_mod2, "/home/butellyn/parentchild_psychopathology/data/forTables/collateral_sui_mod.csv", row.names=FALSE)


# Physical assault
collateral_phys_mod2 <- data.frame(matrix(NA, nrow=4, ncol=3))
colnames(collateral_phys_mod2) <- c("Variable", "Z-Score", "P(Z != 0)")
collateral_phys_mod2$Variable <- c("(Intercept)", "Age", "Sex", "INT")
collateral_phys_mod2[,"Z-Score"] <- round(collateral_phys_mod[,"z.value"], digits=2)
for (i in 1:nrow(collateral_phys_mod2)) {
	if (collateral_phys_mod[i,"Pr...z.."] < .0001) { collateral_phys_mod2[i,"P(Z != 0)"] <- "< .0001"
	} else if (collateral_phys_mod[i,"Pr...z.."] < .001) { collateral_phys_mod2[i,"P(Z != 0)"] <- "< .001"
	} else if (collateral_phys_mod[i,"Pr...z.."] < .01) { collateral_phys_mod2[i,"P(Z != 0)"] <- "< .01"
	} else if (collateral_phys_mod[i,"Pr...z.."] < .05) { collateral_phys_mod2[i,"P(Z != 0)"] <- "< .05"
	} else if (collateral_phys_mod[i,"Pr...z.."] > .05) { collateral_phys_mod2[i,"P(Z != 0)"] <- "> .05"
	}
}

collateral_phys_plot <- ggplot(comb_df[!is.na(comb_df$PTD003),], aes_string(x="internal_bifactor_C", fill="PTD003", color="PTD003")) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5) + labs(fill = "Physical Assault", color="Physical Assault") +
	labs(title="Lifetime Internalizing Severity (Collaterals)", x="Internalizing Severity", y="# of Collaterals") +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), legend.position="top") 

write.csv(collateral_phys_mod2, "/home/butellyn/parentchild_psychopathology/data/forTables/collateral_phys_mod.csv", row.names=FALSE)

# Sexual assault
collateral_sex_mod2 <- data.frame(matrix(NA, nrow=4, ncol=3))
colnames(collateral_sex_mod2) <- c("Variable", "Z-Score", "P(Z != 0)")
collateral_sex_mod2$Variable <- c("(Intercept)", "Age", "Sex", "INT")
collateral_sex_mod2[,"Z-Score"] <- round(collateral_sex_mod[,"z.value"], digits=2)
for (i in 1:nrow(collateral_sex_mod2)) {
	if (collateral_sex_mod[i,"Pr...z.."] < .0001) { collateral_sex_mod2[i,"P(Z != 0)"] <- "< .0001"
	} else if (collateral_sex_mod[i,"Pr...z.."] < .001) { collateral_sex_mod2[i,"P(Z != 0)"] <- "< .001"
	} else if (collateral_sex_mod[i,"Pr...z.."] < .01) { collateral_sex_mod2[i,"P(Z != 0)"] <- "< .01"
	} else if (collateral_sex_mod[i,"Pr...z.."] < .05) { collateral_sex_mod2[i,"P(Z != 0)"] <- "< .05"
	} else if (collateral_sex_mod[i,"Pr...z.."] > .05) { collateral_sex_mod2[i,"P(Z != 0)"] <- "> .05"
	}
}

collateral_sex_plot <- ggplot(comb_df[!is.na(comb_df$PTD004),], aes_string(x="internal_bifactor_C", fill="PTD004", color="PTD004")) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5) + labs(fill = "Sexual Assault", color="Sexual Assault") +
	labs(title="Lifetime Internalizing Severity (Collaterals)", x="Internalizing Severity", y="# of Collaterals") +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), legend.position="top") 

write.csv(collateral_sex_mod2, "/home/butellyn/parentchild_psychopathology/data/forTables/collateral_sex_mod.csv", row.names=FALSE)

# Motor speed
collateral_speed_mod2 <- data.frame(matrix(NA, nrow=4, ncol=3))
colnames(collateral_speed_mod2) <- c("Variable", "T-Value", "P(T != 0)")
collateral_speed_mod2$Variable <- c("(Intercept)", "Age", "Sex", "INT")
collateral_speed_mod2[,"T-Value"] <- round(collateral_speed_mod[,"t.value"], digits=2)
for (i in 1:nrow(collateral_speed_mod2)) {
	if (collateral_speed_mod[i,"Pr...t.."] < .0001) { collateral_speed_mod2[i,"P(T != 0)"] <- "< .0001"
	} else if (collateral_speed_mod[i,"Pr...t.."] < .001) { collateral_speed_mod2[i,"P(T != 0)"] <- "< .001"
	} else if (collateral_speed_mod[i,"Pr...t.."] < .01) { collateral_speed_mod2[i,"P(T != 0)"] <- "< .01"
	} else if (collateral_speed_mod[i,"Pr...t.."] < .05) { collateral_speed_mod2[i,"P(T != 0)"] <- "< .05"
	} else if (collateral_speed_mod[i,"Pr...t.."] > .05) { collateral_speed_mod2[i,"P(T != 0)"] <- "> .05"
	}
}

collateral_speed_plot <- ggplot(comb_df, aes_string(y="internal_bifactor_C", x="mot_s_z")) + theme_minimal() +
	geom_point() + labs(title="Discriminant Validity", y="Internalizing Severity", x="Motor Speed") +
	theme(plot.title = element_text(size=25), axis.title = element_text(size=20), axis.text = element_text(size=15))

write.csv(collateral_speed_mod2, "/home/butellyn/parentchild_psychopathology/data/forTables/collateral_speed_mod.csv", row.names=FALSE)

# Combine tables
table4 <- data.frame(matrix(NA, nrow=18, ncol=6))
table4[1,] <- c("", "", "Probands", "", "Collaterals", "")
table4[2,] <- c("DV", "IV", "Statistic", "P", "Statistic", "P")
table4[3:18, 1] <- c("SUI", "", "", "", "PHYS", "", "", "", "SEX", "", "", "", "SPEED", "", "", "")
table4[3:6, 2:4] <- as.matrix(proband_sui_mod2)
table4[7:10, 2:4] <- as.matrix(proband_phys_mod2)
table4[11:14, 2:4] <- as.matrix(proband_sex_mod2)
table4[15:18, 2:4] <- as.matrix(proband_speed_mod2)
table4[3:6, 5:6] <- as.matrix(collateral_sui_mod2[,2:3])
table4[7:10, 5:6] <- as.matrix(collateral_phys_mod2[,2:3])
table4[11:14, 5:6] <- as.matrix(collateral_sex_mod2[,2:3])
table4[15:18, 5:6] <- as.matrix(collateral_speed_mod2[,2:3])
noterow <- c("Note: For categorical DVs, the statistic is a z-score and for continuous it is a t-value.", "", "", "", "", "")
table4 <- rbind(table4, noterow)

write.csv(table4, "/home/butellyn/parentchild_psychopathology/data/forTables/Table4/table4.csv", row.names=FALSE)


# Export plots
pdf(file="/home/butellyn/parentchild_psychopathology/plots/validity.pdf", width=10, height=6)
grid.arrange(proband_sui_plot, tableGrob(proband_sui_mod2), ncol=2)
grid.arrange(proband_phys_plot, tableGrob(proband_phys_mod2), ncol=2)
grid.arrange(proband_sex_plot, tableGrob(proband_sex_mod2), ncol=2)
grid.arrange(proband_speed_plot, tableGrob(proband_speed_mod2), ncol=2)
grid.arrange(collateral_sui_plot, tableGrob(collateral_sui_mod2), ncol=2)
grid.arrange(collateral_phys_plot, tableGrob(collateral_phys_mod2), ncol=2)
grid.arrange(collateral_sex_plot, tableGrob(collateral_sex_mod2), ncol=2)
grid.arrange(collateral_speed_plot, tableGrob(collateral_speed_mod2), ncol=2)
dev.off()

#### Old Figure 2 - November 5, 2019: No longer in the paper. Tested for mean differences here 
#### (controling for other metrics), but ran logistic regression for actual analysis, so 
#### too confusing to put in this plot. 
comb_df <- read.csv("/home/butellyn/parentchild_psychopathology/data/comb.csv")
proband_conv_df <- data.frame(matrix(NA, nrow=3, ncol=4))
colnames(proband_conv_df) <- c("Question", "Answer", "Coef", "SE")
proband_conv_df$Question <- c("SUI",  "PHYS", "SEX")
proband_conv_df$Answer <- c("Yes-No", "Yes-No", "Yes-No")
for (i in 1:3) {
	ques <- proband_conv_df[i, "Question"]
	ans <- proband_conv_df[i, "Answer"]
	if (ques == "SUI") { vari <- "SUI002"
	} else if (ques == "PHYS") { vari <- "PTD003"
	} else if (ques == "SEX") { vari <- "PTD004"
	}
	tmp_df <- comb_df[!is.na(comb_df[,vari]),]
	mod <- glm(tmp_df$internal_bifactor_P ~ tmp_df$ageAtClinicalAssess1 + tmp_df$sex + tmp_df[, vari])
	proband_conv_df[i, "Coef"] <- summary(mod)$coefficients[4,1]
	proband_conv_df[i, "SE"] <- summary(mod)$coefficients[4,2]
}

proband_conv_df$Question <- factor(proband_conv_df$Question, levels=c("SUI", "PHYS", "SEX"))
proband_conv_df$Answer <- factor(proband_conv_df$Answer)

subtit <- "INT ~  A + B*Age + C*Gender + D*Question ('Yes' = 1)"
proband_conv_plot <- ggplot(proband_conv_df, aes(x=Question, y=Coef, fill=Question)) + geom_bar(stat="identity", position=position_dodge()) + 
	theme_minimal() + scale_fill_manual(values=c("royalblue3", "mediumaquamarine", "maroon4")) +
	geom_errorbar(aes(ymin=Coef-2*SE, ymax=Coef+2*SE), width=.2, position=position_dodge(.9)) + ylab("Question Coefficient (95% CI)") +
	ggtitle("Probands: Convergent Validity", subtitle=subtit) + scale_y_continuous(limits=c(0, 1.25), breaks=seq(0, 1.25, .25)) +
	theme(plot.title = element_text(size=25), plot.subtitle = element_text(size=14), axis.title = element_text(size=20), axis.text = element_text(size=15), legend.position = "none", legend.title = element_blank()) 

collateral_conv_df <- data.frame(matrix(NA, nrow=3, ncol=4))
colnames(collateral_conv_df) <- c("Question", "Answer", "Coef", "SE")
collateral_conv_df$Question <- c("SUI",  "PHYS", "SEX")
collateral_conv_df$Answer <- c("Yes-No", "Yes-No", "Yes-No")
for (i in 1:3) {
	ques <- collateral_conv_df[i, "Question"]
	ans <- collateral_conv_df[i, "Answer"]
	if (ques == "SUI") { vari <- "SUI002"
	} else if (ques == "PHYS") { vari <- "PTD003"
	} else if (ques == "SEX") { vari <- "PTD004"
	}
	tmp_df <- comb_df[!is.na(comb_df[,vari]),]
	mod <- glm(tmp_df$internal_bifactor_P ~ tmp_df$ageAtClinicalAssess1 + tmp_df$sex + tmp_df[, vari])
	collateral_conv_df[i, "Coef"] <- summary(mod)$coefficients[4,1]
	collateral_conv_df[i, "SE"] <- summary(mod)$coefficients[4,2]
}

collateral_conv_df$Question <- factor(collateral_conv_df$Question, levels=c("SUI", "PHYS", "SEX"))
collateral_conv_df$Answer <- factor(collateral_conv_df$Answer)

subtit <- "INT ~  A + B*Age + C*Gender + D*Question ('Yes' = 1)"
collateral_conv_plot <- ggplot(collateral_conv_df, aes(x=Question, y=Coef, fill=Question)) + geom_bar(stat="identity", position=position_dodge()) + 
	theme_minimal() + scale_fill_manual(values=c("royalblue3", "mediumaquamarine", "maroon4")) +
	geom_errorbar(aes(ymin=Coef-2*SE, ymax=Coef+2*SE), width=.2, position=position_dodge(.9)) + ylab("Question Coefficient (95% CI)") +
	ggtitle("Collaterals: Convergent Validity", subtitle=subtit) + scale_y_continuous(limits=c(0, 1.25), breaks=seq(0, 1.25, .25)) +
	theme(plot.title = element_text(size=25), plot.subtitle = element_text(size=14), axis.title = element_text(size=20), axis.text = element_text(size=15), legend.position = "none", legend.title = element_blank()) 

#pdf(file="/home/butellyn/parentchild_psychopathology/plots/Figure2/figure2.pdf", width=14, height=6)
#grid.arrange(proband_conv_plot, collateral_conv_plot, ncol=2)
#dev.off()


################ Females worse internalizing than males (according to proband and collateral) ################

comb_df <- read.csv("/home/butellyn/parentchild_psychopathology/data/comb.csv")

### Correlation plots
subtit <- paste0("Female r=", round(cor(comb_df[comb_df$sex == "Female", "internal_bifactor_P"], comb_df[comb_df$sex == "Female", "internal_bifactor_C"]), digits=3), ", Male r=", round(cor(comb_df[comb_df$sex == "Male", "internal_bifactor_P"], comb_df[comb_df$sex == "Male", "internal_bifactor_C"]), digits=3))
corr_plot <- ggplot(comb_df, aes(x=internal_bifactor_P, y=internal_bifactor_C, fill=sex, color=sex)) + 
	geom_point(alpha=.5) + theme_minimal() + scale_x_continuous(limits=c(-1.25, 2.75), breaks=seq(-1.25, 2.75, .25)) + scale_y_continuous(limits=c(-1.25, 2.75), breaks=seq(-1.25, 2.75, .25)) +
	labs(title="Internalizing Severity", subtitle=subtit, x="Proband-Reported", y="Collateral-Reported", fill="Gender", color="Gender") + geom_abline(slope=1, intercept=0, linetype="dashed") +
	theme(plot.title = element_text(size=25), plot.subtitle = element_text(size=15), axis.title = element_text(size=20), axis.text = element_text(size=12), legend.title = element_text(size=15), legend.text = element_text(size=12), axis.text.x= element_text(angle=45)) 

pdf(file="/home/butellyn/parentchild_psychopathology/plots/JAACAP/supplement_figure2.pdf", width=7, height=6)
corr_plot
dev.off()

# Probands
proband_int_mod <- read.csv("/home/butellyn/parentchild_psychopathology/data/mods/proband_internalizing.csv")
proband_int_sex_table <- data.frame(matrix(NA, nrow=2, ncol=5))
colnames(proband_int_sex_table) <- c("Variable", "Df", "SS", "Iter", "P")
proband_int_sex_table$Variable <- c("Gender", "Residuals")
proband_int_sex_table$Df <- proband_int_mod$Df
proband_int_sex_table$SS <- round(proband_int_mod$R.Sum.Sq, digits=2)
proband_int_sex_table$Iter <- c("5000", "")
proband_int_sex_table$P <- c("< .00001", "")
rownames(proband_int_sex_table) <- NULL

proband_sex_plot <- ggplot(proband_df, aes(x=internal_bifactor, fill=sex, color=sex)) + theme_minimal() +
	theme(plot.title = element_text(size=25), axis.title = element_text(size=20), axis.text = element_text(size=15), legend.title = element_text(size=15), legend.text = element_text(size=12), legend.position="top") + geom_vline(xintercept=mean(proband_df[proband_df$sex == "Female", "internal_bifactor"]), linetype="longdash", color="#F8766D") + geom_vline(xintercept=mean(proband_df[proband_df$sex == "Male", "internal_bifactor"]), linetype="longdash", color="#00BFC4") +
	geom_histogram(position="identity", alpha=0.2) + annotation_custom(tableGrob(proband_int_sex_table, rows=NULL), ymin=400) +
	labs(title="Probands: Internalizing Severity", x="Internalizing Severity", y="# of Probands", fill="Gender", color="Gender") +
	scale_y_continuous(limits=c(0, 800))


# Collaterals
collateral_int_mod <- read.csv("/home/butellyn/parentchild_psychopathology/data/mods/collateral_internalizing.csv")
collateral_int_sex_table <- data.frame(matrix(NA, nrow=2, ncol=5))
colnames(collateral_int_sex_table) <- c("Variable", "Df", "SS", "Iter", "P")
collateral_int_sex_table$Variable <- c("Gender", "Residuals")
collateral_int_sex_table$Df <- collateral_int_mod$Df
collateral_int_sex_table$SS <- round(collateral_int_mod$R.Sum.Sq, digits=2)
collateral_int_sex_table$Iter <- c("5000", "")
collateral_int_sex_table$P <- c("< .00001", "")

collateral_sex_plot <- ggplot(collateral_df, aes(x=internal_bifactor, fill=sex, color=sex)) + theme_minimal() +
	theme(plot.title = element_text(size=25), axis.title = element_text(size=20), axis.text = element_text(size=15), legend.title = element_text(size=15), legend.text = element_text(size=12), legend.position="top") + geom_vline(xintercept=mean(collateral_df[collateral_df$sex == "Female", "internal_bifactor"]), linetype="longdash", color="#F8766D") + geom_vline(xintercept=mean(collateral_df[collateral_df$sex == "Male", "internal_bifactor"]), linetype="longdash", color="#00BFC4") +
	geom_histogram(position="identity", alpha=0.2) + annotation_custom(tableGrob(collateral_int_sex_table, rows=NULL), ymin=400) +
	labs(title="Collaterals: Internalizing Severity", x="Internalizing Severity", y="# of Collaterals", fill="Gender", color="Gender") +
	scale_y_continuous(limits=c(0, 800))

pdf(file="/home/butellyn/parentchild_psychopathology/plots/JAACAP/main_figure1.pdf", width=14, height=6)
grid.arrange(proband_sex_plot, collateral_sex_plot, ncol=2)
dev.off()

#### Disagreement trait


# Disagreement greater for one sex than another?
diff_int_sex <- read.csv("/home/butellyn/parentchild_psychopathology/data/mods/sex_diff_int.csv") 


diff_sex_plot <- ggplot(comb_df, aes(x=IntDiff, fill=sex, color=sex)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.2) + 
	labs(title="Difference Between Informants", subtitle=paste0("T=", round(diff_int_sex$T, digits=2), ", p < .00001"), x="Internalizing Severity", y="# of Pairs", fill="Gender", color="Gender") + geom_vline(xintercept=round(diff_int_sex$F.Mean, digits=3), linetype = "longdash", color="#F8766D") + geom_vline(xintercept=round(diff_int_sex$M.Mean, digits=3), linetype = "longdash", color="#00BFC4") + geom_vline(xintercept=0) +
	theme(plot.title = element_text(size=25), plot.subtitle = element_text(size=15), axis.title = element_text(size=20), axis.text = element_text(size=15), legend.title = element_text(size=15), legend.text = element_text(size=12), legend.position="top")

pdf(file="/home/butellyn/parentchild_psychopathology/plots/JAACAP/main_figure2.pdf", width=7, height=6)
diff_sex_plot
dev.off()

















################################ Disagreement items ################################

items <- c(paste0("ITEM00", 1:9), paste0("ITEM0", 10:35))

# Results
# Both
prop_disagree <- read.csv("/home/butellyn/parentchild_psychopathology/data/mods/prop_disagree.csv")
prop_disagree$ChiSq <- round(prop_disagree$ChiSq, digits=2)
prop_disagree$BON_P <- round(prop_disagree$BON_P, digits=5)
prop_disagree$P <- NA
for (i in 1:nrow(prop_disagree)) {
	if (prop_disagree[i, "BON_P"] < .00001) { prop_disagree[i, "P"] <- "< .00001" 
	} else if (prop_disagree[i, "BON_P"] < .0001) { prop_disagree[i, "P"] <- "< .0001" 
	} else if (prop_disagree[i, "BON_P"] < .001) { prop_disagree[i, "P"] <- "< .001" 
	} else if (prop_disagree[i, "BON_P"] < .01) { prop_disagree[i, "P"] <- "< .01" 
	} else if (prop_disagree[i, "BON_P"] < .05) { prop_disagree[i, "P"] <- "< .05" 
	} else if (prop_disagree[i, "BON_P"] > .05) { prop_disagree[i, "P"] <- "> .05" 
	}
}
prop_disagree$BON_P <- NULL
colnames(prop_disagree) <- c("Both: Item", "Chi-Sq", "P (Bon)")
write.csv(prop_disagree, file="/home/butellyn/parentchild_psychopathology/data/forTables/prop_disagree_clean.csv", row.names=FALSE)


# Females
prop_disagree_F <- read.csv("/home/butellyn/parentchild_psychopathology/data/mods/prop_disagree_F.csv")
prop_disagree_F$ChiSq <- round(prop_disagree_F$ChiSq, digits=2)
prop_disagree_F$BON_P <- round(prop_disagree_F$BON_P, digits=5)
prop_disagree_F$P <- NA
for (i in 1:nrow(prop_disagree_F)) {
	if (prop_disagree_F[i, "BON_P"] < .00001) { prop_disagree_F[i, "P"] <- "< .00001" 
	} else if (prop_disagree_F[i, "BON_P"] < .0001) { prop_disagree_F[i, "P"] <- "< .0001" 
	} else if (prop_disagree_F[i, "BON_P"] < .001) { prop_disagree_F[i, "P"] <- "< .001" 
	} else if (prop_disagree_F[i, "BON_P"] < .01) { prop_disagree_F[i, "P"] <- "< .01" 
	} else if (prop_disagree_F[i, "BON_P"] < .05) { prop_disagree_F[i, "P"] <- "< .05" 
	} else if (prop_disagree_F[i, "BON_P"] > .05) { prop_disagree_F[i, "P"] <- "> .05" 
	}
}
prop_disagree_F$BON_P <- NULL
colnames(prop_disagree_F) <- c("Females: Item", "Chi-Sq", "P (Bon)")
write.csv(prop_disagree_F, file="/home/butellyn/parentchild_psychopathology/data/forTables/prop_disagree_clean_F.csv", row.names=FALSE)

# Males
prop_disagree_M <- read.csv("/home/butellyn/parentchild_psychopathology/data/mods/prop_disagree_M.csv")
prop_disagree_M$ChiSq <- round(prop_disagree_M$ChiSq, digits=2)
prop_disagree_M$BON_P <- round(prop_disagree_M$BON_P, digits=5)
prop_disagree_M$P <- NA
for (i in 1:nrow(prop_disagree_M)) {
	if (prop_disagree_M[i, "BON_P"] < .00001) { prop_disagree_M[i, "P"] <- "< .00001" 
	} else if (prop_disagree_M[i, "BON_P"] < .0001) { prop_disagree_M[i, "P"] <- "< .0001" 
	} else if (prop_disagree_M[i, "BON_P"] < .001) { prop_disagree_M[i, "P"] <- "< .001" 
	} else if (prop_disagree_M[i, "BON_P"] < .01) { prop_disagree_M[i, "P"] <- "< .01" 
	} else if (prop_disagree_M[i, "BON_P"] < .05) { prop_disagree_M[i, "P"] <- "< .05" 
	} else if (prop_disagree_M[i, "BON_P"] > .05) { prop_disagree_M[i, "P"] <- "> .05" 
	}
}
prop_disagree_M$BON_P <- NULL
colnames(prop_disagree_M) <- c("Males: Item", "Chi-Sq", "P (Bon)")
write.csv(prop_disagree_M, file="/home/butellyn/parentchild_psychopathology/data/forTables/prop_disagree_clean_M.csv", row.names=FALSE)

pdf(file="/home/butellyn/parentchild_psychopathology/plots/disagree_sig_table.pdf", width=10, height=12)
grid.arrange(tableGrob(prop_disagree), tableGrob(prop_disagree_F), tableGrob(prop_disagree_M), ncol=3) # Figure out how to get rid of rownames in table
dev.off()



# Table
agree_df <- read.csv("/home/butellyn/parentchild_psychopathology/data/agreement.csv")

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

# Create a version of the table that has information about significance
agree_sig_table <- agree_table
i=3
j=1
for (item in items) {
	agree_sig_table[i, 1] <- item
	#### Males
	# PNCY
	if (prop_disagree_M[j, "P (Bon)"] %in% c("< .00001", "< .0001", "< .001", "< .01", "< .05")) {
		agree_sig_table[i, 5] <- paste0(agree_sig_table[i, 5], "*")
	}

	#### Females
	# PNCY
	if (prop_disagree_F[j, "P (Bon)"] %in% c("< .00001", "< .0001", "< .001", "< .01", "< .05")) {
		agree_sig_table[i, 9] <- paste0(agree_sig_table[i, 9], "*")
	}

	i=i+1
	j=j+1
}

notevec <- c("Note: The disagreement type proportions within sex are significantly different (Bonferroni-adjusted p < .05)", "", "", "", "", "", "", "", "")
agree_sig_table <- rbind(agree_sig_table, notevec)



write.csv(agree_sig_table, file="/home/butellyn/parentchild_psychopathology/data/forTables/Table5/table5.csv", row.names=FALSE)

