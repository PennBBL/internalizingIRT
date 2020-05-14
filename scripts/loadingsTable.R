### This script combines the loadings data from proband and collateral reports into a useful table
###
### Ellyn Butler
### August 26, 2019

# Load libraries
library('ggplot2')
library('gridExtra')

proband_bi <- read.csv("/home/butellyn/parentchild_psychopathology/data/loadings/proband_final_bi.csv")
proband_one <- read.csv("/home/butellyn/parentchild_psychopathology/data/loadings/proband_one.csv")
collateral_bi <-  read.csv("/home/butellyn/parentchild_psychopathology/data/loadings/collateral_final_bi.csv")
collateral_one <-  read.csv("/home/butellyn/parentchild_psychopathology/data/loadings/collateral_one.csv")

summary_df <- data.frame(matrix(NA, ncol=11, nrow=38))
summary_df[1, ] <- c("", "Probands", "", "", "", "", "Collaterals", "", "", "", "")
summary_df[2, ] <- c("Items", "One-Factor", "INT-Bifactor", "DEP-Bifactor", "GAD-Bifactor", "SOC-Bifactor", "One-Factor", "INT-Bifactor", "DEP-Bifactor", "GAD-Bifactor", "SOC-Bifactor")
summary_df[3:38, 1] <- c("ITEM001", "ITEM002 (F)", "ITEM002 (M)", "ITEM003", "ITEM004", "ITEM005", "ITEM006", "ITEM007", "ITEM008", "ITEM009", "ITEM010", "ITEM011", "ITEM012", "ITEM013", "ITEM014", "ITEM015", "ITEM016", "ITEM017", "ITEM018", "ITEM019", "ITEM020", "ITEM021", "ITEM022", "ITEM023", "ITEM024", "ITEM025", "ITEM026", "ITEM027", "ITEM028", "ITEM029", "ITEM030", "ITEM031", "ITEM032", "ITEM033", "ITEM034", "ITEM035")
summary_df[3:38, 2] <- round(proband_one$F1, digits=3)
summary_df[3:38, 3] <- round(proband_bi$G, digits=3)
summary_df[3:38, 4] <- round(proband_bi$S1, digits=3)
summary_df[3:38, 5] <- round(proband_bi$S2, digits=3)
summary_df[3:38, 6] <- round(proband_bi$S3, digits=3)
summary_df[3:38, 7] <- round(collateral_one$F1, digits=3)
summary_df[3:38, 8] <- round(collateral_bi$G, digits=3)
summary_df[3:38, 9] <- round(collateral_bi$S1, digits=3)
summary_df[3:38, 10] <- round(collateral_bi$S2, digits=3)
summary_df[3:38, 11] <- round(collateral_bi$S3, digits=3)

rownames(summary_df) <- NULL
colnames(summary_df) <- NULL

write.csv(summary_df, "/home/butellyn/parentchild_psychopathology/data/forTables/loadings.csv", row.names=FALSE)

pdf(file="/home/butellyn/parentchild_psychopathology/plots/loadingsComparison.pdf", width=15, height=12)
grid.arrange(tableGrob(summary_df))
dev.off()
