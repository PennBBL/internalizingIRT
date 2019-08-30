### This script produces descriptive statistics on the sample
###
### Ellyn Butler
### August 14, 2019


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

proband_df <- df[df$informant == "proband",]
rownames(proband_df) <- 1:nrow(proband_df)

# Age
proband_df$ageAtClinicalAssess1 <- proband_df$ageAtClinicalAssess1/12
mean(proband_df$ageAtClinicalAssess1)

# % Female
nrow(proband_df[proband_df$sex == "Female",])/nrow(proband_df)

# % White
nrow(proband_df[proband_df$race2 == 1,])/nrow(proband_df)

# % Black
nrow(proband_df[proband_df$race2 == 2,])/nrow(proband_df)
