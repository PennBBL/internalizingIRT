### This script determines collateral demographics and checks to see if GOA_df and kosha_df are in agreement
###
### Ellyn Butler
### November 1, 2019


################## Collateral Demographics ##################
kosha_df <- read.csv("/home/analysis/psycha1/pnc/PNC_GO1_GOASSESSDataArchiveNontext_DATA_2015-07-14_1157.csv")
GOA_df <- read.csv("/home/butellyn/parentchild_psychopathology/data/GOA_itemwise_for_reporter_agreement.csv")
clean_df <- read.csv("/home/butellyn/parentchild_psychopathology/data/cleanitems.csv")

# Limit the sample to those who are part of complete proband/collateral pairs
clean_df$Remove <- 0
for (i in 1:nrow(clean_df)) {
	bblid <- clean_df[i, "bblid"]
	if (length(clean_df[clean_df$bblid == bblid, "bblid"]) != 2) { clean_df[i, "Remove"] <- 1 }
}
clean_df <- clean_df[clean_df$Remove != 1,]
rownames(clean_df) <- 1:nrow(clean_df)

# Change bblid and informant to factors
clean_df$bblid <- factor(clean_df$bblid)
clean_df$informant <- factor(clean_df$informant)

# Change ITEM columns to numeric
items <- grep("ITEM", colnames(clean_df), value=TRUE)
for (item in items) { clean_df[,item] <- as.numeric(clean_df[,item]) }

clean_df$Remove <- NULL


# Merge Kosha's and Clean
names(kosha_df)[names(kosha_df) == "proband_bblid"] <- "bblid"
kosha_df <- kosha_df[kosha_df$interview_type == "MI", ]
clean_df <- clean_df[clean_df$informant == "collateral", ]

cleankosha_df <- merge(kosha_df, clean_df, by="bblid")
cleankosha_df <- cleankosha_df[,c("bblid", "col_rel", "interview_date1")]

# Dataframe with all collateral DOBs (or ages) and race?


# Interview dates
cleankosha_df$interview_date <- as.Date(cleankosha_df$interview_date1, tryFormats = c("%m/%d/%y"))
max(cleankosha_df$interview_date)
min(cleankosha_df$interview_date)


################## GOA_df & kosha_df in agreement? Yes ##################
kosha_df <- read.csv("/home/analysis/psycha1/pnc/PNC_GO1_GOASSESSDataArchiveNontext_DATA_2015-07-14_1157.csv")
GOA_df <- read.csv("/home/butellyn/parentchild_psychopathology/data/GOA_itemwise_for_reporter_agreement.csv")

names(GOA_df)[names(GOA_df) == "PROBAND_BBLID"] <- "proband_bblid"
names(GOA_df)[names(GOA_df) == "INTERVIEW_TYPE"] <- "interview_type"

combined_df <- merge(kosha_df, GOA_df)

colstocheck <- c("DEP001", "DEP002", "DEP004", "DEP006", "DEP012", "DEP013", "DEP014", "DEP015", "DEP016", "DEP017", "DEP018", "DEP019", "DEP020", "GAD001", "GAD002", "GAD003A", "GAD003B", "GAD003C", "GAD003D", "GAD003E", "GAD003F", "GAD011", "GAD011A", "GAD011B", "GAD012", "GAD015", "GAD016", "GAD017", "GAD018", "GAD019", "GAD020", "SOC001", "SOC002", "SOC003", "SOC004", "SOC005", "SOC007", "SOC008", "SOC016")

dfsagree <- data.frame(matrix(NA, nrow=length(colstocheck), ncol=2))
colnames(dfsagree) <- c("Item", "NumDisagree")

k=1
for (coli in colstocheck) {
	dfsagree[k, "Item"] <- coli
	lowcoli <- tolower(coli)
	agreevec <- combined_df[,coli] == combined_df[,lowcoli]
	agreevec <- agreevec[!is.na(agreevec)]
	dfsagree[k, "NumDisagree"] <- length(agreevec[agreevec == FALSE])
	k=k+1
}







