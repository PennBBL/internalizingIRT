### This function combines DEP, GAD and SOC items using the cleaning functions, and puts in 
### "proband" versus "collateral"
### 
### Ellyn Butler
### July 18, 2019


# load packages 
library('car')
library('dplyr')
source('/home/butellyn/goassess_validation/scripts/anxiety/cleanSOC.R')
source('/home/butellyn/goassess_validation/scripts/anxiety/cleanGAD.R')
source('/home/butellyn/goassess_validation/scripts/depression/cleanDEP.R')


# Load the data 
df <- read.csv("/home/butellyn/parentchild_psychopathology/data/GOA_itemwise_for_reporter_agreement.csv") 
demo <- read.csv("/home/butellyn/age_prediction/data/n9498_demographics_go1_20161212.csv")

# Filter for dates starting ~February 2010 (Once GAD060 is fully missing)
df$SCR_STARTTIME <- as.Date(df$SCR_STARTTIME, "%m/%d/%Y")
df <- df %>% filter(SCR_STARTTIME > "2010-01-30")
#df <- df[!is.na(df$PROBAND_BBLID) | df$PROBAND_BBLID == "<NA>",]
rownames(df) <- 1:nrow(df)

# ------------------- DEP ------------------- #

##### Proband #####
depcols <- colnames(df)[grepl("DEP", colnames(df))][c(1:7, 10:20)]
dep_df <- df[df$INTERVIEW_TYPE == "MP",c("PROBAND_BBLID", depcols)]
names(dep_df)[names(dep_df) == 'PROBAND_BBLID'] <- 'bblid'

rownames(dep_df) <- 1:nrow(dep_df)

dep_df <- identifyBadDEP(dep_df)
dep_df <- dep_df[dep_df$Mistake != 1,]
rownames(dep_df) <- 1:nrow(dep_df)
dep_df <- fillInDEP(dep_df)
proband_dep_df <- remakeDEP(dep_df, informant="proband")

##### Collateral #####
depcols <- colnames(df)[grepl("DEP", colnames(df))][c(1:7, 10:20)]
dep_df <- df[df$INTERVIEW_TYPE == "MI",c("PROBAND_BBLID", depcols)]
names(dep_df)[names(dep_df) == 'PROBAND_BBLID'] <- 'bblid'

rownames(dep_df) <- 1:nrow(dep_df)

dep_df <- identifyBadDEP(dep_df)
dep_df <- dep_df[dep_df$Mistake != 1,]
rownames(dep_df) <- 1:nrow(dep_df)
dep_df <- fillInDEP(dep_df)
collateral_dep_df <- remakeDEP(dep_df, informant="collateral")

final_dep_df <- rbind(proband_dep_df, collateral_dep_df)

# ------------------- GAD ------------------- #

##### Proband #####
gadcols <- colnames(df)[grepl("GAD", colnames(df))][c(1:8, 10:12, 14:21, 24, 26:28, 13, 9, 25)]
gad_df <- df[df$INTERVIEW_TYPE == "MP",c("PROBAND_BBLID", gadcols)]
names(gad_df)[names(gad_df) == 'PROBAND_BBLID'] <- 'bblid'

rownames(gad_df) <- 1:nrow(gad_df)

gad_df <- identifyBadGAD(gad_df)
gad_df <- gad_df[gad_df$Mistake != 1,]
rownames(gad_df) <- 1:nrow(gad_df)
gad_df <- fillInGAD(gad_df)
proband_gad_df <- remakeGAD(gad_df, informant="proband")

##### Collateral #####
gadcols <- colnames(df)[grepl("GAD", colnames(df))][c(1:8, 10:12, 14:21, 24, 26:28, 13, 9, 25)]
gad_df <- df[df$INTERVIEW_TYPE == "MI",c("PROBAND_BBLID", gadcols)]
names(gad_df)[names(gad_df) == 'PROBAND_BBLID'] <- 'bblid'

rownames(gad_df) <- 1:nrow(gad_df)

gad_df <- identifyBadGAD(gad_df)
gad_df <- gad_df[gad_df$Mistake != 1,]
rownames(gad_df) <- 1:nrow(gad_df)
gad_df <- fillInGAD(gad_df)
collateral_gad_df <- remakeGAD(gad_df, informant="collateral")

final_gad_df <- rbind(proband_gad_df, collateral_gad_df)

# ------------------- SOC ------------------- #

##### Proband #####
soccols <- colnames(df)[grepl("SOC", colnames(df))][c(1:7, 10, 12:14, 16, 20, 11)]
soc_df <- df[df$INTERVIEW_TYPE == "MP",c("PROBAND_BBLID", soccols)]
names(soc_df)[names(soc_df) == 'PROBAND_BBLID'] <- 'bblid'

rownames(soc_df) <- 1:nrow(soc_df)

soc_df <- identifyBadSOC(soc_df)
soc_df <- soc_df[soc_df$Mistake != 1,]
rownames(soc_df) <- 1:nrow(soc_df)
soc_df <- fillInSOC(soc_df)
proband_soc_df <- remakeSOC(soc_df, informant="proband")

##### Collateral #####
soccols <- colnames(df)[grepl("SOC", colnames(df))][c(1:7, 10, 12:14, 16, 20, 11)]
soc_df <- df[df$INTERVIEW_TYPE == "MI",c("PROBAND_BBLID", soccols)]
names(soc_df)[names(soc_df) == 'PROBAND_BBLID'] <- 'bblid'

rownames(soc_df) <- 1:nrow(soc_df)

soc_df <- identifyBadSOC(soc_df)
soc_df <- soc_df[soc_df$Mistake != 1,]
rownames(soc_df) <- 1:nrow(soc_df)
soc_df <- fillInSOC(soc_df)
collateral_soc_df <- remakeSOC(soc_df, informant="collateral")

final_soc_df <- rbind(proband_soc_df, collateral_soc_df)

# ------------------- DEMO ------------------- #

final_df <- merge(demo, final_dep_df)
final_df <- merge(final_df, final_gad_df)
final_df <- merge(final_df, final_soc_df)

final_df$ageAtCnb1 <- NULL
final_df$handednessv2 <- NULL
final_df[final_df$sex == 2, "sex"] <- "Female"
final_df[final_df$sex == 1, "sex"] <- "Male"

write.csv(final_df, file="/home/butellyn/parentchild_psychopathology/data/cleanitems.csv", row.names=FALSE)















