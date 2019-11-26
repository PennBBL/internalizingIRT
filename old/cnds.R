### This script produces plots and results for CNDS' Sasquatch meeting (August 21, 2019)
###
### Ellyn Butler
### August 20, 2019


# Load packages
library('ggplot2')
library('gridExtra')
library('dplyr')

# Load the data
proband_df <- read.csv("/home/butellyn/parentchild_psychopathology/data/proband_2019-08-15.csv")
comb_df <- read.csv("/home/butellyn/parentchild_psychopathology/data/comb.csv")

df <- merge(proband_df, comb_df)

######## Make depressed versus suicidal plot
#ITEM001: Has there ever been a time when you felt sad or depressed most of the time?
#SUI002: Have you ever thought about killing yourself?

# Limit df to subjects with above some trait level, excluding the first item, on internalizing
df <- df[df$general_ITEM001_ex > 1,]
rownames(df) <- 1:nrow(df)
rate_df <- data.frame(matrix(NA, nrow=2, ncol=2))
colnames(rate_df) <- c("Item", "Rate")
rate_df$Item <- c("DEP", "SUI")
rate_df$Rate <- c(round(nrow(df[df$ITEM001 == 1,])/nrow(df), digits=2)*100, round(nrow(df[df$SUI002 == 1,])/nrow(df), digits=2)*100)


rate_plot <- ggplot(rate_df, aes(x=Item, y=Rate, color=Item, fill=Item)) + theme_minimal() +
	geom_bar(stat="identity") + scale_y_continuous(limits=c(0,100)) +
	labs( x="Items", y="% Yes", subtitle="DEP: 'Has there ever been a time when you\n\tfelt sad or depressed most of the time?'\nSUI: 'Have you ever thought about killing yourself?'") +
	theme(plot.subtitle = element_text(size=20), axis.text=element_text(size=22), axis.title=element_text(size=24,face="bold"))


pdf(file="/home/butellyn/parentchild_psychopathology/plots/cnds_depsui.pdf", width=10, height=10)
rate_plot
dev.off()



