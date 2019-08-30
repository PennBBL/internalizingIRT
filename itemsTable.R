### This script makes a csv of all of the items and skip logic
###
### Ellyn Butler
### August 26, 2019

# Load packages
library('gridExtra')
library('ggplot2')

item_df <- data.frame(matrix(NA, nrow=35, ncol=4))
colnames(item_df) <- c("Item", "Section", "Question", "Response Options")

item_df$Item <- c(paste0("ITEM00", 1:9), paste0("ITEM0", 10:35))
item_df$Section <- c(rep("DEP", 13), rep("GAD", 15), rep("SOC", 7))

item_df[1, "Question"] <- "Has there ever been a time when you felt sad or depressed most of the time?"
item_df[2, "Question"] <- "Has there ever been a time when you cried a lot, or felt like crying?"
item_df[3, "Question"] <- "Has there ever been a time when you felt grouchy, irritable, or in a bad mood most of the time; even little things would make you mad?"
item_df[4, "Question"] <- "Has there ever been a time when nothing was fun for you and you just weren't interested in anything?"
item_df[5, "Question"] <- "During this time, did you have problems falling asleep, waking up before you had to, or sleeping more than usual for you?*"
item_df[6, "Question"] <- "During this time, did you eat less or more, or feel less or more hungry than usual? (Did you lose or gain weight?)*"
item_df[7, "Question"] <- "During this time, did you have trouble sitting still or feel like you had to keep moving around OR did you move or think more slowly than usual?*"
item_df[8, "Question"] <- "During this time, did you feel really tired or have less energy than usual?*"
item_df[9, "Question"] <- "During this time, did you have problems keeping your mind on things (like homework), remembering things, thinking clearly, or making decisions?*"
item_df[10, "Question"] <- "During this time, did you blame yourself for bad things that happened or feel like you didn't really matter?*"
item_df[11, "Question"] <- "How much did your feeling (sad, grouchy, like nothing was fun) upset or bother you?**"
item_df[12, "Question"] <- "How much did your feeling (sad, grouchy, like nothing was fun) you told me about cause problems for you at home, at school or work, or with your family or friends?**"
item_df[13, "Question"] <- "Did you stay home from school or work because of the way you were feeling?**"
item_df[14, "Question"] <- "Have you ever been a worrier?"
item_df[15, "Question"] <- "Did you worry a lot more than most children/people your age?"
item_df[16, "Question"] <- "Do you worry a lot about your performance in school and/or sports?***"
item_df[17, "Question"] <- "Do you worry a lot about being on time?***"
item_df[18, "Question"] <- "Do you worry a lot about never making mistakes?***"
item_df[19, "Question"] <- "Do you worry a lot about your own health or the health of significant others?***"
item_df[20, "Question"] <- "Do you worry a lot about family situations (for example, divorce, finances)?***"
item_df[21, "Question"] <- "Do you worry a lot about things going on in the world (for example, war, terrorism, crime)?***"
item_df[22, "Question"] <- "When you worried the most, did you worry a lot about things either before they happened, or after they happened? Was it hard for you to stop yourself from worrying about these things?****"
item_df[23, "Question"] <- "When you worried the most, did you feel restless?****"
item_df[24, "Question"] <- "When you worried the most, did you feel fatigued (feeling tired)?****"
item_df[25, "Question"] <- "When you worried the most, did you experience concentration problems (trouble focusing or paying attention)?****"
item_df[26, "Question"] <- "When you worried the most, did you feel irritable (feeling easily annoyed)?****"
item_df[27, "Question"] <- "When you worried the most, did you experience muscle tension (feeling tense or tight muscles)?****"
item_df[28, "Question"] <- "When you worried the most, did you experience sleep disturbance (trouble sleeping)?****"
item_df[29, "Question"] <- "Was there ever a time in your life when you felt afraid or uncomfortable, or really really shy with people? Like when meeting new people, going to parties, or eating or drinking or writing or doing homework in front of others?"
item_df[30, "Question"] <- "Was there ever a time in your life when you felt afraid or uncomfortable talking on the telephone or with people your own age who you don't know very well?"
item_df[31, "Question"] <- "Was there ever a time in your life when you felt afraid or uncomfortable when you had to do something in nfront of a group of people, like speaking in class?"
item_df[32, "Question"] <- "Was there ever a time in your life when you felt afraid or uncomfortable acting, performing, giving a talk/speech, playing a sport or doing a musical performance, or taking an important test or exam (even though you studied enough)?"
item_df[33, "Question"] <- "Was there ever a time in your life when you felt afraid or uncomfortable because you were the center of attention and you were concerned something embarrassing might happen and you felt very afraid or felt uncomfortable?"
item_df[34, "Question"] <- "When you had to face (insert feared situation), did you try to avoid it or if you couldn't avoid it, did you feel very distressed when you faced it?*****"
item_df[35, "Question"] <- "Did this bother you more than most people your age?*****"

item_df[1, "Response Options"] <- "0-No, 1-Yes"
item_df[2, "Response Options"] <- "0-No, 1-Yes"
item_df[3, "Response Options"] <- "0-No, 1-Yes"
item_df[4, "Response Options"] <- "0-No, 1-Yes"
item_df[5, "Response Options"] <- "0-No, 1-Yes"
item_df[6, "Response Options"] <- "0-No, 1-Yes"
item_df[7, "Response Options"] <- "0-No, 1-Yes"
item_df[8, "Response Options"] <- "0-No, 1-Yes"
item_df[9, "Response Options"] <- "0-No, 1-Yes"
item_df[10, "Response Options"] <- "0-No, 1-Yes"
item_df[11, "Response Options"] <- "0-No Bother, 6-Extremely Serious Bother"
item_df[12, "Response Options"] <- "0-No Problems, 6-Extremely Serious Problems"
item_df[13, "Response Options"] <- "0-No, 1-Yes"
item_df[14, "Response Options"] <- "0-No, 1-Yes"
item_df[15, "Response Options"] <- "0-No, 1-Yes"
item_df[16, "Response Options"] <- "0-No, 1-Yes"
item_df[17, "Response Options"] <- "0-No, 1-Yes"
item_df[18, "Response Options"] <- "0-No, 1-Yes"
item_df[19, "Response Options"] <- "0-No, 1-Yes"
item_df[20, "Response Options"] <- "0-No, 1-Yes"
item_df[21, "Response Options"] <- "0-No, 1-Yes"
item_df[22, "Response Options"] <- "0-No & No, 1-Yes & No, 2-Yes & Yes"
item_df[23, "Response Options"] <- "0-No, 1-Yes"
item_df[24, "Response Options"] <- "0-No, 1-Yes"
item_df[25, "Response Options"] <- "0-No, 1-Yes"
item_df[26, "Response Options"] <- "0-No, 1-Yes"
item_df[27, "Response Options"] <- "0-No, 1-Yes"
item_df[28, "Response Options"] <- "0-No, 1-Yes"
item_df[29, "Response Options"] <- "0-No, 1-Yes"
item_df[30, "Response Options"] <- "0-No, 1-Yes"
item_df[31, "Response Options"] <- "0-No, 1-Yes"
item_df[32, "Response Options"] <- "0-No, 1-Yes"
item_df[33, "Response Options"] <- "0-No, 1-Yes"
item_df[34, "Response Options"] <- "0-No, 1-Yes"
item_df[35, "Response Options"] <- "0-No, 1-Yes"

# Add rows at end for skip info
# *Ask if 'yes' to both of the following questions: 'During that time when you were feeling the most (sad, grouchy, irritable, in a bad mood, had trouble having fun), did that/those feelings last most of the day?' AND 'Did you feel this way nearly every day for at least one week?'; **Ask if 'yes' to any of ITEM005-ITEM012; ***Ask if 'yes' to ITEM014 or ITEM015; ****Ask if 'yes' to one or more of ITEM016-ITEM021; *****Ask if 'yes' to at least two of ITEM029-ITEM033

item_df <- rbind(item_df, c("*Ask if 'yes' to both of the following questions: 'During that time when you were feeling the most (sad, grouchy, irritable, in a bad mood, had trouble having fun), did that/those feelings last most of the day?' AND 'Did you feel this way nearly every day for at least one week?'; **Ask if 'yes' to any of ITEM005-ITEM012; ***Ask if 'yes' to ITEM014 or ITEM015; ****Ask if 'yes' to one or more of ITEM016-ITEM021; *****Ask if 'yes' to at least two of ITEM029-ITEM033", "", "", ""))

write.csv(item_df, file="/home/butellyn/parentchild_psychopathology/data/forTables/Table1/table1.csv", row.names=FALSE)


