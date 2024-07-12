library(readxl)
library(dplyr)
catch <- read_excel("C:\\Users\\ggb24\\Downloads\\Catch_data.xlsx")
status <- read_excel("C:\\Users\\ggb24\\Downloads\\usys_qBreedStatusByFieldPeriodPlusBreedGroup.xlsx")
joined_data <- left_join(catch, status, by = c("BirdID","FieldPeriodID"))
View(joined_data)
nrow(joined_data)
df <- joined_data[!is.na(joined_data$BodyMass)&!is.na(joined_data$Status),]
#write.csv(df,"C:\\Users\\ggb24\\Downloads\\catch_status.csv")
df <- df[df$Status!="U",]
View(df)
df_CN <- df[df$Island=="CN",]
View(df_CN)
df_new <- df_CN %>%
  group_by(FieldPeriodID,BirdID) %>% filter( n() > 1 )
df_new2 <- df_new %>%
  group_by(BreedGroupID) %>% filter( n_distinct(BirdID) > 1 )
View(df_new2)
write.csv(df_new2,"C:\\Users\\ggb24\\Downloads\\catch_twice_nest2.csv")
########################################################################
help_catch1 <- read_csv("C:\\Users\\ggb24\\Downloads\\catch_twice.csv")
help_catch2 <- help_catch[help_catch$Ageclass=="A"&!is.na(help_catch$Ageclass),]
help_catch1$Ageclass <- factor(help_catch1$Ageclass)
write.csv(help_catch2,"C:\\Users\\ggb24\\Downloads\\twice_adults.csv")
help_catch$order <- factor(help_catch$order)
str(help_catch)
boxplot(BodyMass~Ageclass,data = help_catch)
summary(lm(BodyMass~Ageclass,data = help_catch1))
ggplot(help_catch2, aes(x=BodyMass, fill=Status,colour = Status)) +
  geom_histogram(position="identity",binwidth = 0.2)
