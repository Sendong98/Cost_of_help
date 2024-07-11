library(readxl)
library(tidyverse)
library(lubridate)
library(clock)
library(GGally)
library(dplyr)
library(ggpubr)
library(lme4)
library(lmerTest)
library(ggplot2)
df <- read.csv("/Users/dongsen/Downloads/twice_adults_new.csv")
df_all <- read.csv("/Users/dongsen/Downloads/catch_twice.csv")
str(df)
#write.csv(df,"/Users/dongsen/Downloads/twice_adults2.csv")
df2 <- df %>% group_by(BirdID,FieldPeriodID) %>% arrange(as.POSIXct(mdy(OccasionDate)))%>% mutate(CatchOrder = 1:n())
df3 <- df2 %>% group_by(BirdID,FieldPeriodID) %>% mutate(OrderDateDiff= as.numeric(difftime(mdy(PeriodEnd),mdy(OccasionDate), units = "days")))%>%mutate(OrderDateDiff=if_else(OrderDateDiff<0,0,OrderDateDiff))
write.csv(df3,"/Users/dongsen/Downloads/twice_adults3.csv")
hist(df3$BodyMass)
View(df3)
helper_behaviour <- read_xlsx("/Users/dongsen/Downloads/tblNestWatchStatus_helper.xlsx")
############################################################################
df_new <- read.csv("/Users/dongsen/Downloads/twice_adults_new.csv")
df_new$Year <- get_year(mdy(df_new$OccasionDate))
ggpairs(df_new[,-4])
df_sex <- read_xlsx("/Users/dongsen/Downloads/sex.xlsx")
df_new2 <- left_join(df_new,df_sex,by="BirdID")
View(df_new2)
df_new2$CatchOrder <- factor(df_new2$CatchOrder)
ggpaired(df_new2, x = "CatchOrder", y = "BodyMass", line.color = "gray", line.size = 0.4,
         palette = "jco",facet.by = "Status", short.panel.labs = FALSE,group="BirdID")
ggplot(df_new2, aes(x = CatchOrder, y = BodyMass)) + 
  geom_boxplot(aes(fill = CatchOrder), alpha = .5) +
  geom_line(aes(group = BirdID),alpha=.5) + 
  geom_point(size = 0.5,alpha=.5) + 
  facet_wrap(~ Status)
#set color
library(rijkspalette)
letter <- rijksPalette("Vermeer Letter")
letter
plot.format <- theme(plot.background=element_blank(),panel.grid=element_blank(),panel.background=element_blank(),panel.border=element_rect(color="black",linewidth=0.5,fill=NA),axis.line=element_blank(),axis.ticks=element_line(color="black",linewidth=0.5),axis.text=element_text(color="black",size=7),axis.title=element_text(color="black",size=7),plot.title=element_text(color="black",size=7),legend.background=element_blank(),legend.key=element_blank(),legend.text=element_text(color="black",size=7),legend.title=element_text(color="black",size=7))

##############################################################################
df_all$Year <- get_year(ymd(df_all$OccasionDate))
df_all2 <- left_join(df_all,df_sex,by="BirdID")
df_all3 <- df_all2 %>% group_by(BirdID,FieldPeriodID) %>% arrange(as.POSIXct(ymd(OccasionDate)))%>% mutate(CatchOrder = 1:n())
df_all3$CatchOrder <- factor(df_all3$CatchOrder)
write.csv(df_all3,"/Users/dongsen/Downloads/catch_twice_order.csv")
ggplot(df_all3, aes(x = CatchOrder, y = BodyMass,fill= CatchOrder)) + 
  geom_boxplot(alpha = .5) +
  geom_line(aes(group = BirdID),linewidth = .5,alpha=.3) + 
  geom_point(size = .5,alpha=.5) +
  facet_wrap(~ Status)
ggplot(df_all3[df_all3$Ageclass=="A"&!is.na(df_all3$Ageclass),], aes(x = CatchOrder, y = BodyMass,fill= CatchOrder)) + 
  geom_boxplot(alpha = .5) +
  geom_line(aes(group = BirdID),linewidth = .5,alpha=.3) + 
  geom_point(size = .5,alpha=.5) +
  facet_wrap(~ Status)
ggplot(df_all3[df_all3$Status=="H",], aes(x = CatchOrder, y = BodyMass,)) +  
  geom_point(size = 5, color="firebrick", alpha = 0.5) + 
  geom_line(aes(group = interaction(BirdID,FieldPeriodID),colour = Ageclass),linewidth =1.2,alpha=.6) +
  labs(x = "CatchOrder", y = "BodyMass (g)") + 
  facet_wrap(~ Status)
ggplot(df_all3, aes(x = CatchOrder, y = BodyMass,)) +  
  geom_point(size = 2, color="firebrick", alpha = 0.5) + 
  geom_line(aes(group = interaction(BirdID,FieldPeriodID),colour = Ageclass),linewidth =0.5,alpha=.6) +
  labs(x = "CatchOrder", y = "BodyMass (g)") + 
  facet_wrap(~ Status)
###############################################################################
m1 <- lmer(BodyMass~1+(1|BirdID),data = df_all3)
summary(m1)
v <- VarCorr(m1)
v1 <- as.data.frame(v)
v1$vcov[1] / (v1$vcov[1] + v1$vcov[2])
plot(m1)
################################################################################
df_all3 <- df_all3 %>% mutate(Status=case_when(Status))
m2 <- lmer(BodyMass~factor(Status)+(1|BirdID),data = df_all3)
summary(m2)
################################################################################
# calculate body condition by using body mass and tarsus length
library(smatr)
# population level average
# average tarsus length and bSMA
catch_all <- read.csv("/Users/dongsen/Downloads/catch_status.csv")
# remove NAs
catch_all <- catch_all %>% filter(!is.na(BodyMass)) %>% filter(!is.na(RightTarsus)|!is.na(LeftTarsus))
# merge left and right tarsus
catch_all <- catch_all %>% rowwise() %>% mutate(tarsus=mean(c(RightTarsus,LeftTarsus),na.rm = TRUE))


catch_birth_date <- read_xlsx("C:\\Users\\ggb24\\Downloads\\catch_birth_date.xlsx")
catch_birth_date <- read_xlsx("/Users/dongsen/Downloads/catch_birth_date.xlsx")


df_all_4 <- df_all3 %>% filter(!is.na(BodyMass)) %>% filter(!is.na(RightTarsus)|!is.na(LeftTarsus)) %>% rowwise() %>% mutate(tarsus=mean(c(RightTarsus,LeftTarsus),na.rm = TRUE))
pop_mean_tarsu <- mean(df_all_4$tarsus)
bSMA <- sma(log(BodyMass)~log(tarsus),data = df_all_4)$groupsummary$Slope
df_all_4 <- df_all_4 %>% mutate(condition = BodyMass*((tarsus/pop_mean_tarsu)^bSMA))
df_all_4 <- left_join(df_all_4,catch_birth_date[,c("CatchID","BirthDate")],by="CatchID" )
df_all_4$age <- year(df_all_4$OccasionDate)-year(df_all_4$BirthDate)
write.csv(df_all_4,"/Users/dongsen/Downloads/condition_caught_twice.csv")

df_all_4 <- read_csv("C:\\Users\\ggb24\\Downloads\\condition_caught_twice.csv")
hist(df_all_4$condition)
hist(df_all_4$BodyMass)
hist(df_all_4$age,breaks = 20)
true_helper_id <- df_all_4[df_all_4$Status=="H",]$BirdID[df_all_4[df_all_4$Status=="H",]$BirdID %in%unique(helper_behaviour$BirdID)]
df_all_4[df_all_4$Status=="H"& df_all_4$BirdID %in% true_helper_id,]

par(mfrow = c(1, 2))


ggplot(df_all_4[df_all_4$Status=="H",], aes(x = CatchOrder, y = condition,)) +  
  geom_point(size = 5, color="firebrick", alpha = 0.5) + 
  geom_line(aes(group = interaction(BirdID,FieldPeriodID),colour = Ageclass),linewidth =1.2,alpha=.6) +
  labs(x = "CatchOrder", y = "BodyCondition") + 
  facet_wrap(~ Status)


ggplot(df_all_4[df_all_4$Status=="H"& df_all_4$BirdID %in% true_helper_id,], aes(x = CatchOrder, y = BodyMass,)) +  
  geom_point(size = 5, color="firebrick", alpha = 0.5) + 
  geom_line(aes(group = interaction(BirdID,FieldPeriodID),colour = Ageclass),linewidth =1.2,alpha=.6) +
  labs(x = "CatchOrder", y = "BodyMass") + 
  facet_wrap(~ Status)


ggplot(df_all_4[df_all_4$Status=="H"& df_all_4$BirdID %in% true_helper_id,], aes(x = CatchOrder, y = condition,)) +  
  geom_point(size = 5, color="firebrick", alpha = 0.5) + 
  geom_line(aes(group = interaction(BirdID,FieldPeriodID),colour = Ageclass),linewidth =1.2,alpha=.6) +
  labs(x = "CatchOrder", y = "BodyCondition") + 
  facet_wrap(~ Status)

ggplot(df_all_4, aes(x = CatchOrder, y = condition,fill= CatchOrder)) + 
  geom_boxplot(alpha = .5) +
  geom_line(aes(group = interaction(BirdID,FieldPeriodID),colour = Ageclass),linewidth = .8,alpha=.3) + 
  geom_point(size = .5,alpha=.5) +
  facet_wrap(~ Status)

ggplot(df_all_4[df_all_4$Ageclass=="A"&!is.na(df_all_4$Ageclass),], aes(x = CatchOrder, y = condition,fill= CatchOrder)) + 
  geom_boxplot(alpha = .5) +
  geom_line(aes(group = interaction(BirdID,FieldPeriodID)),linewidth = .5,alpha=.3) + 
  geom_point(size = .5,alpha=.5) +
  facet_wrap(~ Status)
# linear mixed model
m1 <- lmer(BodyMass~LastOfSexEstimate+age+I(age^2)+CatchOrder+(1|BirdID)+(1|FieldPeriodID),data = df_all_4[df_all_4$Status=="H",])
summary(m1)
plot(m1)
qqnorm(resid(m1))
qqline(resid(m1))

m2 <- lmer(condition~LastOfSexEstimate+age+I(age^2)+CatchOrder+(1|BirdID)+(1|FieldPeriodID),data = df_all_4[df_all_4$Status=="H",])
summary(m2)
plot(m2)
qqnorm(resid(m2))
qqline(resid(m2))

m3 <- lmer(BodyMass~age+I(age^2)+CatchOrder*Status+LastOfSexEstimate+(1|BirdID)+(1|FieldPeriodID),data = df_all_4)
summary(m3)
plot(m3)
qqnorm(resid(m3))
qqline(resid(m3))

m4 <- lmer(condition~age+I(age^2)+CatchOrder*Status+LastOfSexEstimate+(1|BirdID)+(1|FieldPeriodID),data = df_all_4)
summary(m4)
plot(m4)
qqnorm(resid(m4))
qqline(resid(m4))
