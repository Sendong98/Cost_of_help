library(readxl)
library(dplyr)
library(smatr)
library(tidyverse)
library(lubridate)
library(clock)
library(GGally)
library(dplyr)
library(ggpubr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(emmeans)
library(cowplot)
library(ggpubr)
library(DHARMa)
library(sjPlot)
# extract catch twice data
# load data
catch <- read_excel("C:\\Users\\ggb24\\Downloads\\Catch_data.xlsx")
status <- read_excel("C:\\Users\\ggb24\\Downloads\\usys_qBreedStatusByFieldPeriodPlusBreedGroup.xlsx")
catch_birth_date <- read_xlsx("C:\\Users\\ggb24\\Downloads\\catch_birth_date.xlsx")
df_sex <- read_xlsx("C:\\Users\\ggb24\\Downloads\\sex.xlsx")
helper_behaviour <- read_xlsx("C:\\Users\\ggb24\\Downloads\\tblNestWatchStatus_helper.xlsx")
joined_data <- left_join(catch, status, by = c("BirdID","FieldPeriodID"))
joined_data <- left_join(joined_data,df_sex,by="BirdID")
joined_data <- left_join(joined_data,catch_birth_date[,c("CatchID","BirthDate")],by="CatchID")
joined_data$age <- year(joined_data$OccasionDate)-year(joined_data$BirthDate)
# remove NAs
df <- joined_data[!is.na(joined_data$BodyMass)&!is.na(joined_data$Status)&joined_data$Status!="U"&joined_data$Island=="CN",]
# write.csv(df,"C:\\Users\\ggb24\\Downloads\\catch_status.csv")
df_catch_twice <- df %>%
  group_by(FieldPeriodID,BirdID) %>% filter( n() > 1 )
# give catch order number and days to end of season
df_order <- df_catch_twice %>% group_by(BirdID,FieldPeriodID) %>% arrange(as.POSIXct(ymd(OccasionDate)))%>% mutate(CatchOrder = 1:n())
df_order_diff <- df_order %>% group_by(BirdID,FieldPeriodID) %>% mutate(OrderDateDiff= abs(as.numeric(difftime(ymd(OccasionDate),ymd(PeriodEnd)), units = "days")))%>%mutate(OrderDateDiff=if_else(OrderDateDiff<0,0,OrderDateDiff))
# calculate body condition by using body mass and tarsus length
# population level average
# average tarsus length and bSMA
df_order_diff$RightTarsus <- as.numeric(df_order_diff$RightTarsus)
df_order_diff$LeftTarsus <- as.numeric(df_order_diff$LeftTarsus)
df_order_diff$BodyMass <- as.numeric(df_order_diff$BodyMass)
df_order_diff$CatchOrder <- factor(df_order_diff$CatchOrder)
df_order_diff$LastOfSexEstimate <- factor(df_order_diff$LastOfSexEstimate)
# remove NAs
df_order_diff <- df_order_diff %>% filter(!is.na(RightTarsus)|!is.na(LeftTarsus))
# merge left and right tarsus
df_order_diff <- df_order_diff %>% rowwise() %>% mutate(tarsus=mean(c(RightTarsus,LeftTarsus),na.rm = TRUE))
pop_mean_tarsu <- mean(df_order_diff$tarsus)
bSMA <- sma(log(BodyMass)~log(tarsus),data = df_order_diff)$groupsummary$Slope
# calculate body condition
df_order_diff <- df_order_diff %>% mutate(BodyCondition = BodyMass*((tarsus/pop_mean_tarsu)^bSMA))
# compare
par(mfrow = c(1, 2))
hist(df_order_diff$BodyMass)
hist(df_order_diff$BodyCondition)
par(mfrow = c(1, 1))
# Category of status
df_order_diff <- df_order_diff %>% mutate(Status=case_when(Status=="H"~"Helper",Status=="AB"~"NonHelper",Status=="BrF"|Status=="BrM"~"Breeder",TRUE~"Others"))
#plot different status
p1 <- ggplot(df_order_diff, aes(x = CatchOrder, y = BodyMass,fill= CatchOrder)) + 
  geom_boxplot(alpha = .5) +
  geom_line(aes(group = BirdID),linewidth = .5,alpha=.3) + 
  geom_point(size = .5,alpha=.5) +
  facet_wrap(~ Status)+labs( y = "BodyMass (g)")
p2 <- ggplot(df_order_diff, aes(x = CatchOrder, y = BodyCondition,fill= CatchOrder)) + 
  geom_boxplot(alpha = .5) +
  geom_line(aes(group = BirdID),linewidth = .5,alpha=.3) + 
  geom_point(size = .5,alpha=.5) +
  facet_wrap(~ Status)+theme(legend.position = "none")
ggarrange(p1, p2, labels = c("A", "B"),common.legend = TRUE)
# double check with behaviour
true_helper_id <- df_order_diff[df_order_diff$Status=="Helper",]$BirdID[df_order_diff[df_order_diff$Status=="Helper",]$BirdID %in%unique(helper_behaviour$BirdID)]
df_order_diff <- df_order_diff[df_order_diff$Status=="Helper"& df_order_diff$BirdID %in% true_helper_id,]
# paired plot
p3 <- ggplot(df_order_diff, aes(x = OrderDateDiff, y = BodyMass,colour = CatchOrder)) +  
  geom_point(size = 3, alpha = 0.5) + 
  geom_line(aes(group = interaction(BirdID,FieldPeriodID),colour = Ageclass),linewidth =1,alpha=.6) +
  labs(x = "Days to End of Season", y = "BodyMass (g)")+scale_x_reverse()
p4 <- ggplot(df_order_diff, aes(x = OrderDateDiff, y = BodyCondition,color=CatchOrder)) +  
  geom_point(size = 3, alpha = 0.5) + 
  geom_line(aes(group = interaction(BirdID,FieldPeriodID),colour = Ageclass),linewidth =1,alpha=.6) +
  labs(x = "Days to End of Season", y = "BodyCondition")+scale_x_reverse()
ggarrange(p3, p4, labels = c("A", "B"),common.legend = TRUE)
# linear mixed model of Bodymass
m1 <- lmer(BodyMass~CatchOrder*LastOfSexEstimate+OrderDateDiff+(1|BirdID)+(1|FieldPeriodID),data = df_order_diff)
summary(m1)
confint(m1)
# test assumptions
plot(m1)
qqnorm(resid(m1))
qqline(resid(m1))
dh1 <- simulateResiduals(m1, re.form = NULL)
plot(dh1)
plot_model(m1,type = "int")
emmeans(m1, pairwise ~ CatchOrder*LastOfSexEstimate)
# linear mixed model of Bodycondition
m2 <- lmer(BodyCondition~CatchOrder*LastOfSexEstimate+OrderDateDiff+(1|BirdID)+(1|FieldPeriodID),data = df_order_diff)
summary(m2)
confint(m2)
# test assumptions
plot(m2)
qqnorm(resid(m2))
qqline(resid(m2))
dh2 <- simulateResiduals(m2, re.form = NULL)
plot(dh2)
plot_model(m1,type = "int")
