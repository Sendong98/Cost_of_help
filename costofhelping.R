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
# extract catch twice data
# load data
catch <- read_excel("C:\\Users\\ggb24\\Downloads\\Catch_data.xlsx")
status <- read_excel("C:\\Users\\ggb24\\Downloads\\usys_qBreedStatusByFieldPeriodPlusBreedGroup.xlsx")
joined_data <- left_join(catch, status, by = c("BirdID","FieldPeriodID"))
# remove nas
df <- joined_data[!is.na(joined_data$BodyMass)&!is.na(joined_data$Status)&joined_data$Status!="U"&joined_data$Island=="CN",]
# write.csv(df,"C:\\Users\\ggb24\\Downloads\\catch_status.csv")
df_catch_twice <- df %>%
  group_by(FieldPeriodID,BirdID) %>% filter( n() > 1 )
# give catch order number and days to end of season
df_order <- df_catch_twice %>% group_by(BirdID,FieldPeriodID) %>% arrange(as.POSIXct(ymd(OccasionDate)))%>% mutate(CatchOrder = 1:n())
df_order_diff <- df_order %>% group_by(BirdID,FieldPeriodID) %>% mutate(OrderDateDiff= as.numeric(difftime(ymd(PeriodEnd),ymd(OccasionDate), units = "days")))%>%mutate(OrderDateDiff=if_else(OrderDateDiff<0,0,OrderDateDiff))

# calculate body condition by using body mass and tarsus length
# population level average
# average tarsus length and bSMA
catch_all <- read.csv("/Users/dongsen/Downloads/catch_status.csv")
# remove NAs
catch_all <- catch_all %>% filter(!is.na(BodyMass)) %>% filter(!is.na(RightTarsus)|!is.na(LeftTarsus))
# merge left and right tarsus
catch_all <- catch_all %>% rowwise() %>% mutate(tarsus=mean(c(RightTarsus,LeftTarsus),na.rm = TRUE))

catch_birth_date <- read_xlsx("C:\\Users\\ggb24\\Downloads\\catch_birth_date.xlsx")