#cleaning
rm(list=ls())
cat("\014")

library(readr)
library(readxl)
library(ggplot2)
library(vars)
library(car)
library(zoo)
library(psych)
library(gridExtra)
library(tidyverse)
library(tseries)
library(urca)
library(forecast)
library(writexl)
library(stargazer)
library(modelsummary)
library(dplyr)
library(qpcR)

#setting the working directory
#setwd("~/EGYETEM/Mesterképzés-KözgazdaságiElemző/3. félév/Macroeconomic Policy Analysis")

data1 <- read_csv("Data/ert_bil_eur_m__custom_22915514_linear.csv")
data2 <- read_csv("Data/ert_eff_ic_m__custom_22915488_linear.csv")
data3 <- read_csv("Data/prc_hicp_iw__custom_22915504_linear.csv")
data4 <- read_csv("Data/prc_hicp_minr__custom_22915512_linear.csv")
data5 <- read_csv("Data/prc_hicp_minr__custom_22915551_linear.csv")
data6 <- read_csv("Data/sts_inpi_m__custom_22915523_linear.csv")
data7 <- read_csv("Data/sts_inpp_m__custom_22915529_linear.csv")
data8 <- read_csv("Data/sts_inpr_m__custom_22915545_linear.csv")
data9 <- read_csv("Data/une_rt_m__custom_22915548_linear.csv")

unique(data2$geo)
#------------------Constructing the Hungarian dataset-------------------
data1_HU <- data1[data1$currency=="Hungarian forint",]
data2_HU <- data2[data2$geo=="Hungary",]
data3_HU <- data3[data3$geo=="Hungary",]
data4_HU <- data4[data4$geo=="Hungary",]
data5_HU <- data5[data5$geo=="Hungary",]
data6_HU <- data6[data6$geo=="Hungary",]
data7_HU <- data7[data7$geo=="Hungary",]
data8_HU <- data8[data8$geo=="Hungary",]
data9_HU <- data9[data9$geo=="Hungary",]

data1_HU$TIME_PERIOD_2 <- data1_HU$TIME_PERIOD
data1_HU$TIME_PERIOD_2 <- paste0(data1_HU$TIME_PERIOD_2,"-01")

data2_HU$NEER_HUF <- data2_HU$OBS_VALUE

#Now I make my call and do the index=2015

data2_HU_filtered <- data2_HU[data2_HU$unit=="Index, 2015=100",]

data4_HU_filtered <- data4_HU[data4_HU$unit=="Index, 2015=100",]

data4_HU_filtered_1 <- data4_HU_filtered[data4_HU_filtered$coicop18=="Food and non-alcoholic beverages",]
data4_HU_filtered_1$Inflation_Foodandnonalcoholicbaverages <- data4_HU_filtered_1$OBS_VALUE

data4_HU_filtered_2 <- data4_HU_filtered[data4_HU_filtered$coicop18=="Food",]
data4_HU_filtered_2$Inflation_Food <- data4_HU_filtered_2$OBS_VALUE

data4_HU_filtered_3 <- data4_HU_filtered[data4_HU_filtered$coicop18=="Cereals and cereal products (ND)",]
data4_HU_filtered_3$Inflation_CerealsandCerealProducts <- data4_HU_filtered_3$OBS_VALUE

data4_HU_filtered_4 <- data4_HU_filtered[data4_HU_filtered$coicop18=="Cereals (ND)",]
data4_HU_filtered_4$Inflation_Cereals <- data4_HU_filtered_4$OBS_VALUE

data4_HU_filtered_5 <- data4_HU_filtered[data4_HU_filtered$coicop18=="Total",]
data4_HU_filtered_5$Inflation_Total <- data4_HU_filtered_5$OBS_VALUE

data4_HU_filtered_2 <- data4_HU_filtered_2[,c(7,11)]
data4_HU_filtered_3 <- data4_HU_filtered_3[,c(7,11)]
data4_HU_filtered_4 <- data4_HU_filtered_4[,c(7,11)]
data4_HU_filtered_5 <- data4_HU_filtered_5[,c(7,11)]

data4_HU_filtered_1 <- data4_HU_filtered_1 %>% map_df(rev)
data4_HU_filtered_2 <- data4_HU_filtered_2 %>% map_df(rev)
data4_HU_filtered_3 <- data4_HU_filtered_3 %>% map_df(rev)
data4_HU_filtered_4 <- data4_HU_filtered_4 %>% map_df(rev)
data4_HU_filtered_5 <- data4_HU_filtered_5 %>% map_df(rev)



#data4_HU_filtered_final <- cbind(data4_HU_filtered_1,data4_HU_filtered_2,data4_HU_filtered_3,data4_HU_filtered_4,data4_HU_filtered_5)

data4_HU_filtered_final <-qpcR:::cbind.na(data4_HU_filtered_1,data4_HU_filtered_2,data4_HU_filtered_3,data4_HU_filtered_4,data4_HU_filtered_5)

data4_HU_filtered_final <- data4_HU_filtered_final[,-c(12,14,16,18)]
data4_HU_filtered_final <- data4_HU_filtered_final %>% map_df(rev)

data_underconstruction <- data4_HU_filtered_final
data_underconstruction <- data_underconstruction %>% map_df(rev)

data7_HU_filtered <- data7_HU[data7_HU$unit=="Index, 2015=100",]
data7_HU_filtered$ProducerPriceIndexUndajusted <- data7_HU_filtered$OBS_VALUE
data7_HU_filtered <- data7_HU_filtered[,c(9,13)]

data7_HU_filtered <- rbind(data7_HU_filtered, data.frame(TIME_PERIOD = rep(NA, 32), ProducerPriceIndexUndajusted = rep(NA, 32)))
data7_HU_filtered <- data7_HU_filtered %>% map_df(rev)

data_underconstruction <- qpcR:::cbind.na(data_underconstruction,data7_HU_filtered)
data_underconstruction <- data_underconstruction[,-16]


data8_HU_filtered <- data8_HU[data8_HU$unit=="Index, 2015=100",]

data8_HU_filtered_1 <- data8_HU_filtered[data8_HU_filtered$s_adj=="Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)",]
data8_HU_filtered_2 <- data8_HU_filtered[data8_HU_filtered$s_adj=="Calendar adjusted data, not seasonally adjusted data",]
data8_HU_filtered_3 <- data8_HU_filtered[data8_HU_filtered$s_adj=="Seasonally and calendar adjusted data",]

data8_HU_filtered_11 <- data8_HU_filtered_1[data8_HU_filtered_1$nace_r2=="Mining and quarrying; manufacturing; electricity, gas, steam and air conditioning supply",]
data8_HU_filtered_12 <- data8_HU_filtered_1[data8_HU_filtered_1$nace_r2=="Manufacturing",]
data8_HU_filtered_13 <- data8_HU_filtered_1[data8_HU_filtered_1$nace_r2=="Electricity, gas, steam and air conditioning supply",]

data8_HU_filtered_11$ProductionVolumeUnadjusted_MiningQuarryingManufacturingElectricityGasSteamAC <- data8_HU_filtered_11$OBS_VALUE
data8_HU_filtered_12$ProductionVolumeUnadjusted_Manufacturing <- data8_HU_filtered_12$OBS_VALUE
data8_HU_filtered_13$ProductionVolumeUnadjusted_ElectricityGasSteamAC <- data8_HU_filtered_13$OBS_VALUE

data8_HU_filtered_11 <- data8_HU_filtered_11[,c(9,13)]
data8_HU_filtered_12 <- data8_HU_filtered_12[,c(9,13)]
data8_HU_filtered_13 <- data8_HU_filtered_13[,c(9,13)]

data8_HU_filtered_1_final <- cbind(data8_HU_filtered_11,data8_HU_filtered_12,data8_HU_filtered_13)

data8_HU_filtered_1_final <- data8_HU_filtered_1_final[,c(1,2,4,6)]

data8_HU_filtered_1_final <- rbind(data8_HU_filtered_1_final, data.frame(TIME_PERIOD = rep(NA, 32), ProductionVolumeUnadjusted_MiningQuarryingManufacturingElectricityGasSteamAC = rep(NA, 32),ProductionVolumeUnadjusted_Manufacturing = rep(NA, 32), ProductionVolumeUnadjusted_ElectricityGasSteamAC = rep(NA, 32)))
data8_HU_filtered_1_final <- data8_HU_filtered_1_final %>% map_df(rev)

data_underconstruction <- qpcR:::cbind.na(data_underconstruction,data8_HU_filtered_1_final)
data_underconstruction <- data_underconstruction[,-17]

data9_HU_filtered <- data9_HU[data9_HU$s_adj=="Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)",]
data9_HU_filtered_1 <- data9_HU_filtered[data9_HU_filtered$unit=="Percentage of population in the labour force",]
data9_HU_filtered_2 <- data9_HU_filtered[data9_HU_filtered$unit=="Thousand persons",]
data9_HU_filtered_2 <- data9_HU_filtered_2[-c(1:12),]

data9_HU_filtered_1$UnemploymentPercentageUnadjusted <- data9_HU_filtered_1$OBS_VALUE
data9_HU_filtered_2$UnemploymentThousandpplUnadjusted <- data9_HU_filtered_2$OBS_VALUE
data9_HU_filtered_1 <- data9_HU_filtered_1[,c(9,13)]
data9_HU_filtered_2 <- data9_HU_filtered_2[,c(9,13)]
data9_HU_filtered_final <- cbind(data9_HU_filtered_1,data9_HU_filtered_2)
data9_HU_filtered_final <- data9_HU_filtered_final[,-3]

data_underconstruction <- data_underconstruction %>% map_df(rev)
data_underconstruction <- qpcR:::cbind.na(data_underconstruction,data9_HU_filtered_final)
data_underconstruction <- data_underconstruction[,-20]
data_underconstruction <- data_underconstruction[,-c(5,8)]



data2_HU_filtered1 <- data2_HU_filtered[data2_HU_filtered$exch_rt=="Nominal effective exchange rate - 20 trading partners (euro area 2023-2025)",]
data2_HU_filtered2 <- data2_HU_filtered[data2_HU_filtered$exch_rt=="Nominal effective exchange rate - 21 trading partners (euro area from 2026)",]
data2_HU_filtered3 <- data2_HU_filtered[data2_HU_filtered$exch_rt=="Nominal effective exchange rate - 27 trading partners (European Union from 2020)",]
data2_HU_filtered4 <- data2_HU_filtered[data2_HU_filtered$exch_rt=="Nominal effective exchange rate - 37 trading partners (industrial countries)",]
data2_HU_filtered5 <- data2_HU_filtered[data2_HU_filtered$exch_rt=="Nominal effective exchange rate - 42 trading partners (industrial countries)",]

data2_HU_filtered1$NEER_HUF20 <- data2_HU_filtered1$OBS_VALUE
data2_HU_filtered2$NEER_HUF21 <- data2_HU_filtered2$OBS_VALUE
data2_HU_filtered3$NEER_HUF27 <- data2_HU_filtered3$OBS_VALUE
data2_HU_filtered4$NEER_HUF37 <- data2_HU_filtered4$OBS_VALUE
data2_HU_filtered5$NEER_HUF42 <- data2_HU_filtered5$OBS_VALUE

data2_HU_filtered1 <- data2_HU_filtered1[,c(7,12)]
data2_HU_filtered2 <- data2_HU_filtered2[,c(7,12)]
data2_HU_filtered3 <- data2_HU_filtered3[,c(7,12)]
data2_HU_filtered4 <- data2_HU_filtered4[,c(7,12)]
data2_HU_filtered5 <- data2_HU_filtered5[,c(7,12)]

data2_HU_filtered_final <- cbind(data2_HU_filtered1,data2_HU_filtered2,data2_HU_filtered3,data2_HU_filtered4,data2_HU_filtered5)
data2_HU_filtered_final <- data2_HU_filtered_final[,c(1,2,4,6,8,10)]

data2_HU_filtered_final <- data2_HU_filtered_final[-c(1:24),]

data_underconstruction <- qpcR:::cbind.na(data_underconstruction,data2_HU_filtered_final)
data_underconstruction <- data_underconstruction[,-20]

data1_HU_filetered_1 <- data1_HU[data1_HU$statinfo=="Average",]
data1_HU_filetered_2 <- data1_HU[data1_HU$statinfo=="Value at the end of the period",]

data1_HU_filetered_1$EURHUF_Average <- data1_HU_filetered_1$OBS_VALUE
data1_HU_filetered_2$EURHUF_Endofperiod <- data1_HU_filetered_2$OBS_VALUE
data1_HU_filetered_1 <- data1_HU_filetered_1[,c(7,12)]
data1_HU_filetered_2 <- data1_HU_filetered_2[,c(7,12)]

data1_HU_filetered_final <- cbind(data1_HU_filetered_1,data1_HU_filetered_2)

data1_HU_filetered_final <- data1_HU_filetered_final[,-3]
data1_HU_filetered_final <- data1_HU_filetered_final[-c(1:96),]

data_underconstruction <- qpcR:::cbind.na(data_underconstruction,data1_HU_filetered_final)
data_underconstruction <- data_underconstruction[,-25]


#write_xlsx(data_underconstruction,"MacroPolicyAnalysis_Data_HUN_Temp.xlsx")

#------------------Constructing the Slovakian dataset-------------------

data2_SK <- data2[data2$geo=="Slovakia",]
data3_SK <- data3[data3$geo=="Slovakia",]
data4_SK <- data4[data4$geo=="Slovakia",]
data5_SK <- data5[data5$geo=="Slovakia",]
data6_SK <- data6[data6$geo=="Slovakia",]
data7_SK <- data7[data7$geo=="Slovakia",]
data8_SK <- data8[data8$geo=="Slovakia",]
data9_SK <- data9[data9$geo=="Slovakia",]


data4_SK_filtered <- data4_SK[data4_SK$unit=="Index, 2015=100",]

data4_SK_filtered_1 <- data4_SK_filtered[data4_SK_filtered$coicop18=="Food and non-alcoholic beverages",]
data4_SK_filtered_1$Inflation_Foodandnonalcoholicbaverages <- data4_SK_filtered_1$OBS_VALUE

data4_SK_filtered_2 <- data4_SK_filtered[data4_SK_filtered$coicop18=="Food",]
data4_SK_filtered_2$Inflation_Food <- data4_SK_filtered_2$OBS_VALUE

data4_SK_filtered_3 <- data4_SK_filtered[data4_SK_filtered$coicop18=="Cereals and cereal products (ND)",]
data4_SK_filtered_3$Inflation_CerealsandCerealProducts <- data4_SK_filtered_3$OBS_VALUE

data4_SK_filtered_4 <- data4_SK_filtered[data4_SK_filtered$coicop18=="Cereals (ND)",]
data4_SK_filtered_4$Inflation_Cereals <- data4_SK_filtered_4$OBS_VALUE

data4_SK_filtered_5 <- data4_SK_filtered[data4_SK_filtered$coicop18=="Total",]
data4_SK_filtered_5$Inflation_Total <- data4_SK_filtered_5$OBS_VALUE

data4_SK_filtered_2 <- data4_SK_filtered_2[,c(7,11)]
data4_SK_filtered_3 <- data4_SK_filtered_3[,c(7,11)]
data4_SK_filtered_4 <- data4_SK_filtered_4[,c(7,11)]
data4_SK_filtered_5 <- data4_SK_filtered_5[,c(7,11)]

data4_SK_filtered_1 <- data4_SK_filtered_1 %>% map_df(rev)
data4_SK_filtered_2 <- data4_SK_filtered_2 %>% map_df(rev)
data4_SK_filtered_3 <- data4_SK_filtered_3 %>% map_df(rev)
data4_SK_filtered_4 <- data4_SK_filtered_4 %>% map_df(rev)
data4_SK_filtered_5 <- data4_SK_filtered_5 %>% map_df(rev)



data4_SK_filtered_final <-qpcR:::cbind.na(data4_SK_filtered_1,data4_SK_filtered_2,data4_SK_filtered_3,data4_SK_filtered_4,data4_SK_filtered_5)

data4_SK_filtered_final <- data4_SK_filtered_final[,-c(12,14,16,18)]
data4_SK_filtered_final <- data4_SK_filtered_final %>% map_df(rev)

data_ucs_SK <- data4_SK_filtered_final
data_ucs_SK <- data_ucs_SK %>% map_df(rev)


data7_SK_filtered <- data7_SK[data7_SK$unit=="Index, 2015=100",]
data7_SK_filtered$ProducerPriceIndexUndajusted <- data7_SK_filtered$OBS_VALUE
data7_SK_filtered <- data7_SK_filtered[,c(9,13)]


data7_SK_filtered <- rbind(data7_SK_filtered, data.frame(TIME_PERIOD = rep(NA, 32), ProducerPriceIndexUndajusted = rep(NA, 32)))
data7_SK_filtered <- data7_SK_filtered %>% map_df(rev)

data_ucs_SK <- qpcR:::cbind.na(data_ucs_SK,data7_SK_filtered)
data_ucs_SK <- data_ucs_SK[,-16]


data8_SK_filtered <- data8_SK[data8_SK$unit=="Index, 2015=100",]

data8_SK_filtered_1 <- data8_SK_filtered[data8_SK_filtered$s_adj=="Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)",]
data8_SK_filtered_2 <- data8_SK_filtered[data8_SK_filtered$s_adj=="Calendar adjusted data, not seasonally adjusted data",]
data8_SK_filtered_3 <- data8_SK_filtered[data8_SK_filtered$s_adj=="Seasonally and calendar adjusted data",]

data8_SK_filtered_11 <- data8_SK_filtered_1[data8_SK_filtered_1$nace_r2=="Mining and quarrying; manufacturing; electricity, gas, steam and air conditioning supply",]
data8_SK_filtered_12 <- data8_SK_filtered_1[data8_SK_filtered_1$nace_r2=="Manufacturing",]
data8_SK_filtered_13 <- data8_SK_filtered_1[data8_SK_filtered_1$nace_r2=="Electricity, gas, steam and air conditioning supply",]

data8_SK_filtered_11$ProductionVolumeUnadjusted_MiningQuarryingManufacturingElectricityGasSteamAC <- data8_SK_filtered_11$OBS_VALUE
data8_SK_filtered_12$ProductionVolumeUnadjusted_Manufacturing <- data8_SK_filtered_12$OBS_VALUE
data8_SK_filtered_13$ProductionVolumeUnadjusted_ElectricityGasSteamAC <- data8_SK_filtered_13$OBS_VALUE

data8_SK_filtered_11 <- data8_SK_filtered_11[,c(9,13)]
data8_SK_filtered_12 <- data8_SK_filtered_12[,c(9,13)]
data8_SK_filtered_13 <- data8_SK_filtered_13[,c(9,13)]

data8_SK_filtered_1_final <- cbind(data8_SK_filtered_11,data8_SK_filtered_12,data8_SK_filtered_13)

data8_SK_filtered_1_final <- data8_SK_filtered_1_final[,c(1,2,4,6)]

data8_SK_filtered_1_final <- rbind(data8_SK_filtered_1_final, data.frame(TIME_PERIOD = rep(NA, 32), ProductionVolumeUnadjusted_MiningQuarryingManufacturingElectricityGasSteamAC = rep(NA, 32),ProductionVolumeUnadjusted_Manufacturing = rep(NA, 32), ProductionVolumeUnadjusted_ElectricityGasSteamAC = rep(NA, 32)))
data8_SK_filtered_1_final <- data8_SK_filtered_1_final %>% map_df(rev)

data_ucs_SK <- qpcR:::cbind.na(data_ucs_SK,data8_SK_filtered_1_final)
data_ucs_SK <- data_ucs_SK[,-17]

data9_SK_filtered <- data9_SK[data9_SK$s_adj=="Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)",]
data9_SK_filtered_1 <- data9_SK_filtered[data9_SK_filtered$unit=="Percentage of population in the labour force",]
data9_SK_filtered_2 <- data9_SK_filtered[data9_SK_filtered$unit=="Thousand persons",]
data9_SK_filtered_2 <- data9_SK_filtered_2[-c(1:12),]

data9_SK_filtered_1$UnemploymentPercentageUnadjusted <- data9_SK_filtered_1$OBS_VALUE
data9_SK_filtered_2$UnemploymentThousandpplUnadjusted <- data9_SK_filtered_2$OBS_VALUE
data9_SK_filtered_1 <- data9_SK_filtered_1[,c(9,13)]
data9_SK_filtered_2 <- data9_SK_filtered_2[,c(9,13)]
#
data9_SK_filtered_1 <- rbind(data.frame(TIME_PERIOD = rep(NA, 24), UnemploymentPercentageUnadjusted = rep(NA, 24)),data9_SK_filtered_1)


data9_SK_filtered_final <- cbind(data9_SK_filtered_2,data9_SK_filtered_1)
data9_SK_filtered_final <- data9_SK_filtered_final[,-3]

data9_SK_filtered_final <- rbind(data9_SK_filtered_final,data.frame(TIME_PERIOD = rep(NA, 1), UnemploymentThousandpplUnadjusted = rep(NA, 1) ,UnemploymentPercentageUnadjusted = rep(NA, 1)))
data9_SK_filtered_final <- data9_SK_filtered_final %>% map_df(rev)


data_ucs_SK <- qpcR:::cbind.na(data_ucs_SK,data9_SK_filtered_final)
data_ucs_SK <- data_ucs_SK[,-20]
data_ucs_SK <- data_ucs_SK[,-c(5,8)]
data_ucs_SK <- data_ucs_SK[-c(358:368),]

data2_SK_filtered <- data2_SK[data2_SK$unit=="Index, 2015=100",]

data2_SK_filtered1 <- data2_SK_filtered[data2_SK_filtered$exch_rt=="Nominal effective exchange rate - 20 trading partners (euro area 2023-2025)",]
data2_SK_filtered2 <- data2_SK_filtered[data2_SK_filtered$exch_rt=="Nominal effective exchange rate - 21 trading partners (euro area from 2026)",]
data2_SK_filtered3 <- data2_SK_filtered[data2_SK_filtered$exch_rt=="Nominal effective exchange rate - 27 trading partners (European Union from 2020)",]
data2_SK_filtered4 <- data2_SK_filtered[data2_SK_filtered$exch_rt=="Nominal effective exchange rate - 37 trading partners (industrial countries)",]
data2_SK_filtered5 <- data2_SK_filtered[data2_SK_filtered$exch_rt=="Nominal effective exchange rate - 42 trading partners (industrial countries)",]

data2_SK_filtered1$NEER_SK20 <- data2_SK_filtered1$OBS_VALUE
data2_SK_filtered2$NEER_SK21 <- data2_SK_filtered2$OBS_VALUE
data2_SK_filtered3$NEER_SK27 <- data2_SK_filtered3$OBS_VALUE
data2_SK_filtered4$NEER_SK37 <- data2_SK_filtered4$OBS_VALUE
data2_SK_filtered5$NEER_SK42 <- data2_SK_filtered5$OBS_VALUE

data2_SK_filtered1 <- data2_SK_filtered1[,c(7,11)]
data2_SK_filtered2 <- data2_SK_filtered2[,c(7,11)]
data2_SK_filtered3 <- data2_SK_filtered3[,c(7,11)]
data2_SK_filtered4 <- data2_SK_filtered4[,c(7,11)]
data2_SK_filtered5 <- data2_SK_filtered5[,c(7,11)]

data2_SK_filtered_final <- cbind(data2_SK_filtered1,data2_SK_filtered2,data2_SK_filtered3,data2_SK_filtered4,data2_SK_filtered5)
data2_SK_filtered_final <- data2_SK_filtered_final[,c(1,2,4,6,8,10)]

data2_SK_filtered_final <- data2_SK_filtered_final[-c(1:35),]


data_ucs_SK <- data_ucs_SK %>% map_df(rev)
data_ucs_SK <- qpcR:::cbind.na(data_ucs_SK,data2_SK_filtered_final)
data_ucs_SK <- data_ucs_SK[,-20]

#write_xlsx(data_ucs_SK,"MacroPolicyAnalysis_Data_SK_Temp.xlsx")
