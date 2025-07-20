######################################################################################                                                                         ##
## TEMPORAL PATTERNS OF YOUNG-OF-YEAR FISH EMIGRATION IN UPPER ST. LAWRENCE RIVER   ##
## COASTAL WETLANDS IN RELATION TO ENVIRONMENTAL VARIATION                          ##
## Script Created by Thornton Ritz - thornton.ritz@gmail.com                        ##
## GLM modeling Table 3, Figure 5, Figure 6                                         ##
## Data Source : 4 Onset U26 data loggers (2 per wetland) - Water TEMP and DO       ##
## Alexandria Bay NOAA Water Level Gauge Station ALXN6 - 8311062                    ##
## Data available upon request                                                      ##
######################################################################################

#### Load packages ####
library(MASS)
library(ggpubr)
library(tidyverse)
library(readxl)
library(writexl)
library(stats)
library(broom)
library(car)
library(lme4)
library(effects)
library(ggeffects)
library(patchwork)
library(lmtest)
library(Metrics)

options(scipen = 999, digits = 6)

#### Read in combined abiotic and biotic data for both locations ####
setwd("C:/Users/Thornton.A.Ritz/Desktop/Pub1/")
Model_DF<- read_excel("Ritz_Embayment_2021_Model.xlsx")
Model_DF$Location<- as.factor(Model_DF$Location)
Model_DF$Species<- as.factor(Model_DF$Species)
Model_DF$Period<-as.factor(Model_DF$Period)

## Filter out focal species ##
Model_DF <- Model_DF |>
  filter(Species %in% c("Largemouth Bass", "Smallmouth Bass", "Lepomis", 
                        "Leuciscidae", "Brown Bullhead"))

## Taxonomic Group Specific Model Dataframe Generation ##
Model_LEU<- Model_DF |>
  filter(Species %in% "Leuciscidae")
Model_BBH<- Model_DF |>
  filter(Species %in% "Brown Bullhead")
Model_LMB<- Model_DF |>
  filter(Species %in% "Largemouth Bass") 
Model_LEP<- Model_DF |>
  filter(Species %in% "Lepomis")
Model_SMB<- Model_DF |>
  filter(Species %in% "Smallmouth Bass")

## Taxonomic Grouping Specific Models ##
## Summary, Confidence Intervals, and McFaddens Adjusted R2 ##

######################################### Leuciscidae ####################################
Global_model_LEU<- glm.nb(Catch ~ DEPTH_mean + TEMP_mean + DO_mean + Period +
                              Location,
                          data = Model_LEU, maxit=1000)
summary(Global_model_LEU)

predicted_values <- fitted.values(Global_model_LEU) 
actual_values <-  Model_LEU$Catch
residuals <- actual_values - predicted_values

rmse <- sqrt(mean(residuals^2)) 
confint(Global_model_LEU)
with(summary(Global_model_LEU), 1 - deviance/null.deviance)

mae(actual_values, predicted_values)

######################################### BBH ####################################
Global_model_BBH<- glm.nb(Catch~ DEPTH_mean+TEMP_mean + DO_mean + Period+Location,
                          data = Model_BBH, maxit=1000)
summary(Global_model_BBH)

predicted_values <- fitted.values(Global_model_BBH) 
actual_values <-  Model_BBH$Catch
residuals <- actual_values - predicted_values

rmse <- sqrt(mean(residuals^2)) 
confint(Global_model_BBH)
with(summary(Global_model_BBH), 1 - deviance/null.deviance)
mae(actual_values, predicted_values)
######################################### LMB ####################################
Global_model_LMB<- glm.nb(Catch~ DEPTH_mean + TEMP_mean + DO_mean + Period+Location,
                          data = Model_LMB)
summary(Global_model_LMB)

predicted_values <- fitted.values(Global_model_LMB) 
actual_values <-  Model_LMB$Catch
residuals <- actual_values - predicted_values

rmse <- sqrt(mean(residuals^2)) 
confint(Global_model_LMB)
with(summary(Global_model_LMB), 1 - deviance/null.deviance)
mae(actual_values, predicted_values)
######################################### LEP ####################################
Global_model_LEP<- glm.nb(Catch~ DO_mean + TEMP_mean+ DEPTH_mean+ Period+Location,
                          data = Model_LEP)
summary(Global_model_LEP)

predicted_values <- fitted.values(Global_model_LEP) 
actual_values <-  Model_LEP$Catch
residuals <- actual_values - predicted_values

rmse <- sqrt(mean(residuals^2)) 
confint(Global_model_LEP)
with(summary(Global_model_LEP), 1 - deviance/null.deviance)

mae(actual_values, predicted_values)
######################################### SMB ####################################
Global_model_SMB<- glm.nb(Catch~ DO_mean + TEMP_mean+ DEPTH_mean + Period+Location,
                          data = Model_SMB)
summary(Global_model_SMB)

predicted_values <- fitted.values(Global_model_SMB) 
actual_values <-  Model_SMB$Catch
residuals <- actual_values - predicted_values

rmse <- sqrt(mean(residuals^2)) 
confint(Global_model_SMB)
with(summary(Global_model_SMB), 1 - deviance/null.deviance)
mae(actual_values, predicted_values)
#### Model Effects for plotting ##
## Values used to unscale and uncenter data for plots ##
## DO mean = 6.170    DO SD = 2.169 ##
## TEMP mean = 23.020 TEMP SD = 2.234 ##
## DEPTH mean = 74.568 SD = 0.041 ####

############################### LEU Effects Modeling ###########################
Effect_LEU_DO<- effect(term= "DO_mean", mod=Global_model_LEU)
Effect_LEU_TEMP<- effect(term= "TEMP_mean", mod=Global_model_LEU)

## Period ##
Effect_LEU_Period<- effect(term = "Period", mod = Global_model_LEU)
Effect_LEU_Period<- as.data.frame(Effect_LEU_Period) |>
  mutate(Species="Leuciscidae")
## WT ##
Effect_LEU_TEMP<- as.data.frame(Effect_LEU_TEMP) |> 
  mutate(Species="Leuciscidae")
Effect_LEU_TEMP<-Effect_LEU_TEMP |> mutate(Unscaled=(TEMP_mean*2.234)+23.020)
## DO ##
Effect_LEU_DO<- as.data.frame(Effect_LEU_DO) |>
  mutate(Species="Leuciscidae")
Effect_LEU_DO<- Effect_LEU_DO |> mutate(Unscaled=(DO_mean*2.169)+6.170)
## WL ##
Effect_LEU_WL<- effect(term = "DEPTH_mean", mod = Global_model_LEU)
Effect_LEU_WL<- as.data.frame(Effect_LEU_WL) |>
  mutate(Species="Leuciscidae")
Effect_LEU_WL<- Effect_LEU_WL |> mutate(Unscaled=(DEPTH_mean*0.041)+74.568)

############################### BBH Effects Modeling ###########################
Effect_BBH_DO<- effect(term= "DO_mean", mod=Global_model_BBH)
Effect_BBH_DO<- as.data.frame(Effect_BBH_DO) |>
  mutate(Species="Brown Bullhead")
Effect_BBH_DO<- Effect_BBH_DO |> mutate(Unscaled=(DO_mean*2.169)+6.170)

Effect_BBH_Period<- effect(term = "Period", mod = Global_model_BBH)
Effect_BBH_Period<- as.data.frame(Effect_BBH_Period) |>
  mutate(Species="Brown Bullhead")

Effect_BBH_TEMP<- effect(term="TEMP_mean", mod=Global_model_BBH)
Effect_BBH_TEMP<- as.data.frame(Effect_BBH_TEMP) |> 
  mutate(Species="Brown Bullhead")
Effect_BBH_TEMP<-Effect_BBH_TEMP |> mutate(Unscaled=(TEMP_mean*2.234)+23.020)

Effect_BBH_WL<- effect(term = "DEPTH_mean", mod = Global_model_BBH)
Effect_BBH_WL<- as.data.frame(Effect_BBH_WL) |>
  mutate(Species="Brown Bullhead")
Effect_BBH_WL<- Effect_BBH_WL |> mutate(Unscaled=(DEPTH_mean*0.041)+74.568)

############################### LMB Effects Modeling ###########################
Effect_LMB_DO<- effect(term= "DO_mean", mod=Global_model_LMB)
Effect_LMB_DO<- as.data.frame(Effect_LMB_DO) |>
  mutate(Species="Largemouth Bass") |>
  mutate(Unscaled=(DO_mean*2.169)+6.170)

Effect_LMB_Period<- effect(term = "Period", mod = Global_model_LMB)
Effect_LMB_Period<- as.data.frame(Effect_LMB_Period) |>
  mutate(Species="Largemouth Bass")

Effect_LMB_TEMP<- effect(term= "TEMP_mean", mod=Global_model_LMB)
Effect_LMB_TEMP<- as.data.frame(Effect_LMB_TEMP) |>
  mutate(Species="Largemouth Bass") |> 
  mutate(Unscaled=(TEMP_mean*2.234)+23.020)

Effect_LMB_WL<- effect(term= "DEPTH_mean", mod=Global_model_LMB)
Effect_LMB_WL<- as.data.frame(Effect_LMB_WL) |> 
  mutate(Unscaled=(DEPTH_mean*0.041)+74.568) |>
  mutate(Species="Largemouth Bass")

############################### LEP Effects Modeling ###########################
Effect_LEP_TEMP<- effect(term= "TEMP_mean", mod=Global_model_LEP)
Effect_LEP_TEMP<- as.data.frame(Effect_LEP_TEMP) |>
  mutate(Species="Lepomis") |> 
  mutate(Unscaled=(TEMP_mean*2.234)+23.020)

Effect_LEP_DO<- effect(term= "DO_mean", mod=Global_model_LEP)
Effect_LEP_DO<- as.data.frame(Effect_LEP_DO) |> 
  mutate(Species="Lepomis") |>
  mutate(Unscaled=(DO_mean*2.169)+6.170)

Effect_LEP_Period<- effect(term= "Period", mod=Global_model_LEP) 
Effect_LEP_Period<- as.data.frame(Effect_LEP_Period)  |>
mutate(Species="Lepomis")

Effect_LEP_WL<- effect(term= "DEPTH_mean", mod=Global_model_LEP)
Effect_LEP_WL<- as.data.frame(Effect_LEP_WL) |>
  mutate(Species="Lepomis")
Effect_LEP_WL<- Effect_LEP_WL |> mutate(Unscaled=(DEPTH_mean*0.041)+74.568)

############################### SMB Effects Modeling ###########################
Effect_SMB_DO<- effect(term= "DO_mean", mod=Global_model_SMB)
Effect_SMB_DO<- as.data.frame(Effect_SMB_DO) |>
  mutate(Species="Smallmouth Bass") |>
  mutate(Unscaled=(DO_mean*2.169)+6.170)

Effect_SMB_TEMP<- effect(term= "TEMP_mean", mod=Global_model_SMB)
Effect_SMB_TEMP<- as.data.frame(Effect_SMB_TEMP)|>
  mutate(Species="Smallmouth Bass") |> 
  mutate(Unscaled=(TEMP_mean*2.234)+23.020)

Effect_SMB_Period<- effect(term = "Period", mod = Global_model_SMB)
Effect_SMB_Period<- as.data.frame(Effect_SMB_Period) |>
  mutate(Species="Smallmouth Bass")

Effect_SMB_WL<- effect(term= "DEPTH_mean", mod=Global_model_SMB)
Effect_SMB_WL<- as.data.frame(Effect_SMB_WL)
Effect_SMB_WL<- Effect_SMB_WL |>
  mutate(Unscaled=(DEPTH_mean*0.041)+74.568) |>
  mutate(Species="Smallmouth Bass")

#### Period Effects Plots ####
                                                                                                       
Period<- bind_rows(Effect_LEU_Period, Effect_BBH_Period, Effect_LMB_Period,
                   Effect_LEP_Period, Effect_SMB_Period)
Period$Species<- factor(Period$Species, 
                    levels=c("Brown Bullhead", "Lepomis","Leuciscidae",
                             "Largemouth Bass","Smallmouth Bass"))
Figure_Period<- ggplot()+
  geom_ribbon(data=Period, aes(Period, ymin=lower, ymax=upper, group=Species), 
              fill="grey",
              alpha=0.6)+
  geom_point(data=Period, aes(Period, y=fit, group=Species))+
  geom_line(data=Period, aes(Period, y=fit, group=Species), linewidth=1)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=15),
                   axis.title = element_text(size=16),
                   strip.text = element_text(size=16),
                   legend.text=element_text(size=17),
                   strip.background = element_rect (fill = "white"),
                   plot.title = element_text(hjust=0.5, size=14, face="bold"))+
  ylab("Daily Fish Catch")+xlab("Time Period")+
  #scale_x_continuous(breaks = seq(1,3,1))+
  facet_wrap(~Species, nrow=3, scales = "free_y")
Figure_Period

ggsave("Figure_5.png", dpi = 300, height = 6, width = 8)

#### Water TEMP Effects Plots ####
TEMP<- bind_rows(Effect_LEU_TEMP,Effect_BBH_TEMP, Effect_LMB_TEMP, 
                 Effect_LEP_TEMP, Effect_SMB_TEMP)
TEMP$Species<- factor(TEMP$Species, 
                    levels=c("Brown Bullhead", "Lepomis","Leuciscidae",
                             "Largemouth Bass","Smallmouth Bass"))
Figure_TEMP<- ggplot()+
  geom_ribbon(data=TEMP, aes(Unscaled, ymin=lower, ymax=upper, group=Species), 
              fill="grey",
              alpha=0.6)+
  geom_point(data=TEMP, aes(Unscaled, y=fit, group=Species))+
  geom_line(data=TEMP, aes(Unscaled, y=fit, group=Species), linewidth=1)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=15), axis.title.y=element_blank(),
                   axis.title = element_text(size=16),
                   strip.text = element_text(size=16),
                   legend.text=element_text(size=17),
                   strip.background = element_rect (fill = "white"),
                   plot.title = element_text(hjust=0.5, size=14, face="bold"))+
  ylab("Daily YOY Fish Catch")+xlab("Daily Mean Water Temp. (°C)")+
  facet_wrap(~Species, scales = "free_y", nrow = 5)
Figure_TEMP

#### DO Effects Plots ####

DO<- bind_rows(Effect_LEU_DO, Effect_BBH_DO, Effect_LMB_DO, 
                 Effect_LEP_DO, Effect_SMB_DO)
DO$Species<- factor(DO$Species, 
                    levels=c("Brown Bullhead", "Lepomis","Leuciscidae",
                             "Largemouth Bass","Smallmouth Bass"))
Figure_DO<- ggplot()+
  geom_vline(xintercept = 3, color=c("#999999"), size=1.2, lty="dotted")+
  geom_ribbon(data=DO, aes(Unscaled, ymin=lower, ymax=upper, group=Species), 
              fill="grey",
              alpha=0.6)+
  geom_point(data=DO, aes(Unscaled, y=fit, group=Species))+
  geom_line(data=DO, aes(Unscaled, y=fit, group=Species), linewidth=1)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=15), axis.title.y=element_blank(),
                   axis.title = element_text(size=16),
                   strip.text = element_text(size=16),
                   legend.text=element_text(size=17),
                   strip.background = element_rect (fill = "white"),
                   plot.title = element_text(hjust=0.5, size=14, face="bold"))+
  ylab("Daily YOY Fish Catch")+xlab("Daily Mean DO (mg/l)")+
  facet_wrap(~Species, scales = "free_y", nrow = 5)
Figure_DO


#### Depth Effects Plots ####
Depth<- bind_rows(Effect_LEU_WL, Effect_BBH_WL, Effect_LEP_WL, Effect_LMB_WL, Effect_SMB_WL)
Depth$Species<- factor(Depth$Species, 
                    levels=c("Brown Bullhead", "Lepomis","Leuciscidae",
                             "Largemouth Bass","Smallmouth Bass"))

Figure_Depth<- ggplot()+
  geom_ribbon(data=Depth, aes(Unscaled, ymin=lower, ymax=upper, group=Species), 
              fill="grey",
              alpha=0.6)+
  geom_point(data=Depth, aes(Unscaled, y=fit, group=Species))+
  geom_line(data=Depth, aes(Unscaled, y=fit, group=Species), linewidth=1)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=15),
                   axis.title = element_text(size=16),
                   strip.text = element_text(size=16),
                   legend.text=element_text(size=17),
                   strip.background = element_rect (fill = "white"),
                   plot.title = element_text(hjust=0.5, size=14, face="bold"))+
  ylab("Daily YOY Fish Catch")+xlab("Daily Mean Water Level (m)")+
  facet_wrap(~Species, nrow=5, scales = "free_y")
Figure_Depth

Figure_6<- Figure_Depth + Figure_TEMP + Figure_DO 
Figure_6
ggsave("Figure_6.png", dpi = 300, height = 10, width = 12)

