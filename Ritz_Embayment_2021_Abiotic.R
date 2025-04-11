######################################################################################                                                                         ##
## TEMPORAL PATTERNS OF YOUNG-OF-YEAR FISH EMIGRATION IN UPPER ST. LAWRENCE RIVER   ##
## COASTAL WETLANDS IN RELATION TO ENVIRONMENTAL VARIATION                          ##
## Script Created by Thornton Ritz - thornton.ritz@gmail.com                        ##
## Abiotic Data Data Generation for Table 2, Fig. 3 & 4, and used for modeling      ##
## Data Source : 4 Onset U26 data loggers (2 per wetland) - Water TEMP and DO       ##
## Alexandria Bay NOAA Water Level Gauge Station ALXN6 - 8311062                    ##
## Data available upon request                                                      ##
######################################################################################

#### Packages used ####
library(zoo)
library(dplyr)
library(Rmisc)
library(ggpubr)
library(ggfortify)
library(tidyverse)
library(readxl)
library(stats)
library(car)
library(patchwork)
library(plotly)

#### Set working directory and read in data files ####
options(scipen = 999, digits = 6)
setwd("/Users/thorn/OneDrive/Desktop/Ritz_Embayment_2021/Manuscript_Data_Files/")

#### Read in Abiotic File (DO, TEMP, DEPTH) from 06/11/21 - 07/22/21 ####
## This file contains pooled daily logger data from each wetland ##

Abiotic<- read_excel("Ritz_Embayment_2021_Abiotic_ALL.xlsx")
Abiotic$Location<- as.factor(Abiotic$Location)
Abiotic$Date<- as.Date(Abiotic$Date)

Depth<- read_excel("Ritz_Embayment_2021_ABAY_WL.xlsx")
Depth <- Depth |>
  mutate(Date=format(Date, "%m-%d"))
Depth$Date<-as.Date(Depth$Date, format = "%m-%d")
Depth$Location<- as.factor(Depth$Location)
Depth$Year<- as.factor(Depth$Year)

## Daily Mean WL for Appendix Figure 1 ##
Depth_Appendix<- Depth |>
  filter(Year %in% c("2018", "2019", "2020", "2021"))

DEPTH_Appendix_Plot<- ggplot(Depth_Appendix, aes(Date, DEPTH_mean, group=Year, lty=Year))+
  geom_point(size=0.8)+
  geom_line(linewidth=1)+
  theme_bw()+ylab("Daily Mean Water Level (m)")+
  scale_x_date( minor_breaks = "5 days",
                date_labels = "%m/%d",
                breaks = seq(as.Date("2025-06-11"), 
                             as.Date("2025-07-22"), by="10 days"))+
  scale_y_continuous(breaks=seq(74.5,75.75,.2), limits=c(74.5,75.78))+
  scale_linetype_manual(values=c(1,3,6,2))+
  theme(
        legend.position ="right", legend.title = element_blank(),
        legend.text = element_text(size=12), axis.title.x = element_blank(),
        axis.text = element_text(size=13),
        axis.title = element_text(size=14))

DEPTH_Appendix_Plot
ggsave("Figure_1_Appendix.png", dpi=300, height = 6, width = 8)

## Daily mean WL plot for Figure 3 ##

Depth_Fig<- Depth |>
  filter(Year %in% c("2021"))
DEPTH_plot<- ggplot(Depth_Fig, aes(Date, DEPTH_mean))+
  geom_point(size=0.8)+
  geom_line(linewidth=1)+
  theme_bw()+ylab("Daily Mean Water Level (m)")+xlab("Date")+
  scale_x_date( minor_breaks = "5 days",
                date_labels = "%m/%d",
                breaks = seq(as.Date("2025-06-11"), as.Date("2025-07-22"), by="10 days"))+
  scale_y_continuous(breaks=seq(74.50,74.68,.04), limits=c(74.50,74.67))+
  scale_linetype_manual(values=c(1,3,6,4))+
  theme(legend.title = element_blank(), 
        legend.position =c(0.5,0.96),
        legend.text = element_text(size=12),
        axis.text = element_text(size=13),
        axis.title = element_text(size=14),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+guides(lty = guide_legend(nrow = 1))
DEPTH_plot+
  guides(colour="none",
         lty=guide_legend(order=1, nrow = 1))+plot_annotation(title = "Main River Gauge") &
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

ggsave("Figure_3.png", dpi=300, height = 6, width = 8)

#### WT and DO summary code for daily means for plotting ####
## Eel Bay Abiotic Data Filtering & Summary ##
Abiotic_DO_Eel<- Abiotic |>
  filter(Location %in% "Eel Bay") |>
  filter(Date <= "2021-07-22" & Date >= "2021-06-11") |> 
  select("DO Mean", "DO Max.", "DO Min.", "Location", "Date") |>
  gather(key = "Type", value = "Measurement", -Location, -Date)
Abiotic_DO_Eel$Type<- as.factor(Abiotic_DO_Eel$Type)

Abiotic_TEMP_Eel<- Abiotic |>
  filter(Date <= "2021-07-22" & Date >= "2021-06-11") |> 
  filter(Location %in% "Eel Bay") |>
  select("TEMP Mean", "TEMP Max.", "TEMP Min.", "Location", "Date") |>
  gather(key = "Type", value = "Measurement", -Location, -Date)
Abiotic_TEMP_Eel$Type<- as.factor(Abiotic_TEMP_Eel$Type)

## Eel Bay Plotting for Figure 4 ##

# Daily mean DO  #
DO_Eel_plot<- ggplot(Abiotic_DO_Eel, aes(Date, Measurement, 
                  group=Type, lty=Type))+
  geom_point(size=0.8)+
  geom_line(linewidth=1.2)+
  scale_linetype_manual(values=c("dotted","solid","twodash"))+
  theme_bw()+ylab("DO (mg/l)")+xlab("Date")+
  scale_y_continuous(breaks=seq(0,20,4), limits=c(0,22))+
  scale_x_date(breaks = seq(as.Date("2021-06-11"), as.Date("2021-07-22"), by="10 days"),
               date_labels = "%m/%d")+
  theme(legend.title = element_blank(), 
        legend.position.inside =c(0.5,0.91), 
        legend.position = "inside",
        legend.text = element_text(size=12), 
        axis.text = element_text(size=13),
        axis.title = element_text(size=14),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+guides(lty = guide_legend(nrow = 1))
  
DO_Eel_plot

# Daily mean WT # 
TEMP_Eel_plot<- ggplot(Abiotic_TEMP_Eel, aes(Date, Measurement, group=Type, lty=Type)) +
  geom_point(size=1.2)+
  geom_line(linewidth=1)+
  scale_linetype_manual(values=c("dotted","solid","twodash"))+
  theme_bw()+ylab("Water Temp. (°C)")+xlab("Date")+
  scale_y_continuous(breaks=seq(14,34,4), limits=c(14,36))+
  scale_x_date(breaks = seq(as.Date("2021-06-11"), as.Date("2021-07-22"), by="10 days"),date_labels = "%m/%d")+
  theme(legend.title = element_blank(), 
        legend.position.inside =c(0.5,0.91), 
        legend.position = "inside",
        legend.text = element_text(size=12),
        axis.text = element_text(size=13), axis.text.x = element_blank(),
        axis.title = element_text(size=14),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+guides(lty = guide_legend(nrow = 1))
TEMP_Eel_plot

## Combine Eel WT and DO Plots ##
EEL<-  TEMP_Eel_plot / DO_Eel_plot +
  guides(colour="none",
               lty=guide_legend(order=1, nrow = 1))+plot_annotation(title = "Eel Bay") &
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))
EEL

## Flynn Bay Filtering & Summary ## 
Abiotic_DO_Flynn<- Abiotic |>
  filter(Date <= "2021-07-22" & Date >= "2021-06-11") |> 
  filter(Location %in% "Flynn Bay") |>
  select("DO Mean", "DO Max.", "DO Min.", "Location", "Date") |>
  gather(key = "Type", value = "Measurement", -Location, -Date)
Abiotic_DO_Flynn$Type<- as.factor(Abiotic_DO_Flynn$Type)

Abiotic_TEMP_Flynn<- Abiotic |>
  filter(Date <= "2021-07-22" & Date >= "2021-06-11") |> 
  filter(Location %in% "Flynn Bay") |>
  select("TEMP Mean", "TEMP Max.", "TEMP Min.", "Location", "Date") |>
  gather(key = "Type", value = "Measurement", -Location, -Date)
Abiotic_TEMP_Flynn$Type<- as.factor(Abiotic_TEMP_Flynn$Type)

## Flynn Bay Plotting for Figure 4 ##

# Daily mean DO #
DO_Flynn_plot<- ggplot(Abiotic_DO_Flynn, aes(Date, Measurement, 
                                         group=Type, lty=Type))+
  geom_point(size=1.2)+
  geom_line(linewidth=1)+
  scale_linetype_manual(values=c("dotted","solid","twodash"))+
  theme_bw()+ylab("DO (mg/l)")+xlab("Date")+
  scale_y_continuous(breaks=seq(0,20,4), limits=c(0,22))+
  scale_x_date(breaks = seq(as.Date("2021-06-11"), as.Date("2021-07-22"), by="10 days"), 
               date_labels = "%m/%d")+
  theme(legend.title = element_blank(), 
        legend.position =c(0.5,0.91), 
        legend.text = element_text(size=12),
        axis.text = element_text(size=13),
        axis.title = element_text(size=14),
        axis.title.x = element_blank())+guides(lty = guide_legend(nrow = 1))
DO_Flynn_plot

# Daily mean WT #
TEMP_Flynn_plot<- ggplot(Abiotic_TEMP_Flynn, aes(Date, Measurement, 
                     group=Type, lty=Type))+
  geom_point(size=1.2)+
  geom_line(linewidth=1)+
  scale_linetype_manual(values=c("dotted","solid","twodash"))+
  theme_bw()+ylab("Water Temp. (°C)")+xlab("Date")+
  scale_y_continuous(breaks=seq(14,34,4), limits=c(14,36))+
  scale_x_date(breaks = seq(as.Date("2021-06-11"), as.Date("2021-07-22"), by="10 days"),
               date_labels = "%m/%d")+
  theme(legend.title = element_blank(), legend.position =c(0.5,0.91), 
        legend.text = element_text(size=12),
        axis.text = element_text(size=13), axis.text.x = element_blank(),
        axis.title = element_text(size=14),
        axis.title.x = element_blank())+guides(lty = guide_legend(nrow = 1))
TEMP_Flynn_plot

# Combine Flynn Bay DO and WT #
FLYNN<-  TEMP_Flynn_plot /  DO_Flynn_plot+
  guides(colour="none",lty=guide_legend(order=1, nrow = 1))+plot_annotation(title = "Flynn Bay") &
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))
FLYNN

Figure_4<- wrap_elements(EEL) / wrap_elements(FLYNN) 
Figure_4

ggsave("Figure_4.png", dpi=300, width=10, height=12)