######################################################################################                                                                         ##
## TEMPORAL PATTERNS OF YOUNG-OF-YEAR FISH EMIGRATION IN UPPER ST. LAWRENCE RIVER   ##
## COASTAL WETLANDS IN RELATION TO ENVIRONMENTAL VARIATION          n               ##
## Script Created by Thornton Ritz - thornton.ritz@gmail.com                        ##
## Abiotic Data Data Generation for Table 1, Figure 2                               ##
## Data Source : Daily Catch from 42 day period - 06/11/21 - 07/22/21               ##
## Data available upon request                                                      ##
######################################################################################

#### Packages used ####
library(tidyverse)
library(ggpubr)
library(readxl)
library(purrr)
library(writexl)
library(ggh4x)
library(patchwork)
options(scipen = 999, digits = 6)

##### Set working directory #### 
setwd("/Users/thorn/OneDrive/Desktop/Ritz_Embayment_2021/Manuscript_Data_Files/")
options(scipen = 999, digits = 6)

#### Catch Data file preparation for figures ####
Catch_Raw<- read_excel("Ritz_Embayment_2021_Catch.xlsx")
Catch_Raw$Species<- as.factor(Catch_Raw$Species)
Catch_Raw$Location<- as.factor(Catch_Raw$Location)
Catch_Raw$Date<- as.Date(Catch_Raw$Date)

## Summarize catch data to daily catch for 5 focal taxonomic groups ##
Catch_Sum<- Catch_Raw |>
  filter(Species %in% c("LEU", "LMB", "LEP","BBH", "SMB")) |>
  group_by(Species,Location, Date) |>
  summarize(Total_Catch=sum(Catch), Catch_Per_Day=(sum(Catch)/42))

## Table 2 data generation ##
Table2<- Catch_Sum |>
  group_by(Species, Location) |>
  summarize(MeanCPD= mean(Catch_Per_Day), SD=sd(Catch_Per_Day))

## Data generation for Figure 2 data preparation and plotting ####
Catch_Plot<- Catch_Raw |>
  filter(Catch >0) |>
  filter(Species %in% c("LEU", "LMB", "LEP","BBH", "SMB"))

Catch_Plot$Date<- as.Date(Catch_Plot$Date,  format="%M-%d")
Catch_Plot$Species<- factor(Catch_Plot$Species, 
                            levels=c("BBH", "LEP","LEU",
                                     "LMB","SMB"))

## Figure 2 plotting - Log scale ##
Catch_Figure<- ggplot(Catch_Plot, aes(Date, log(Catch), group=Location, fill=Location))+
  geom_col(position=position_dodge2(preserve = "single"), color="#333333")+
  theme_bw()+
  theme(axis.text = element_text(size=13 ),
        axis.title = element_text(size=14), 
        axis.title.x = element_blank(),
        strip.text = element_text(size=14),
        legend.text=element_text(size=14),
        strip.background = element_rect (fill = "white"),
        legend.position="bottom", legend.title = element_blank())+
  scale_fill_manual(values=c("#333333", "white"))+
  ylab("Log Daily Catch")+ 
  scale_x_date(date_breaks = "5 days", 
               minor_breaks = "1 days",
               date_labels = "%m/%d",
               limits=as.Date(c("2021-05-04","2021-07-22")))+
  scale_y_continuous(breaks = seq(0,8,4), limits=c(0,8))+
  facet_wrap(~Species, nrow = 5)
Catch_Figure

ggsave("Figure_2.png", dpi=300, height = 6, width=10)

