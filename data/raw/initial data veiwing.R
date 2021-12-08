#Exploratory analysis on Opioid Question

library(tidyverse)

setwd("C:/Users/Josh/OneDrive - University of Missouri/econometrics 2021/final project")

#checking out the VSSR data:
data<-read.csv("file:///C:/Users/Josh/OneDrive - University of Missouri/econometrics 2021/final project/VSRR_Provisional_Drug_Overdose_Death_Counts.csv")
data$Indicator %>% table()

#WONDER data
Wonder = read.delim("Overdose Deaths by Category.txt", header = T, stringsAsFactors = FALSE, quote = "", sep = "\t")

#importing the CDC WONDER data
Wonder = read.delim("Opioid Overdose Deaths.txt", header = T, stringsAsFactors = FALSE, quote = "", sep = "\t")

#Mental and behavioral disorders due to psychoactive substance use (F10-F19)
#Nontransport accidents (W00-X59,Y86) 
#Other external causes (X60-X84,Y10-Y36)
#Assault (homicide) (*U01,X85-Y09) 
#Event of undetermined intent (Y10-Y34,Y87.2,Y89.9) 

