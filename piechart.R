library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)

Diagnosegroep <- read_excel("~/Downloads/Diagnoses en OKs Morgan tijdelijk.xlsx")

#delete patients 
excluded_ids <- c("110007", "110011", "110012", "110019")
Diagnosegroep <- Diagnosegroep %>%
  filter(!`Participant Id` %in% excluded_ids)

#determine count per diagnosis
df_piechart_diagnose<- Diagnosegroep %>% count(Diagnosegroep)
df_piechart_diagnose


#determine percentages
pie_perc <- paste0(round(100 * df_piechart_diagnose$n/sum(df_piechart_diagnose$n), 2), "%")
pie_perc
pie_labels<-pie_perc

#make piechart
pie(df_piechart_diagnose$n, labels=pie_labels)

library(RColorBrewer)

#add legend to piechart
col1 <- brewer.pal(4, "GnBu") 
rainbow<-col1
pie(df_piechart_diagnose$n, labels = pie_labels, main = "Diagnosis",col = rainbow(length(df_piechart_diagnose$n)))
legend("topright", c("LVOTO", "Overig", "SVP", "TGA"), cex = 0.8,
       fill = rainbow(length(df_piechart_diagnose$n)))

#change color of piechart
col1 <- brewer.pal(4, "GnBu") 
pie(df_piechart_diagnose$n, labels = pie_labels, main = "CHD diagnosis",col = col1)
legend("topright", c("LVOTO", "Overig", "SVP", "TGA"), cex = 0.8,
       fill = col1)

#piechart with only diagnosis
col1 <- brewer.pal(4, "GnBu") 
pie(df_piechart_diagnose$n, labels = df_piechart_diagnose$Diagnosegroep,col = col1)


#piechart with diagnosis and percentage
col1 <- brewer.pal(4, "GnBu") 
pie_perc <- paste0(round(100 * df_piechart_diagnose$n/sum(df_piechart_diagnose$n), 2), "%")
pie_perc
lbls <- paste(df_piechart_diagnose$Diagnosegroep, pie_perc) # add percents to labels
lbls <- paste(lbls,sep="") # ad % to labels
pie(df_piechart_diagnose$n, labels = lbls,col=col1)

