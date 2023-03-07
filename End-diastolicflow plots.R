library(readxl)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
library(RColorBrewer)
library(ggsci)
library(ggpubr)


# Load data from Castor

#flow data (repeated measures)
df <- read_excel("~/Downloads/NECTAR_Necrotizing_Enterocolitis_excel_export_20230114104407.xlsx")
#clinical data
timing_castor <- read_excel("~/Downloads/NECTAR_Necrotizing_Enterocolitis_excel_export_20230208011705.xlsx")
#diagnosis 
Diagnosegroep <- read_excel("~/Downloads/Diagnoses en OKs Morgan tijdelijk.xlsx")


# Merge dataframes based on Participant Id
Diagnosegroep$`Participant Id`<- as.character(Diagnosegroep$`Participant Id`)
total_data <- df %>%
  left_join(timing_castor, by = "Participant Id") %>%
  left_join(Diagnosegroep, by = "Participant Id") %>%
  select(-matches(".*\\.(x|y)$"))

total_data$Participant.Id<-total_data$`Participant Id`

# Add flow measurements
total_data <- total_data %>%
  mutate(
    pin = ifelse(ultr_aca_angle == 1, ultr_aca_pi_no_ac_right_angle, ultr_aca_pi_with_ac),
    ri = ifelse(ultr_aca_angle == 1, ultr_aca_ri_no_ac_right_angle, ultr_aca_ri_with_ac),
    ps = ifelse(ultr_aca_angle == 1, ultr_aca_ps_no_ac_right_angle, ultr_aca_ps_with_ac),
    md = ifelse(ultr_aca_angle == 1, ultr_aca_md_no_ac_right_angle, ultr_aca_md_with_ac),
    pin = as.numeric(pin),
    ri = as.numeric(ri),
    ps = as.numeric(ps),
    md = as.numeric(md)
  )

# Add brain injury information
total_data <- total_data %>%
  mutate(
    bd_pre = ifelse(preop_mri_avail == 1, preop_bd_mri, preop_bd_echo),
    bd_post = ifelse(postop_mri_available == 1, postop_bd_mri, NA),
    bd_total = as.numeric(bd_pre == 1 | bd_post == 1)
  )

# Delete excluded patients
excluded_ids <- c("110007", "110011", "110012", "110019")
total_data <- total_data %>%
  filter(!Participant.Id %in% excluded_ids)

# Prepare data for mixed effect analyses
total_data$age_time_ultr<-as.numeric(total_data$age_time_ultr)
total_data <- total_data %>%
  filter(age_time_ultr < 5)


total_data_TGA <- total_data %>% filter(Diagnosegroep %in% c("TGA"))
total_data_LVOTO <- total_data %>% filter(Diagnosegroep %in% c("LVOTO"))
total_data_SVP <- total_data %>% filter(Diagnosegroep %in% c("SVP"))
total_data_Overig <- total_data %>% filter(Diagnosegroep %in% c("Overig"))




##Plot LVOTO
total_data_LVOTO_perc<- total_data_LVOTO  %>%  group_by(age_time_ultr)%>% count()

#calculate percentage normal and absent flow per day
detach("package:plyr", unload = TRUE)

total_data_LVOTO_perc<- total_data_LVOTO %>% 
  filter(!is.na(aca_flow)) %>% 
  group_by(age_time_ultr, aca_flow) %>% 
  summarise(Percentage=n()) %>% 
  group_by(age_time_ultr) %>% 
  mutate(Percentage=Percentage/sum(Percentage)*100)

#as numeric age_time_ultr
total_data_LVOTO_perc$age_time_ultr <- as.numeric(total_data_LVOTO_perc$age_time_ultr)

#plot data
plot_LVOTO <- ggplot() + geom_bar(aes(x = age_time_ultr,y = Percentage, fill = aca_flow), 
                          data = total_data_LVOTO_perc, stat="identity") + 
  scale_fill_discrete(labels = c("Absent flow", "Forward flow"), 
  name="End-diastolic flow") + xlim(-1, 5) + labs(title = "Diagnosed with LVOTO", x= "Age at time ultrasound (Days)", y= "Percentage of patiënts (%)")
plot_LVOTO

##Plot TGA
total_data_TGA_perc<- total_data_TGA  %>%  group_by(age_time_ultr)%>% count()

#calculate percentage normal and absent flow per day
total_data_TGA_perc<- total_data_TGA %>% 
  filter(!is.na(aca_flow)) %>% 
  group_by(age_time_ultr, aca_flow) %>% 
  summarise(Percentage=n()) %>% 
  group_by(age_time_ultr) %>% 
  mutate(Percentage=Percentage/sum(Percentage)*100)

#as numeric age_time_ultr
total_data_TGA_perc$age_time_ultr <- as.numeric(total_data_TGA_perc$age_time_ultr)

#plot data
plot_TGA <- ggplot() + geom_bar(aes(x = age_time_ultr,y = Percentage, fill = aca_flow), 
                                  data = total_data_TGA_perc, stat="identity") + 
  scale_fill_discrete(labels = c("Absent flow", "Forward flow"), 
                      name="End-diastolic flow") + xlim(-1, 5) + labs(title = "Diagnosed with TGA", x= "Age at time ultrasound (Days)", y= "Percentage of patiënts (%)")
plot_TGA

##Plot SVP
total_data_SVP_perc<- total_data_SVP  %>%  group_by(age_time_ultr)%>% count()

#calculate percentage normal and absent flow per day
total_data_SVP_perc<- total_data_SVP %>% 
  filter(!is.na(aca_flow)) %>% 
  group_by(age_time_ultr, aca_flow) %>% 
  summarise(Percentage=n()) %>% 
  group_by(age_time_ultr) %>% 
  mutate(Percentage=Percentage/sum(Percentage)*100)

#as numeric age_time_ultr
total_data_SVP_perc$age_time_ultr <- as.numeric(total_data_SVP_perc$age_time_ultr)

#plot data
plot_SVP <- ggplot() + geom_bar(aes(x = age_time_ultr,y = Percentage, fill = aca_flow), 
                                   data = total_data_SVP_perc, stat="identity") + 
  scale_fill_discrete(labels = c("Absent flow", "Forward flow"), 
                      name="End-diastolic flow") + xlim(-1, 5) + labs(title = "Diagnosed with SVP", x= "Age at time ultrasound (Days)", y= "Percentage of patiënts (%)")
plot_SVP

##Plot Overig
total_data_Overig_perc<- total_data_Overig  %>%  group_by(age_time_ultr)%>% count()

#calculate percentage normal and absent flow per day
total_data_Overig_perc<- total_data_Overig %>% 
  filter(!is.na(aca_flow)) %>% 
  group_by(age_time_ultr, aca_flow) %>% 
  summarise(Percentage=n()) %>% 
  group_by(age_time_ultr) %>% 
  mutate(Percentage=Percentage/sum(Percentage)*100)

#as numeric age_time_ultr
total_data_Overig_perc$age_time_ultr <- as.numeric(total_data_Overig_perc$age_time_ultr)

#plot data
plot_Overig <- ggplot() + geom_bar(aes(x = age_time_ultr,y = Percentage, fill = aca_flow), 
                                data = total_data_Overig_perc, stat="identity") + 
  scale_fill_discrete(labels = c("Absent flow", "Forward flow"), 
                      name="End-diastolic flow") + xlim(-1, 5) + labs(title = "Diagnosed with remaining diagnosis", x= "Age at time ultrasound (Days)", y= "Percentage of patiënts (%)")
plot_Overig



##Combine plots
together <- ggarrange(plot_LVOTO, plot_TGA, plot_SVP,plot_Overig,
                    labels = c("A", "B", "C"),
                    common.legend = TRUE, legend = "bottom",
                    ncol = 2, nrow = 2)
together
#annotate figure
annotate_figure( together,
  top = text_grob("Enddiastolic flow in different CHD diagnosis groups",
                  color = "black", face = "bold", size = 14))



