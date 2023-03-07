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


##Prepare data
df$Participant.Id<-df$`Participant Id`
timing_castor$Participant.Id<-timing_castor$`Participant Id`
Diagnosegroep$Participant.Id<-Diagnosegroep$`Participant Id`

# Merge dataframes based on Participant Id
Diagnosegroep$Participant.Id<- as.character(Diagnosegroep$Participant.Id)
total_data <- df %>%
  left_join(timing_castor, by = "Participant.Id") %>%
  left_join(Diagnosegroep, by = "Participant.Id") %>%
  select(-matches(".*\\.(x|y)$"))


# Add flow measurements
total_data <- total_data %>%
  mutate(
    pi = ifelse(ultr_aca_angle == 1, ultr_aca_pi_no_ac_right_angle, ultr_aca_pi_with_ac),
    ri = ifelse(ultr_aca_angle == 1, ultr_aca_ri_no_ac_right_angle, ultr_aca_ri_with_ac),
    ps = ifelse(ultr_aca_angle == 1, ultr_aca_ps_no_ac_right_angle, ultr_aca_ps_with_ac),
    md = ifelse(ultr_aca_angle == 1, ultr_aca_md_no_ac_right_angle, ultr_aca_md_with_ac),
    pi = as.numeric(pi),
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


bd_data <- subset(timing_castor, select = c("Participant.Id",
                                            "preop_type_bd_mri#White_matter_injury", 
                                            "preop_type_bd_mri#Hypoxicischemic_watershed_injury",                                      
                                            "preop_type_bd_mri#Hemorrhages_intraparenchymal_cerebralcerebellar_intraventricular_subdural", 
                                            "preop_type_bd_mri#Sinovenous_thrombosis",
                                            "preop_type_bd_mri#Arterial_ischemic_stroke",
                                            "preop_type_bd_mri#Cerebellar_lesion",
                                            "postop_type_bd_mri#White_matter_injury",                                                     
                                            "postop_type_bd_mri#Hypoxicischemic_watershed_injury",                                        
                                            "postop_type_bd_mri#Hemorrhages_intraparenchymal_cerebralcerebellar_intraventricular_subdural",
                                            "postop_type_bd_mri#Sinovenous_thrombosis",
                                            "postop_type_bd_mri#Arterial_ischemic_stroke",
                                            "postop_type_bd_mri#Cerebellar_lesion"))

# Rename the columns to simpler and more readable names
colnames(bd_data) <- c("Participant.Id",  
                       "Preop_WM_Injury", "Preop_Hypoxia", "Preop_Hemorrhage",
                       "Preop_Sinovenous_Thrombosis", "Preop_AIS", "Preop_Cerebellar_Lesion",
                       "Postop_WM_Injury", "Postop_Hypoxia", "Postop_Hemorrhage",
                       "Postop_Sinovenous_Thrombosis", "Postop_AIS", "Postop_Cerebellar_Lesion")


#create colums for groups of brain damage: 
bd_data$pre_thrombosis <- ifelse((bd_data$Preop_AIS == 1 | bd_data$Preop_Sinovenous_Thrombosis == 1), 1, 0)
bd_data$post_thrombosis <- ifelse((bd_data$Postop_AIS == 1 | bd_data$Postop_Sinovenous_Thrombosis == 1), 1, 0)
bd_data$pre_hemor <- ifelse((bd_data$Preop_Hemorrhage == 1), 1, 0)
bd_data$post_hemor <- ifelse((bd_data$Postop_Hemorrhage == 1), 1, 0)
bd_data$pre_wmi <- ifelse((bd_data$Preop_WM_Injury == 1), 1, 0)
bd_data$post_wmi <- ifelse((bd_data$Postop_WM_Injury == 1), 1, 0)
bd_data$pre_wsi <- ifelse((bd_data$Preop_Hypoxia == 1), 1, 0)
bd_data$post_wsi <- ifelse((bd_data$Postop_Hypoxia == 1), 1, 0)

#create colums for pre and post brain damage together:
bd_data$total_thrombosis <- ifelse((bd_data$pre_thrombosis == 1 | bd_data$post_thrombosis == 1), 1, 0)
bd_data$total_hemor <- ifelse((bd_data$pre_hemor == 1 | bd_data$post_hemor == 1), 1, 0)
bd_data$total_wmi <- ifelse((bd_data$pre_wmi == 1 | bd_data$post_wmi == 1), 1, 0)
bd_data$total_wsi <- ifelse((bd_data$pre_wsi == 1 | bd_data$post_wsi == 1), 1, 0)

#make groups of brain damage
flow_thromb<- subset(bd_data, total_thrombosis == "1")
flow_hemor<- subset(bd_data, total_hemor == "1")
flow_wmi<- subset(bd_data, total_wmi == "1")
flow_wsi<- subset(bd_data, total_wsi == "1")


#join brain damage data with total_data
total_data_bd <- merge(total_data, bd_data, by = "Participant.Id", all.x = TRUE)


# Delete excluded patients
excluded_ids <- c("110007", "110011", "110012", "110019")
total_data_bd <- total_data_bd %>%
  filter(!Participant.Id %in% excluded_ids)

# Prepare data for mixed effect analyses
total_data_bd$age_time_ultr<-as.numeric(total_data_bd$age_time_ultr)
total_data_bd$surg_age<-as.numeric(total_data_bd$surg_age)
total_data_bd$tim_ultr <- total_data_bd$age_time_ultr-total_data_bd$surg_age


#create new timing timeline
total_data_birth<-total_data_bd[total_data_bd$tim_ultr %in% c('0','1','2','3'),]



#make new datasets based on diagnosegroep
total_data_hemor<-filter(total_data_birth, total_data_birth$total_hemor == "1")
total_data_thromb<-filter(total_data_birth, total_data_birth$total_thromb == "1")
total_data_wmi<-filter(total_data_birth, total_data_birth$total_wmi == "1")
total_data_wsi<-filter(total_data_birth, total_data_birth$total_wsi == "1")
total_data_nobd<-filter(total_data_birth, total_data_birth$bd_total == "0") 


##Plot hemorrhage
total_data_hemor_perc<- total_data_hemor %>%  group_by(tim_ultr)%>% count()

#calculate percentage normal and absent flow per day
detach("package:plyr", unload = TRUE)

total_data_hemor_perc<- total_data_hemor %>% 
  filter(!is.na(aca_flow)) %>% 
  group_by(tim_ultr, aca_flow) %>% 
  summarise(Percentage=n()) %>% 
  group_by(tim_ultr) %>% 
  mutate(Percentage=Percentage/sum(Percentage)*100)

#as numeric tim_ultr
total_data_hemor_perc$tim_ultr <- as.numeric(total_data_hemor_perc$tim_ultr)

#plot data
plot_hemor <- ggplot(total_data_hemor_perc, aes(x = tim_ultr, y = Percentage, fill = aca_flow)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#f28e2c","#4e79a7"), 
                    labels = c("Absent flow", "Forward flow"), 
                    name = "End-diastolic flow") +
  labs(title = "Hemorrhage", 
       x = "Days post-surgery", y = "Percentage of Patients (%)") + 
  theme_classic() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12), 
        legend.title = element_text(size = 12, face = "bold"), 
        legend.text = element_text(size = 12), 
        panel.grid.major.y = element_line(color = "gray80"), 
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank())

plot_hemor


##Plot thrombosis
total_data_thromb_perc<- total_data_thromb  %>%  group_by(tim_ultr)%>% count()

#calculate percentage normal and absent flow per day
total_data_thromb_perc<- total_data_thromb %>% 
  filter(!is.na(aca_flow)) %>% 
  group_by(tim_ultr, aca_flow) %>% 
  summarise(Percentage=n()) %>% 
  group_by(tim_ultr) %>% 
  mutate(Percentage=Percentage/sum(Percentage)*100)

#as numeric tim_ultr
total_data_thromb_perc$tim_ultr <- as.numeric(total_data_thromb_perc$tim_ultr)

#plot data
plot_throm <- ggplot(total_data_thromb_perc, aes(x = tim_ultr, y = Percentage, fill = aca_flow)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#f28e2c","#4e79a7"), 
                    labels = c("Absent flow", "Forward flow"), 
                    name = "End-diastolic flow") +
  labs(title = "Thrombosis", 
       x = "Days post-surgery", y = "Percentage of Patients (%)") + 
  theme_classic() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12), 
        legend.title = element_text(size = 12, face = "bold"), 
        legend.text = element_text(size = 12), 
        panel.grid.major.y = element_line(color = "gray80"), 
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank())

plot_throm

##Plot WMI
total_data_wmi_perc<- total_data_wmi  %>%  group_by(tim_ultr)%>% count()

#calculate percentage normal and absent flow per day
total_data_wmi_perc<- total_data_wmi %>% 
  filter(!is.na(aca_flow)) %>% 
  group_by(tim_ultr, aca_flow) %>% 
  summarise(Percentage=n()) %>% 
  group_by(tim_ultr) %>% 
  mutate(Percentage=Percentage/sum(Percentage)*100)

#as numeric tim_ultr
total_data_wmi_perc$tim_ultr <- as.numeric(total_data_wmi_perc$tim_ultr)

#plot data
plot_wmi <- ggplot(total_data_wmi_perc, aes(x = tim_ultr, y = Percentage, fill = aca_flow)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#f28e2c","#4e79a7"), 
                    labels = c("Absent flow", "Forward flow"), 
                    name = "End-diastolic flow") +
  labs(title = "White matter injury", 
       x = "Days post-surgery", y = "Percentage of Patients (%)") + 
  theme_classic() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12), 
        legend.title = element_text(size = 12, face = "bold"), 
        legend.text = element_text(size = 12), 
        panel.grid.major.y = element_line(color = "gray80"), 
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank())

plot_wmi


##Plot wsi
total_data_wsi_perc<- total_data_wsi  %>%  group_by(tim_ultr)%>% count()

#calculate percentage normal and absent flow per day
total_data_wsi_perc<- total_data_wsi %>% 
  filter(!is.na(aca_flow)) %>% 
  group_by(tim_ultr, aca_flow) %>% 
  summarise(Percentage=n()) %>% 
  group_by(tim_ultr) %>% 
  mutate(Percentage=Percentage/sum(Percentage)*100)

#as numeric tim_ultr
total_data_wsi_perc$tim_ultr <- as.numeric(total_data_wsi_perc$tim_ultr)

#plot data
plot_wsi <- ggplot(total_data_wsi_perc, aes(x = tim_ultr, y = Percentage, fill = aca_flow)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#f28e2c","#4e79a7"), 
                    labels = c("Absent flow", "Forward flow"), 
                    name = "End-diastolic flow") +
  labs(title = "Watershed injury", 
       x = "Days post-surgery", y = "Percentage of Patients (%)") + 
  theme_classic() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12), 
        legend.title = element_text(size = 12, face = "bold"), 
        legend.text = element_text(size = 12), 
        panel.grid.major.y = element_line(color = "gray80"), 
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank())

plot_wsi

##plot no brain damage

total_data_nobd_perc<- total_data_nobd  %>%  group_by(tim_ultr)%>% count()

#calculate percentage normal and absent flow per day
total_data_nobd_perc<- total_data_nobd %>% 
  filter(!is.na(aca_flow)) %>% 
  group_by(tim_ultr, aca_flow) %>% 
  summarise(Percentage=n()) %>% 
  group_by(tim_ultr) %>% 
  mutate(Percentage=Percentage/sum(Percentage)*100)

#as numeric tim_ultr
total_data_nobd_perc$tim_ultr <- as.numeric(total_data_nobd_perc$tim_ultr)

#plot data
plot_nobd <- ggplot(total_data_nobd_perc, aes(x = tim_ultr, y = Percentage, fill = aca_flow)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#f28e2c","#4e79a7"), 
                    labels = c("Absent flow", "Forward flow"), 
                    name = "End-diastolic flow") +
  labs(title = "No brain injury", 
       x = "Days post-surgery", y = "Percentage of Patients (%)") + 
  theme_classic() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12), 
        legend.title = element_text(size = 12, face = "bold"), 
        legend.text = element_text(size = 12), 
        panel.grid.major.y = element_line(color = "gray80"), 
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank())

plot_nobd


##Combine plots
together <- ggarrange(plot_throm, plot_hemor, plot_wmi,plot_nobd,
                      labels = c("A", "B", "C","D"),
                      common.legend = TRUE, legend = "bottom",
                      ncol = 2, nrow = 2)
together
#annotate figure
annotate_figure( together,
                 top = text_grob("End-diastolic flow results grouped by MRI results",
                                 color = "black", face = "bold", size = 14))



