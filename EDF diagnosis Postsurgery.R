library(readxl)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(extrafont)
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


# Delete excluded patients
excluded_ids <- c("110007", "110011", "110012", "110019")
total_data <- total_data %>%
  filter(!Participant.Id %in% excluded_ids)

# Prepare data for mixed effect analyses
total_data$age_time_ultr<-as.numeric(total_data$age_time_ultr)
total_data$surg_age<-as.numeric(total_data$surg_age)
total_data$tim_ultr <- total_data$age_time_ultr-total_data$surg_age
total_data <- total_data %>% filter(tim_ultr %in% c("0","1","2","3"))




total_data_TGA <- total_data %>% filter(Diagnosegroep %in% c("TGA"))
total_data_LVOTO <- total_data %>% filter(Diagnosegroep %in% c("LVOTO"))
total_data_SVP <- total_data %>% filter(Diagnosegroep %in% c("SVP"))
total_data_Overig <- total_data %>% filter(Diagnosegroep %in% c("Overig"))



##Plot LVOTO
total_data_LVOTO_count <- total_data_LVOTO %>% 
  group_by(tim_ultr, aca_flow) %>% 
  summarise(n = n())

#calculate percentage normal and absent flow per day
total_data_LVOTO_perc<- total_data_LVOTO %>% 
  filter(!is.na(aca_flow)) %>% 
  group_by(tim_ultr, aca_flow) %>% 
  summarise(Percentage=n()) %>% 
  group_by(tim_ultr) %>% 
  mutate(Percentage=Percentage/sum(Percentage)*100)

# add counts to total_data_Overig_perc
total_data_LVOTO_perc <- total_data_LVOTO_perc %>% 
  left_join(total_data_LVOTO_count, by = c("tim_ultr", "aca_flow")) %>% 
  mutate(n = ifelse(is.na(n), 0, n))  # add closing parenthesis here

#plot data
plot_LVOTO <- ggplot(total_data_LVOTO_perc, aes(x = tim_ultr, y = Percentage, fill = aca_flow)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#f28e2c","#4e79a7"), 
                    labels = c("Absent flow", "Forward flow"), 
                    name = "End-diastolic flow") +
  labs(title = "Diagnosed with LVOTO", 
       x = ""  , y = ""  ) + 
  theme_classic() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12), 
        legend.title = element_text(size = 12, face = "bold"), 
        legend.text = element_text(size = 12), 
        panel.grid.major.y = element_line(color = "gray80"), 
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank())+
  geom_text(aes(label = paste0(   n)), 
            position = position_stack(vjust = 0.5), 
            size = 4, color="white")


plot_LVOTO


##Plot TGA
total_data_TGA_count <- total_data_TGA %>% 
  group_by(tim_ultr, aca_flow) %>% 
  summarise(n = n())

#calculate percentage normal and absent flow per day
total_data_TGA_perc<- total_data_TGA %>% 
  filter(!is.na(aca_flow)) %>% 
  group_by(tim_ultr, aca_flow) %>% 
  summarise(Percentage=n()) %>% 
  group_by(tim_ultr) %>% 
  mutate(Percentage=Percentage/sum(Percentage)*100)

# add counts to TGA
total_data_TGA_perc <- total_data_TGA_perc %>% 
  left_join(total_data_TGA_count, by = c("tim_ultr", "aca_flow")) %>% 
  mutate(n = ifelse(is.na(n), 0, n))  # add closing parenthesis here



#plot data
plot_TGA <- ggplot(total_data_TGA_perc, aes(x = tim_ultr, y = Percentage, fill = aca_flow)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#f28e2c","#4e79a7"), 
                    labels = c("Absent flow", "Forward flow"), 
                    name = "End-diastolic flow") +
  labs(title = "Diagnosed with TGA", 
       x = ""  , y = ""  ) + 
  theme_classic() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12), 
        legend.title = element_text(size = 12, face = "bold"), 
        legend.text = element_text(size = 12), 
        panel.grid.major.y = element_line(color = "gray80"), 
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank())+
  geom_text(aes(label = paste0(   n)), 
            position = position_stack(vjust = 0.5), 
            size = 4, color="white")

plot_TGA

##Plot SVP
total_data_SVP_count <- total_data_SVP %>% 
  group_by(tim_ultr, aca_flow) %>% 
  summarise(n = n())

#calculate percentage normal and absent flow per day
total_data_SVP_perc<- total_data_SVP %>% 
  filter(!is.na(aca_flow)) %>% 
  group_by(tim_ultr, aca_flow) %>% 
  summarise(Percentage=n()) %>% 
  group_by(tim_ultr) %>% 
  mutate(Percentage=Percentage/sum(Percentage)*100)

# add counts to SVP
total_data_SVP_perc <- total_data_SVP_perc %>% 
  left_join(total_data_SVP_count, by = c("tim_ultr", "aca_flow")) %>% 
  mutate(n = ifelse(is.na(n), 0, n))  # add closing parenthesis here



#plot data
plot_SVP <- ggplot(total_data_SVP_perc, aes(x = tim_ultr, y = Percentage, fill = aca_flow)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#f28e2c","#4e79a7"), 
                    labels = c("Absent flow", "Forward flow"), 
                    name = "End-diastolic flow") +
  labs(title = "Diagnosed with SVP", 
       x = ""  , y = ""  ) + 
  theme_classic() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12), 
        legend.title = element_text(size = 12, face = "bold"), 
        legend.text = element_text(size = 12), 
        panel.grid.major.y = element_line(color = "gray80"), 
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank())+
  geom_text(aes(label = paste0(   n)), 
            position = position_stack(vjust = 0.5), 
            size = 4, color="white")

plot_SVP


##plot Overig

total_data_Overig_count <- total_data_Overig %>% 
  group_by(tim_ultr, aca_flow) %>% 
  summarise(n = n())

#calculate percentage normal and absent flow per day
total_data_Overig_perc<- total_data_Overig %>% 
  filter(!is.na(aca_flow)) %>% 
  group_by(tim_ultr, aca_flow) %>% 
  summarise(Percentage=n()) %>% 
  group_by(tim_ultr) %>% 
  mutate(Percentage=Percentage/sum(Percentage)*100)

# add counts to total_data_Overig_perc
total_data_Overig_perc <- total_data_Overig_perc %>% 
  left_join(total_data_Overig_count, by = c("tim_ultr", "aca_flow")) %>% 
  mutate(n = ifelse(is.na(n), 0, n))  # add closing parenthesis here


#as numeric tim_ultr
total_data_Overig_perc$tim_ultr <- as.numeric(total_data_Overig_perc$tim_ultr)


#plot data
plot_Overig <- ggplot(total_data_Overig_perc, aes(x = tim_ultr, y = Percentage, fill = aca_flow)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#f28e2c","#4e79a7"), 
                    labels = c("Absent flow", "Forward flow"), 
                    name = "End-diastolic flow") +
  labs(title = "Diagnosed with other CHD diagnosis", 
       x = ""  , y = ""  ) + 
  theme_classic() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12), 
        legend.title = element_text(size = 12, face = "bold"), 
        legend.text = element_text(size = 12), 
        panel.grid.major.y = element_line(color = "gray80"), 
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank())+
  geom_text(aes(label = paste0( n)), 
            position = position_stack(vjust = 0.5), 
            size = 4, color="white")



plot_Overig


##Combine plots
together <- ggarrange(plot_TGA, plot_LVOTO, plot_SVP,plot_Overig, 
                      labels = c("A", "B", "C","D"),
                      
                      common.legend = TRUE, legend = "right",
                      ncol = 2, nrow = 2)
together
#annotate figure
annotate_figure(together,
                top = text_grob("Enddiastolic flow in different CHD diagnosis groups post-surgery",
                                color = "black", face = "bold", size = 14),
                left = textGrob("Percentage of Patients (%)", rot = 90, vjust = 1, gp = gpar(cex = 1.3, fontsize = 11)),
                bottom = textGrob("Days post-surgery", gp = gpar(cex = 1.3, fontsize = 11)))



