#Stacked bar plot for PFAS at Sandia

library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(viridisLite)
library(janitor)

date_format <- "%m/%d/%Y"

sandia_alluvial_file <- 'Data/Sandia_Alluvial_PFAS_2024_Intellus_EXPORT_10_26_2025.csv'
gage_pfas_file <- 'Data/Sandia_Gage_PFAS_2024_Intellus_EXPORT_10_26_2025.csv'
pfas_short <- 'Data/pfas_parameter_code_short_name.xlsx'

pfas_short <- read_xlsx(pfas_short)

custom_colors <- viridisLite::viridis(20, option = "C")
random_colors <- sample(colors(), 20)

san_alluv <- read_csv(sandia_alluvial_file, 
                      col_types = list(`Sample Date` = col_date(format = "%m-%d-%Y"))) %>%
  clean_names() %>%
  filter(sample_purpose=="REG", lab_method == "EPA:1633") %>%
  mutate(location_alias = location_id)%>%
  select(location_alias, parameter_name, parameter_code, detected, report_result, report_units, sample_type, sample_date)

san_gage <- read_csv(gage_pfas_file,
                     col_types = list(`Sample Date` = col_date(format = "%m-%d-%Y"))) %>%
  clean_names() %>%
  filter(location_alias %in% c("E121", "E122", "E123"), sample_date %in% c("2024-07-30", "2024-06-20")) %>%
  select(location_alias, parameter_name, parameter_code, detected, report_result, report_units, sample_type, sample_date)

san_all <- rbind(san_alluv, san_gage) %>%
  filter(detected == "Y")

san_all <- san_all %>%
  left_join(pfas_short, by = 'parameter_code')

stack_bar <- san_all %>%
  ggplot(aes(x = location_alias, y = report_result, fill = parameter_name)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  facet_wrap(~ sample_type, scales = "free_x",
             labeller = labeller(sample_type = c(
               "WG" ="Groundwater 10/24/2024",
               "WS" = "Baseflow 7/30/2024",
               "WT" = "Stormflow 6/20/2024"
             ))) +
  scale_fill_manual(values = random_colors) +
  labs(title = "PFAS Chemical Concentrations by Site and Sample Type",
       x = "",
       y = "Report Result (ng/L)") +
  guides(fill = guide_legend(title = "Detected PFAS Chemicals")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle =45, hjust = 1))
print(stack_bar)

ggsave(stack_bar, filename = "Output/stacked_bar/PFAS_Upper_Sandia_Stacked_Bar_Plot_2024.png", width = 16, height = 8, units = "in")