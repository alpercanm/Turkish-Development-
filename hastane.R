library(readxl)
library(dplyr)
library(ggplot2)

U1 <- c("Muş", "Diyarbakır", "Ağrı", "Bitlis", "Tunceli")
U2 <- c("Şırnak", "Hakkari", "Batman", "Mardin", "Van")

df <- read_excel("Dissertation_dataset_current_prices_USD (1).xlsx", sheet = "panel")

# 3. Calculate the total hospital counts per region
df_hospitals <- df %>%
  mutate(Group = case_when(
    province %in% U1      ~ "U1 (Muş, Diyarbakır, Ağrı, Bitlis, Tunceli)",
    province %in% U2      ~ "U2 (Şırnak, Hakkari, Batman, Mardin, Van)",
    province %in% c("Mersin", "Amasya", "Sinop", "Uşak") ~ "Mersin + Amasya + Uşak + Sinop",
    TRUE                  ~ NA_character_
  )) %>%
  # Filter for the groups and year >= 2002 (since prior years lack data)
  filter(!is.na(Group), year >= 2002) %>%
  # Sum the hospital counts for all provinces in a group per year
  group_by(year, Group) %>%
  summarise(
    Total_Hospitals = sum(total_hospital, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Create the ggplot line chart
p <- ggplot(df_hospitals, aes(x = year, y = Total_Hospitals, color = Group, group = Group)) +
  geom_vline(xintercept = c(2015, 2019), linetype = "dashed", color = "black", alpha = 0.7) +
  geom_line(linewidth = 1.2) +     
  geom_point(size = 2.5) +         
  scale_color_manual(values = c(
    "U1 (Muş, Diyarbakır, Ağrı, Bitlis, Tunceli)" = "#1f77b4",   
    "U2 (Şırnak, Hakkari, Batman, Mardin, Van)"   = "#d62728",
    "Mersin + Amasya + Uşak + Sinop"                                     = "#2ca02c"
  )) + 
  scale_x_continuous(breaks = seq(min(df_hospitals$year), max(df_hospitals$year), by = 2)) + 
  scale_y_continuous(labels = scales::comma) + 
  labs(
    title = "Trend of Total Hospitals: U1 vs. U2 vs. Mersin + Amasya + Uşak + Sinop ",
    subtitle = "Aggregate count of Hospitals (2002-2024)",
    x = "Year",
    y = "Total Hospitals",
    caption = "Source: Turkish Ministry of Health, 2026"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",          
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )

# Display plot
print(p)