library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

f <- "Distribution_of_Number_of_Hospital_Beds_by_Provinces.xls"

# TRUE  = MoH + University + Private + Other  (all sectors; consistent across 2016)
# FALSE = MoH + University + Private          (drops "Diğer - Other")
#
# NOTE: "Diğer - Other" holds military hospital beds and goes to exactly zero
# in 2016, when military hospitals were transferred to the Health Ministry.
# Setting this FALSE removes those beds from 2002-2015 but NOT from 2016
# onward, where they are merged into the MoH line and cannot be separated.
# That inflates measured growth in provinces with a military presence: the
# treated region held 374 such beds in 2015 against the donors' 50, so the
# 2015-2019 gain rises 3.24pp for U1+U2 but only 0.95pp for the donors.
include_other <- T

# ---- Groups ------------------------------------------------------------
U1 <- c("Muş", "Diyarbakır", "Ağrı", "Bitlis", "Tunceli")
U2 <- c("Şırnak", "Hakkari", "Batman", "Mardin", "Van")
D  <- c("Mersin", "Amasya", "Uşak", "Sinop")

lab_U1 <- "U1 (Muş, Diyarbakır, Ağrı, Bitlis, Tunceli)"
lab_U2 <- "U2 (Şırnak, Hakkari, Batman, Mardin, Van)"
lab_D  <- "Mersin + Amasya + Uşak + Sinop"

# ---- Read ---------------------------------------------------------------
# Sheet 't2': row 3 holds the years, column 1 the province (only on its first
# row), column 2 the sector. A province TOTAL is the row with a blank sector.
raw <- read_excel(f, sheet = "t2", col_names = FALSE)

yr_hdr    <- as.character(unlist(raw[3, ]))
year_cols <- which(str_detect(yr_hdr, "^[0-9]{4}$"))
years     <- as.integer(yr_hdr[year_cols])

beds <- raw[-(1:3), c(1, 2, year_cols)]
names(beds) <- c("province_raw", "sector", paste0("y", years))

beds_long <- beds %>%
  fill(province_raw, .direction = "down") %>%
  mutate(
    province = str_trim(str_remove(province_raw, " - .*$")),
    sector   = if_else(is.na(sector), "TOTAL", str_trim(str_remove(sector, " - .*$")))
  ) %>%
  select(province, sector, starts_with("y")) %>%
  pivot_longer(-c(province, sector), names_to = "year", values_to = "beds") %>%
  mutate(
    year = as.integer(str_remove(year, "^y")),
    beds = suppressWarnings(as.numeric(na_if(as.character(beds), "-")))
  ) %>%
  filter(!is.na(beds))

keep <- if (include_other) "TOTAL" else c("Sağlık Bakanlığı", "Üniversite", "Özel")

beds_sel <- beds_long %>% filter(sector %in% keep)

missing <- setdiff(c(U1, U2, D), unique(beds_sel$province))
if (length(missing) > 0) stop("Not found: ", paste(missing, collapse = ", "))

# ---- Aggregate ----------------------------------------------------------
bed_trends <- beds_sel %>%
  mutate(Group = case_when(
    province %in% U1 ~ lab_U1,
    province %in% U2 ~ lab_U2,
    province %in% D  ~ lab_D,
    TRUE             ~ NA_character_
  )) %>%
  filter(!is.na(Group)) %>%
  group_by(Group, year) %>%
  summarise(Total_Beds = sum(beds), .groups = "drop") %>%
  mutate(Group = factor(Group, levels = c(lab_U1, lab_U2, lab_D)))

# ---- Plot ---------------------------------------------------------------
sub <- if (include_other) {
  "Aggregate count of hospital beds, all sectors"
} else {
  "Aggregate count of hospital beds, excluding the 'Other' (military) sector"
}

p <- ggplot(bed_trends, aes(x = year, y = Total_Beds, colour = Group, group = Group)) +
  geom_vline(xintercept = c(2015, 2019),
             linetype = "dashed", colour = "black", alpha = 0.7) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = setNames(c("#1f77b4", "#d62728", "#2ca02c"),
                                        c(lab_U1, lab_U2, lab_D))) +
  scale_x_continuous(breaks = seq(min(bed_trends$year), max(bed_trends$year), by = 2)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Trend of Total Hospital Beds",
    subtitle = sprintf("%s (%d-%d)", sub, min(bed_trends$year), max(bed_trends$year)),
    x        = "Year",
    y        = "Total Hospital Beds",
    caption  = "Source: Turkish Ministry of Health, 2026"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 14),
    legend.position  = "bottom",
    legend.direction = "vertical",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 10)
  )

print(p)

# ggsave("bed_trends.pdf", p, width = 8, height = 5)