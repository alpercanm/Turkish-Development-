library(readxl)
library(dplyr)
library(ggplot2)

# ---- Province groups ---------------------------------------------------
# NOTE: province names are stored in Turkish UPPERCASE ("DİYARBAKIR", "MUŞ",
# "HAKKARİ", "MERSİN", ...). Do not use toupper() to convert title-case names
# to these: in a non-Turkish locale toupper("Mersin") gives "MERSIN" (dotless
# I), which will not match "MERSİN". Match the literals.

U1 <- c("MUŞ", "DİYARBAKIR", "AĞRI", "BİTLİS", "TUNCELİ")
U2 <- c("ŞIRNAK", "HAKKARİ", "BATMAN", "MARDİN", "VAN")
AB <- c("MERSİN", "AMASYA", "SİNOP", "UŞAK")

# Labels defined once, so the case_when value and the factor level cannot
# drift apart. (A mismatch silently turns the group to NA and the curve
# disappears from the plot without any error.)
lab_U1 <- "U1 (Muş, Diyarbakır, Ağrı, Bitlis, Tunceli)"
lab_U2 <- "U2 (Şırnak, Hakkari, Batman, Mardin, Van)"
lab_AB <- "Mersin + Amasya + Sinop + Usak"

# No province may appear in two groups: case_when would take the first match
# silently and the second group would be short by that province.
stopifnot(length(intersect(U1, U2)) == 0,
          length(intersect(U1, AB)) == 0,
          length(intersect(U2, AB)) == 0)

# ---- Read and aggregate ------------------------------------------------
raw <- read_excel("derslik sayisi.xlsx")

# Validate in the useful direction: every province we asked for must exist in
# the file. Extra provinces in the file are fine and are dropped below.
wanted  <- c(U1, U2, AB)
missing <- setdiff(wanted, unique(raw$province))
if (length(missing) > 0) {
  stop("Provinces not found in the data: ", paste(missing, collapse = ", "))
}

classrooms <- raw %>%
  mutate(
    group = case_when(
      province %in% U1 ~ lab_U1,
      province %in% U2 ~ lab_U2,
      province %in% AB ~ lab_AB,
      TRUE             ~ NA_character_
    ),
    # "2015-2016" -> 2015 (academic year start)
    year_start = as.integer(substr(year, 1, 4))
  ) %>%
  filter(!is.na(group))

classroom_trends <- classrooms %>%
  group_by(group, year_start) %>%
  summarise(state_classes = sum(state), .groups = "drop") %>%
  mutate(group = factor(group, levels = c(lab_U1, lab_U2, lab_AB)))

stopifnot(!any(is.na(classroom_trends$group)))  # catches any label drift

# ---- Plot --------------------------------------------------------------
p <- ggplot(classroom_trends, aes(x = year_start, y = state_classes,
                                  colour = group, group = group)) +
  geom_vline(xintercept = c(2015, 2019),
             linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_x_continuous(
    breaks = seq(min(classroom_trends$year_start),
                 max(classroom_trends$year_start), by = 2)
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_manual(values = c("#1f77b4", "#d62728", "#2ca02c")) +
  labs(
    title    = "Trend of State Middle School Classes",
    subtitle = sprintf(
      "Total aggregate count of state classrooms, academic years starting %d-%d",
      min(classroom_trends$year_start), max(classroom_trends$year_start)
    ),
    x        = "Academic Year (Start Year)",
    y        = "Number of State Classes",
    colour   = NULL,
    caption  = "Source: Turkish Ministry of Education"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "bottom",
    legend.direction = "vertical",
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold")
  )

print(p)

# ggsave("classroom_trends.pdf", p, width = 8, height = 5)