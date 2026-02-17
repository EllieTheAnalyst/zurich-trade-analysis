# 1. SETUP & LIBRARIES
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("scales")) install.packages("scales")
if (!require("ggrepel")) install.packages("ggrepel")
if (!require("viridis")) install.packages("viridis")

library(tidyverse)
library(scales)
library(ggrepel)
library(viridis)

# 2. LOAD & CLEAN DATA (The "NA-Free" Pipeline)
data_path <- file.path("data", "processed", "Cleaned_HR_Final_Report.csv")
stopifnot(file.exists(data_path))

df <- readr::read_csv(data_path, show_col_types = FALSE)

# Comprehensive Shorter Names Map (Punchy Labels)
short_names <- c(
  "Pharmazeutische Erzeugnisse" = "Pharma",
  "Maschinen" = "Machinery",
  "Chemische Erzeugnisse" = "Chemicals",
  "Datenverarbeitungsgeräte, elektronische und optische Erzeugnisse" = "Electronics",
  "Elektrische Ausrüstungen" = "Electric Eq",
  "Metallerzeugnisse" = "Metal Prod",
  "Nahrungs- und Futtermittel" = "Food/Bev",
  "Verschiedene (weitere) Güterkategorien" = "Misc",
  "Metalle" = "Raw Metals",
  "Textilien" = "Textiles",
  "Bekleidung" = "Apparel",
  "Uhren" = "Watches",
  "Getränke" = "Drinks",
  "Möbel" = "Furniture",
  "Papier, Pappe und Waren daraus" = "Paper",
  "Leder und Lederwaren" = "Leather",
  "Holz sowie Holz- und Korkwaren (ohne Möbel); Flecht- und Korbwaren" = "Wood",
  "Gummi- und Kunststoffwaren" = "Plastic",
  "Glas- und Glaswaren, Keramik, verarbeitete Steine und Erden" = "Glass/Ceram",
  "Kraftwagen und Kraftwagenteile" = "Auto",
  "Sonstige Fahrzeuge" = "Vehicles",
  "Waren, a.n.g." = "Other Goods",
  "Dienstleistungen des Verlagswesens" = "Publishing",
  "Erzeugnisse der Landwirtschaft und Jagd sowie damit verbundene Dienstleistungen" = "Agri",
  "Kokereierzeugnisse und Mineralölerzeugnisse" = "Oil/Coke",
  "Kreative, künstlerische und unterhaltende Dienstleistungen" = "Creative",
  "Dienstleistungen der Herstellung, des Verleihs und Vertriebs von Filmen..." = "Media",
  "Dienstleistungen von Bibliotheken, Archiven und Museen..." = "Culture",
  "Dienstleistungen der Sammlung, Behandlung und Beseitigung von Abfällen..." = "Waste",
  "Forstwirtschaftliche Erzeugnisse und Dienstleistungen" = "Forestry",
  "Steine und Erden; sonstige Bergbauerzeugnisse" = "Mining",
  "Druckereileistungen..." = "Printing",
  "Sonstige freiberufliche, wissenschaftliche und technische Dienstleistungen" = "Tech Serv",
  "Fische and Fischereierzeugnisse..." = "Fishery",
  "Kohle" = "Coal",
  "Erdöl und Erdgas" = "Oil/Gas",
  "Tabakerzeugnisse" = "Tobacco",
  "Erze" = "Ores"
)

# Initial cleaning: Drop NAs and map names
df_clean <- df %>%
  drop_na(Country, Total_Amount_CHF) %>%
  filter(Total_Amount_CHF > 0) %>%
  mutate(Industry_EN = recode(Industry, !!!short_names, .default = str_trunc(Industry, 15)))

# ---------------------------------------------------------
# PART 1: SCATTER PLOT ANALYSIS (Export vs. Import 2024)
# ---------------------------------------------------------

# Prepare Scatter Data
scatter_data <- df_clean %>%
  filter(Year == 2024) %>%
  group_by(Country, Trade_Type) %>%
  summarise(Value = sum(Total_Amount_CHF), .groups = 'drop') %>%
  pivot_wider(names_from = Trade_Type, values_from = Value, values_fill = 0) %>%
  mutate(Total_Volume = Export + Import)

# A. Normal Scale
p1 <- ggplot(scatter_data, aes(x = Export, y = Import, size = Total_Volume, color = Total_Volume)) +
  geom_point(alpha = 0.6) +
  geom_text_repel(data = head(arrange(scatter_data, desc(Total_Volume)), 8), 
                  aes(label = Country), size = 4, color = "black") +
  scale_x_continuous(labels = label_number(suffix = " B", scale = 1e-9)) +
  scale_y_continuous(labels = label_number(suffix = " B", scale = 1e-9)) +
  scale_color_viridis_c(option = "mako") +
  theme_minimal() +
  labs(title = "Trade Balance: Normal Scale (2024)", x = "Export (CHF)", y = "Import (CHF)")

# B. Log-Log Scale
p2 <- ggplot(scatter_data, aes(x = Export, y = Import)) +
  geom_point(alpha = 0.5, color = "#d95f02") +
  scale_x_log10(labels = label_comma()) +
  scale_y_log10(labels = label_comma()) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  theme_minimal() +
  labs(title = "Trade Balance: Log-Log Scale", subtitle = "Reveals the 'Trade Corridor' for smaller nations")

# C. Zoomed View (Ranks 21-60)
zoom_scatter <- scatter_data %>% arrange(desc(Total_Volume)) %>% slice(21:60)
p3 <- ggplot(zoom_scatter, aes(x = Export, y = Import)) +
  geom_point(color = "#1b9e77", size = 3) +
  geom_text_repel(aes(label = Country), size = 3) +
  scale_x_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  theme_minimal() +
  labs(title = "Zoom: Middle-Market Partners (Ranks 21-60)", subtitle = "Targeted emerging market view")

# ---------------------------------------------------------
# PART 2: HEATMAP ANALYSIS (Temporal Stability)
# ---------------------------------------------------------

# Prepare Heatmap Data
heatmap_data <- df_clean %>%
  group_by(Country, Year) %>%
  summarise(Volume = sum(Total_Amount_CHF), .groups = 'drop')

# Get ordering by total volume to keep the heat consistent
country_order <- heatmap_data %>%
  group_by(Country) %>%
  summarise(total = sum(Volume)) %>%
  arrange(desc(total)) %>%
  pull(Country)

heatmap_data$Country <- factor(heatmap_data$Country, levels = rev(country_order))

# D. Normal Heatmap (Top 50)
p4 <- ggplot(heatmap_data %>% filter(Country %in% head(country_order, 50)), 
             aes(x = factor(Year), y = Country, fill = Volume)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "magma", labels = label_comma()) +
  theme_minimal() +
  labs(title = "Heatmap: Trade Intensity (Normal Scale)", x = "Year", fill = "Volume (CHF)")

# E. Log-Transform Heatmap (Top 50)
p5 <- ggplot(heatmap_data %>% filter(Country %in% head(country_order, 50)), 
             aes(x = factor(Year), y = Country, fill = Volume)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "magma", trans = "log10", labels = label_comma()) +
  theme_minimal() +
  labs(title = "Heatmap: Log-Transformed Intensity", subtitle = "Reveals stability across the entire network", fill = "Log Volume")

# F. Zoomed Heatmap (Partners 51-100)
p6 <- ggplot(heatmap_data %>% filter(Country %in% country_order[51:100]), 
             aes(x = factor(Year), y = Country, fill = Volume)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "rocket", labels = label_comma()) +
  theme_minimal() +
  labs(title = "Zoom: Middle-Tier Partner Evolution (Ranks 51-100)")

# ---------------------------------------------------------
# EXECUTE: Print all plots
# ---------------------------------------------------------
print(p1); print(p2); print(p3)
print(p4); print(p5); print(p6)
