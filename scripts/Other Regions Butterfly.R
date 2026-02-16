# 1. SETUP & LIBRARIES
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("scales")) install.packages("scales")
library(tidyverse)
library(scales)

# 2. DATA PREPARATION & ENGLISH MAPPING
# Ensure 'Cleaned_HR_Final_Report.csv' is selected
message("Please select 'Cleaned_HR_Final_Report.csv'...")
df <- read_csv(file.choose())

industry_map <- c(
  "Chemische Erzeugnisse" = "Chemicals",
  "Metallerzeugnisse" = "Metal Products",
  "Datenverarbeitungsgeräte, elektronische und optische Erzeugnisse" = "Electronics & Optical",
  "Elektrische Ausrüstungen" = "Electrical Equipment",
  "Maschinen" = "Machinery",
  "Pharmazeutische Erzeugnisse" = "Pharmaceuticals",
  "Metalle" = "Basic Metals",
  "Waren, a.n.g." = "Goods n.e.c."
)

# Filtering: 2024, Excluding Europe, and translating industries
global_data <- df %>%
  mutate(Industry = recode(Industry, !!!industry_map)) %>%
  filter(Year == 2024, Continent != "Europe", Continent != "Unknown") %>%
  group_by(Continent, Industry, Trade_Type) %>%
  summarise(Value = sum(Total_Amount_CHF, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = Trade_Type, values_from = Value, values_fill = 0) %>%
  mutate(Net_Balance = Export - Import) %>%
  # Keep the Top 10 industries per continent for clarity
  group_by(Continent) %>%
  slice_max(Export + Import, n = 10) %>%
  ungroup()

# 3. GLOBAL NEON FACET VISUALIZATION
ggplot(global_data, aes(y = reorder(Industry, Net_Balance))) +
  theme_dark() +
  facet_wrap(~Continent, scales = "free_x", ncol = 2) + # Create the global grid
  
  # The Central Pulse Line
  geom_vline(xintercept = 0, color = "white", linewidth = 0.8) +
  
  # Imports (Left) - Electric Magenta
  geom_col(aes(x = -Import, fill = "Import (Inflow)"), width = 0.7) +
  
  # Exports (Right) - Vibrant Cyan
  geom_col(aes(x = Export, fill = "Export (Outflow)"), width = 0.7) +
  
  # Net Balance "Diamond" - Neon Yellow
  geom_point(aes(x = Net_Balance, color = "Net Equilibrium"), size = 3, shape = 18) +
  
  # High-Contrast Color Configuration
  scale_fill_manual(values = c("Export (Outflow)" = "#00F5FF", "Import (Inflow)" = "#FF00FF")) +
  scale_color_manual(values = c("Net Equilibrium" = "#FFFF00")) +
  scale_x_continuous(labels = function(x) label_number(suffix = " B", scale = 1e-9)(abs(x))) +
  
  # Theme and Labels
  labs(title = "ZURICH GLOBAL TRADE PULSE (2024)",
       subtitle = "Comparative Sovereignty Study across Asia, Americas, Africa & Oceania",
       x = "Billion CHF (Import <--- 0 ---> Export)", y = "",
       fill = "Wealth Flow", color = "Profit Anchor") +
  theme(
    plot.background = element_rect(fill = "#1A1A1A"),
    panel.background = element_rect(fill = "#262626"),
    strip.background = element_rect(fill = "#333333"),
    strip.text = element_text(color = "#00F5FF", face = "bold"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white", size = 7),
    legend.position = "bottom",
    plot.title = element_text(color = "#00F5FF", size = 18, face = "bold")
  )
