# 1. SETUP & LIBRARIES
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("scales")) install.packages("scales")
library(tidyverse)
library(scales)

# 2. DATA LOADING
message("Please select 'Cleaned_HR_Final_Report.csv'...")
df <- read_csv(file.choose())

# 3. ENGLISH TRANSLATION MAPPING
industry_map <- c(
  "Chemische Erzeugnisse" = "Chemical Products",
  "Metallerzeugnisse" = "Metal Products",
  "Datenverarbeitungsgeräte, elektronische und optische Erzeugnisse" = "Electronics & Optical",
  "Elektrische Ausrüstungen" = "Electrical Equipment",
  "Maschinen" = "Machinery",
  "Dienstleistungen des Verlagswesens" = "Publishing Services",
  "Verschiedene (weitere) Güterkategorien" = "Misc. Goods",
  "Metalle" = "Basic Metals",
  "Pharmazeutische Erzeugnisse" = "Pharmaceuticals",
  "Nahrungs- und Futtermittel" = "Food & Feed",
  "Waren, a.n.g." = "Goods n.e.c.",
  "Sonstige Fahrzeuge" = "Other Vehicles",
  "Gummi- und Kunststoffwaren" = "Rubber & Plastics",
  "Papier, Pappe und Waren daraus" = "Paper Products",
  "Bekleidung" = "Wearing Apparel",
  "Leder und Lederwaren" = "Leather Products",
  "Erzeugnisse der Landwirtschaft und Jagd sowie damit verbundene Dienstleistungen" = "Agricultural Products"
)

# 4. PREPARATION & JOINING
chosen_region <- "Europe"

balanced_data <- df %>%
  mutate(Industry = recode(Industry, !!!industry_map)) %>%
  filter(Continent == chosen_region, Year == 2024) %>%
  group_by(Industry, Trade_Type) %>%
  summarise(Value = sum(Total_Amount_CHF, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = Trade_Type, values_from = Value, values_fill = 0) %>%
  mutate(Net_Balance = Export - Import,
         Total_Volume = Export + Import) %>%
  slice_max(Total_Volume, n = 15)

# 5. VIBRANT NEON VISUALIZATION
ggplot(balanced_data, aes(y = reorder(Industry, Net_Balance))) +
  # Dark Background Elements
  theme_dark() +
  geom_vline(xintercept = 0, color = "white", size = 1.2) +
  
  # Imports (Left) - Electric Magenta
  geom_col(aes(x = -Import, fill = "Import (Inflow)"), width = 0.7) +
  
  # Exports (Right) - Vibrant Cyan
  geom_col(aes(x = Export, fill = "Export (Outflow)"), width = 0.7) +
  
  # Net Balance Pulse - Neon Yellow
  geom_point(aes(x = Net_Balance, color = "Trade Equilibrium"), size = 5, shape = 18) +
  
  # Color Configuration
  scale_fill_manual(values = c("Export (Outflow)" = "#00F5FF", "Import (Inflow)" = "#FF00FF")) +
  scale_color_manual(values = c("Trade Equilibrium" = "#FFFF00")) +
  
  # Labels
  scale_x_continuous(labels = function(x) label_number(suffix = " B", scale = 1e-9)(abs(x))) +
  labs(title = paste("ZURICH-", toupper(chosen_region), "TRADE PULSE"),
       subtitle = "Analytical Study of Industry Equilibrium & Market Dominance (2024)",
       x = "Volume in Billion CHF (Import <--- 0 ---> Export)",
       y = "", fill = "Wealth Flow", color = "Profit Marker") +
  
  # Theme Customization
  theme(
    plot.background = element_rect(fill = "#1A1A1A"),
    panel.background = element_rect(fill = "#262626"),
    panel.grid.major.x = element_line(color = "#333333"),
    panel.grid.major.y = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white", face = "bold"),
    plot.title = element_text(size = 20, face = "bold", color = "#00F5FF"),
    legend.position = "bottom",
    legend.background = element_blank()
  )
