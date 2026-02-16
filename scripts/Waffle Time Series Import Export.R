# 1. SETUP & LIBRARIES
library(tidyverse)
library(scales)

# 2. LOAD DATA
df <- read_csv("Cleaned_HR_Final_Report.csv")

# 3. COMPREHENSIVE SHORT-NAME MAPPING
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

# Apply mapping and clean
df_clean <- df %>%
  mutate(Ind_Short = recode(Industry, !!!short_names, .default = str_trunc(Industry, 15)))

# 4. MASTER COLOR KEY (Ensures consistency between plots)
all_inds <- sort(unique(df_clean$Ind_Short))
# Use colorRampPalette to generate enough distinct colors from a high-contrast base
master_colors <- setNames(
  colorRampPalette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", 
                     "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62", "#8DA0CB"))(length(all_inds)),
  all_inds
)

# 5. UNIVERSAL WAFFLE ENGINE
generate_waffle <- function(data, trade_type_label) {
  
  waffle_data <- data %>%
    filter(Trade_Type == trade_type_label) %>%
    group_by(Year, Continent, Ind_Short) %>%
    summarise(Total = sum(Total_Amount_CHF), .groups = "drop") %>%
    filter(Total > 0) %>%
    group_by(Year, Continent) %>%
    # Robust Largest Remainder Logic: Guarantees 100 tiles, no negatives
    mutate(Tiles = floor(Total / sum(Total) * 100),
           diff = 100 - sum(Tiles)) %>%
    arrange(desc(Total)) %>%
    mutate(Tiles = if_else(row_number() <= diff, Tiles + 1, Tiles)) %>%
    uncount(Tiles) %>%
    group_by(Year, Continent) %>%
    mutate(x = rep(1:10, length.out = n()),
           y = rep(1:10, each = 10, length.out = n()))
  
  ggplot(waffle_data, aes(x = x, y = y, fill = Ind_Short)) +
    geom_tile(color = "white", linewidth = 0.2) +
    facet_grid(Continent ~ Year) +
    coord_equal() +
    scale_fill_manual(values = master_colors) + # Fixed color map
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 7),
      strip.text.y = element_text(angle = 0, face = "bold", size = 10, color = "#2F4F4F"),
      strip.text.x = element_text(face = "bold", size = 12, color = "#2F4F4F"),
      plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
      plot.margin = margin(15, 15, 15, 15)
    ) +
    labs(title = paste("ZURICH REGIONAL", toupper(trade_type_label), "EVOLUTION"),
         subtitle = "100-Tile Structural Fingerprint (2016-2024) | 1 Square = 1% Market Share",
         fill = "Industry Sector")
}

# 6. RENDER THE DUAL STUDY
plot_export <- generate_waffle(df_clean, "Export")
plot_import <- generate_waffle(df_clean, "Import")

print(plot_export)
print(plot_import)
