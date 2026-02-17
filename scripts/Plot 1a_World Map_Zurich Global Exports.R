library(tidyverse)
library(sf)
library(rnaturalearth)
library(countrycode)
library(scales)

# ---- 1) Paths ----
trade_path <- file.path("data", "1_Total_Countrywise_IndustryWise_YearWise_Export_Import.csv")
stopifnot(file.exists(trade_path))

# ---- 2) Load + clean ----
df <- read_csv(trade_path, show_col_types = FALSE) %>%
  mutate(
    Trade_Type = tolower(Trade_Type),
    Country = str_squish(Country),
    Year = as.integer(Year)
  )

# ---- 3) Multi-pass ISO helper ----
to_iso3_multi <- function(x) {
  iso_en <- countrycode(x, origin = "country.name", destination = "iso3c")
  iso_de <- countrycode(x, origin = "country.name.de", destination = "iso3c")
  coalesce(iso_en, iso_de)
}

# ---- 4) Aggregate totals by country (across all years) ----
totals_country <- df %>%
  group_by(Trade_Type, Country) %>%
  summarise(Total_CHF = sum(Total_Amount_CHF, na.rm = TRUE), .groups = "drop") %>%
  mutate(iso3 = to_iso3_multi(Country)) %>%
  mutate(
    iso3 = case_when(
      str_detect(Country, regex("france|frankreich|french", ignore_case = TRUE)) ~ "FRA",
      str_detect(Country, regex("russia|russian federation", ignore_case = TRUE)) ~ "RUS",
      str_detect(Country, regex("south korea|korea,? republic", ignore_case = TRUE)) ~ "KOR",
      str_detect(Country, regex("iran", ignore_case = TRUE)) ~ "IRN",
      str_detect(Country, regex("viet ?nam", ignore_case = TRUE)) ~ "VNM",
      TRUE ~ iso3
    )
  )

# ---- 5) World map + robust join key ----
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(4326) %>%
  mutate(iso_join = coalesce(iso_a3_eh, iso_a3))

# ---- 6) Zurich point ----
zurich <- tibble(lon = 8.5417, lat = 47.3769) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# ---- 7) Warm palette ----
WARM_COLORS <- c("#120c02", "#5c2e00", "#c46a00", "#ffd166", "#fff4cc")

# ---- 8) Smart legend labels (10 B, 100 M, etc.) ----
smart_chf <- function() {
  lab <- label_number(accuracy = 0.1, scale_cut = cut_short_scale())
  function(x) {
    out <- lab(x)
    gsub("([0-9])([KMBT])$", "\\1 \\2", out)
  }
}

# ---- 9) Dark theme ----
theme_dark_map <- function() {
  theme_void(base_size = 12) +
    theme(
      plot.background  = element_rect(fill = "#0b0f14", color = NA),
      panel.background = element_rect(fill = "#0b0f14", color = NA),
      
      plot.title = element_text(color = "white", face = "bold", size = 18),
      plot.subtitle = element_text(color = "#D7D7D7", size = 12),
      
      legend.position = "right",
      legend.background = element_rect(fill = "#0b0f14", color = NA),
      legend.key = element_rect(fill = "#0b0f14", color = NA),
      
      legend.title = element_text(color = "white", face = "bold", size = 12),
      legend.text  = element_text(color = "white", size = 11),
      
      plot.margin = margin(12, 12, 12, 12)
    )
}

# ---- 10) EXPORT map (same style as your import map) ----
trade_type <- "export"

dat_export <- totals_country %>%
  filter(Trade_Type == trade_type) %>%
  filter(!is.na(iso3)) %>%
  group_by(iso3) %>%
  summarise(Total_CHF = sum(Total_CHF, na.rm = TRUE), .groups = "drop") %>%
  mutate(Total_CHF = ifelse(Total_CHF <= 0, NA_real_, Total_CHF))

map_export <- world %>%
  left_join(dat_export, by = c("iso_join" = "iso3"))

p_export <- ggplot(map_export) +
  geom_sf(aes(fill = Total_CHF), color = "#1b2330", linewidth = 0.15) +
  
  # Zurich glow + dot
  geom_sf(data = zurich, shape = 21, size = 7.0, stroke = 0, alpha = 0.12, fill = "white") +
  geom_sf(data = zurich, shape = 21, size = 3.2, stroke = 0.7, fill = "white", color = "#0b0f14") +
  
  scale_fill_gradientn(
    colours = WARM_COLORS,
    trans = "log10",
    labels = smart_chf(),
    na.value = "#0b0f14",
    name = "Total CHF\n(log10)"
  ) +
  
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  
  labs(
    title = "Zurich Global Exports — World Map",
    subtitle = "Countries shaded by total trade volume (aggregated across years)"
  ) +
  theme_dark_map() +
  guides(
    fill = guide_colorbar(
      barheight = unit(110, "pt"),
      barwidth  = unit(14, "pt"),
      ticks.colour = "white",
      frame.colour = "white"
    )
  )

# ---- 11) Display ----
p_export
