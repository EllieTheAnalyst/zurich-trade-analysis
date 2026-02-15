library(dplyr)
library(ggplot2)
library(scales)

# --------------------------------------------------
# 1) Prepare and guardrail the data
#    Assumes df_plot already exists with:
#    Year, Country, Industry, Total_Amount_CHF
# --------------------------------------------------

df_plot_final <- df_plot %>%
  mutate(
    Year = as.integer(Year),
    Country = factor(Country),
    Industry = factor(Industry)
  ) %>%
  # remove sector–country series that cannot form a trend
  group_by(Country, Industry) %>%
  filter(n_distinct(Year[!is.na(Total_Amount_CHF)]) >= 2) %>%
  ungroup()

# --------------------------------------------------
# 2) Translate industry labels to English
# --------------------------------------------------

df_plot_final <- df_plot_final %>%
  mutate(
    Industry_label = recode(
      as.character(Industry),
      "Chemische Erzeugnisse" =
        "Chemical products",
      "Datenverarbeitungsgeräte, elektronische und optische Erzeugnisse" =
        "Electronics & optical products",
      "Elektrische Ausrüstungen" =
        "Electrical equipment",
      "Maschinen" =
        "Machinery",
      "Metalle" =
        "Metals",
      "Waren, a.n.g." =
        "Other goods (n.e.c.)",
      .default = as.character(Industry)
    ),
    Industry_label = factor(Industry_label)
  )

# --------------------------------------------------
# 3) Harmonious muted palette (dark mode)
# --------------------------------------------------

sector_palette <- c(
  "Chemical products"               = "#E07A5F",
  "Electronics & optical products"  = "#F2CC8F",
  "Electrical equipment"            = "#81B29A",
  "Machinery"                       = "#4EA8DE",
  "Metals"                          = "#8DA9C4",
  "Other goods (n.e.c.)"            = "#B089C6"
)

# --------------------------------------------------
# 4) Dark theme for faceted plots
# --------------------------------------------------

theme_dark_facets <- theme_minimal(base_size = 13) +
  theme(
    plot.background   = element_rect(fill = "#0B0F14", colour = NA),
    panel.background  = element_rect(fill = "#0B0F14", colour = NA),
    legend.background = element_rect(fill = "#0B0F14", colour = NA),
    legend.key        = element_rect(fill = "#0B0F14", colour = NA),
    
    text = element_text(colour = "#E8EEF5"),
    plot.title = element_text(face = "bold", colour = "#E8EEF5"),
    plot.subtitle = element_text(colour = "#B8C2CC"),
    axis.title = element_text(colour = "#E8EEF5"),
    axis.text  = element_text(colour = "#C9D2DC"),
    strip.text = element_text(face = "bold", colour = "#E8EEF5"),
    
    panel.grid.major = element_line(colour = "#1B2631", linewidth = 0.35),
    panel.grid.minor = element_blank(),
    
    legend.position = "bottom",
    legend.title = element_text(
      size = 11,
      face = "bold",
      colour = "#E8EEF5"
    ),
    legend.text = element_text(size = 10),
    
    panel.spacing = unit(1.2, "lines"),
    legend.key.size = unit(0.7, "lines"),
    axis.text.x = element_text(size = 10, margin = margin(t = 6))
  )

# --------------------------------------------------
# 5) X-axis ticks (every 2 years)
# --------------------------------------------------

year_breaks <- seq(
  min(df_plot_final$Year, na.rm = TRUE),
  max(df_plot_final$Year, na.rm = TRUE),
  by = 2
)

# --------------------------------------------------
# 6) Final plot
# --------------------------------------------------

p_sector_trends <- ggplot(
  df_plot_final,
  aes(
    x = Year,
    y = Total_Amount_CHF,
    colour = Industry_label,
    group = Industry_label
  )
) +
  geom_line(linewidth = 1.7, alpha = 0.92, na.rm = TRUE) +
  geom_point(size = 1.4, alpha = 0.75, na.rm = TRUE) +
  facet_wrap(~ Country, scales = "free_y") +
  scale_x_continuous(breaks = year_breaks) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_manual(values = sector_palette, drop = FALSE) +
  labs(
    title = paste0(
      "Zurich ", trade_filter,
      ": Sector Trends by Country (",
      min(df_plot_final$Year, na.rm = TRUE), "–",
      max(df_plot_final$Year, na.rm = TRUE), ")"
    ),
    subtitle = "Top industries by total volume • Facets use free y-scales",
    x = NULL,
    y = "Total amount (CHF)",
    colour = "Industry"
  ) +
  guides(colour = guide_legend(nrow = 3, byrow = TRUE)) +
  theme_dark_facets

p_sector_trends
