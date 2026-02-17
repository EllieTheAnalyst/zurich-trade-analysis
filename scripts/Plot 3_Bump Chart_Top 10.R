library(tidyverse)
library(ggrepel)

# --- Load data ---
trade_path <- file.path("data", "1_Total_Countrywise_IndustryWise_YearWise_Export_Import.csv")
stopifnot(file.exists(trade_path))

df <- readr::read_csv(trade_path, show_col_types = FALSE) %>%

trade_focus <- "Export"
top_n <- 10

# --- Aggregate ---
year_country <- df %>%
  filter(Trade_Type == trade_focus) %>%
  group_by(Year, Country) %>%
  summarise(value = sum(Total_Amount_CHF, na.rm = TRUE), .groups = "drop")

# --- Stable country set (appears in top 10 at least once) ---
top_countries <- year_country %>%
  group_by(Year) %>%
  slice_max(value, n = top_n, with_ties = FALSE) %>%
  ungroup() %>%
  distinct(Country) %>%
  pull(Country)

# --- Rank ---
ranked <- year_country %>%
  filter(Country %in% top_countries) %>%
  group_by(Year) %>%
  mutate(rank = dense_rank(desc(value))) %>%
  ungroup() %>%
  filter(rank <= top_n)

# --- Complete years (smooth lines) ---
years_full <- sort(unique(ranked$Year))

ranked_full <- ranked %>%
  select(Year, Country, rank) %>%
  complete(Year = years_full, Country, fill = list(rank = NA_integer_)) %>%
  arrange(Country, Year)

# --- Determine TOP 5 countries by average rank (importance) ---
top5 <- ranked %>%
  group_by(Country) %>%
  summarise(avg_rank = mean(rank, na.rm = TRUE), .groups = "drop") %>%
  arrange(avg_rank) %>%
  slice(1:5) %>%
  pull(Country)

ranked_full <- ranked_full %>%
  mutate(
    importance = if_else(Country %in% top5, "Top 5", "Other"),
    line_width = if_else(importance == "Top 5", 1.9, 0.9)
  )

# --- Colour groups: green / blue / black ---
# Assign countries evenly to 3 groups
countries <- ranked_full %>%
  distinct(Country) %>%
  arrange(Country) %>%
  pull(Country)

color_groups <- rep(c("Green", "Blue", "Black"), length.out = length(countries))
names(color_groups) <- countries

ranked_full <- ranked_full %>%
  mutate(color_group = color_groups[Country])

ranked <- ranked %>%
  mutate(color_group = color_groups[Country])

# --- Saturated palettes (no pastel) ---
palette <- c(
  "Green" = "#1B8A3A",   # deep emerald
  "Blue"  = "#1F4E79",   # steel blue
  "Black" = "#222222"    # soft black
)

# --- Labels (right side only) ---
last_year <- max(years_full)
lab_right <- ranked %>% filter(Year == last_year)

# --- Plot ---
ggplot(ranked_full, aes(x = Year, y = rank, group = Country)) +
  geom_line(
    aes(color = color_group, linewidth = line_width),
    alpha = 0.9,
    na.rm = TRUE
  ) +
  geom_point(
    data = ranked,
    aes(color = color_group),
    size = 2.3
  ) +
  scale_color_manual(values = palette) +
  scale_linewidth_identity() +
  scale_y_reverse(breaks = 1:top_n) +
  guides(color = "none", linewidth = "none") +
  labs(
    title = paste0("Top ", top_n, " trade partners by rank (", trade_focus, ", ", min(years_full), "–", last_year, ")"),
    subtitle = "Three colour groups and line thickness highlight relative importance",
    x = NULL,
    y = "Rank (1 = highest)"
  ) +
  geom_text_repel(
    data = lab_right %>% filter(!is.na(rank)),
    aes(label = Country, color = color_group),
    nudge_x = 0.55,
    direction = "y",
    hjust = 0,
    size = 4,
    segment.alpha = 0.2,
    box.padding = 0.25,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    breaks = years_full,
    expand = expansion(mult = c(0.08, 0.18))
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 60, 10, 60)
  )

