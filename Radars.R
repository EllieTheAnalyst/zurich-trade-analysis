# 1. SETUP
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("fmsb")) install.packages("fmsb")
library(tidyverse)
library(fmsb)

# 2. LOAD DATA
message("SELECT FILE: Choose 'Cleaned_HR_Final_Report.csv' in the popup...")
# Using file.choose() ensures we get the right path without errors
df <- read_csv(file.choose())

# 3. PREPARE THE EVOLUTIONARY DATA
# Step A: Find the Top 6 global industries by 2024 Export volume
top_6 <- df %>% 
  filter(Trade_Type == "Export", Year == 2024) %>%
  group_by(Industry) %>% 
  summarise(val = sum(Total_Amount_CHF, na.rm=TRUE)) %>% 
  slice_max(val, n = 6) %>% 
  pull(Industry)

# Step B: Summarize percentage share per continent for 2016 and 2024
radar_prep <- df %>%
  filter(Trade_Type == "Export", 
         Year %in% c(2016, 2024), 
         Industry %in% top_6) %>%
  group_by(Continent, Year, Industry) %>%
  summarise(Amount = sum(Total_Amount_CHF, na.rm=TRUE), .groups = 'drop') %>%
  group_by(Continent, Year) %>%
  mutate(Pct = (Amount / sum(Amount)) * 100) %>%
  select(Continent, Year, Industry, Pct) %>%
  pivot_wider(names_from = Industry, values_from = Pct, values_fill = 0)

# 4. PLOTTING THE STUDY
continents <- unique(radar_prep$Continent)
par(mfrow=c(2, 4), mar=c(2, 2, 4, 2)) # Create a grid for all continents

for(cont in continents) {
  # Isolate continent data
  cont_data <- radar_prep %>% filter(Continent == cont) %>% ungroup() %>% select(-Continent, -Year)
  
  # Define the Radar frame (Max 60%, Min 0%)
  radar_final <- rbind(rep(60, 6), rep(0, 6), cont_data)
  
  # Generate Plot
  radarchart(radar_final, 
             title = cont,
             axistype = 1,
             # Red line for 2016 (Baseline), Green area for 2024 (Current)
             pcol = c("#CD5C5C", "#2E8B57"), 
             pfcol = c(NA, rgb(0.18, 0.55, 0.34, 0.4)), 
             plwd = c(2, 3), 
             plty = c(2, 1), 
             cglcol = "grey", cglty = 1, axislabcol = "grey", 
             caxislabels = seq(0, 60, 15), vlcex = 0.7)
  
  # Add a legend to the first plot
  if(cont == continents[1]) {
    legend(x="bottom", legend=c("2016 Baseline", "2024 Current"), 
           pch=20, col=c("#CD5C5C", "#2E8B57"), bty="n", cex=0.8)
  }
}

message("SUCCESS: Your 8-Year Evolution Study is ready.")