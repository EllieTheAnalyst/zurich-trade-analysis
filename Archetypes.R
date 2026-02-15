# 1. SETUP
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("dendextend")) install.packages("dendextend") # For advanced labeling
library(tidyverse)
library(dendextend)

# 2. LOAD DATA
message("Please select 'Cleaned_HR_Final_Report.csv'...")
df <- read_csv(file.choose())

# 3. PIVOT & NORMALIZE (Market Profile Matrix)
cluster_prep <- df %>%
  drop_na(Country) %>% 
  filter(Trade_Type == "Export", Year == 2024) %>%
  group_by(Country, Industry) %>%
  summarise(Total = sum(Total_Amount_CHF, na.rm = TRUE), .groups = 'drop') %>%
  # Filter for top partners to keep analysis substantial
  filter(Country %in% (df %>% drop_na(Country) %>% group_by(Country) %>% 
                         summarise(t = sum(Total_Amount_CHF)) %>% 
                         slice_max(t, n = 40) %>% pull(Country))) %>%
  pivot_wider(names_from = Industry, values_from = Total, values_fill = 0) %>%
  as.data.frame()

rownames(cluster_prep) <- cluster_prep$Country
cluster_prep$Country <- NULL

# Compare PROFILES (Percentages), not SIZE
cluster_norm <- t(apply(cluster_prep, 1, function(x) x / sum(x)))

# 4. PERFORM HIERARCHICAL CLUSTERING
hc <- hclust(dist(cluster_norm), method = "ward.D2")
dend <- as.dendrogram(hc)

# 5. UNRAVEL THE CLUSTERS (Defining the Archetypes)
# Color the branches to separate the 4 archetypes
dend <- color_branches(dend, k = 4, col = c("#2c3e50", "#e74c3c", "#27ae60", "#f39c12"))

# 6. FINAL PLOT: The Strategic Market Archetypes
par(mar=c(12, 4, 4, 2))
plot(dend, main = "The Global Mirror: Zurich's Strategic Market Archetypes",
     ylab = "Economic Distance (DNA Dissimilarity)")

# Overlay labels for clarity
legend("topright", 
       legend = c("Archetype A: Industrialists", "Archetype B: Tech Hubs", 
                  "Archetype C: Generalists", "Archetype D: Niche/Legacy"), 
       fill = c("#2c3e50", "#e74c3c", "#27ae60", "#f39c12"), bty = "n", cex = 0.8)

message("Archetype Study Generated. Look for countries in the same color group—they share identical business needs.")