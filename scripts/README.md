# Scripts

This folder contains modular R scripts used to build the full analytical pipeline and visualisations for the **Zurich Trade Analysis** project.

## Data Processing & Architecture

- Data cleaning and harmonisation (country matching, aggregation logic)
- Sector consolidation and categorical restructuring
- Active-partner filtering and zero-value removal
- Log-scale preparation and multi-scale transformation
- Largest Remainder allocation for waffle matrices (100-tile constraint)
- Derived metrics (growth rates, volatility, partner rankings)

## Visualisation Modules

- **World maps**: global export/import architecture
- **Growth–volatility landscape**: scatter + bubble encoding
- **Ranking dynamics**: bump chart for top export partners
- **Trade intensity**: multi-facet line plots (sector trends by country)
- **Trade balance & scaling**: normal + log–log symmetry plots
- **Temporal intensity**: partner heatmaps (linear and log-scale views)
- **Regional structural shifts**: radar / spider plots (2016 vs. 2024)
- **Structural fingerprints**: waffle matrices (regional export/import evolution)
- **Continental equilibrium**: butterfly charts (exports vs. imports by region)

Each script is designed to be reproducible and modular, allowing individual components to be tested independently while remaining consistent with the project’s overall analytical framework.
