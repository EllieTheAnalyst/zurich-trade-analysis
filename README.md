# Zurich Trade Analysis (2016–2024)

This project provides a structural exploratory analysis of the Canton of Zurich’s international trade relationships between 2016 and 2024.

Using R and the tidyverse ecosystem, the study examines export and import architectures through multi-scale visual analytics. Rather than focusing on causal inference, the project investigates structural hierarchies, sectoral concentration, trade symmetry, and resilience patterns within Zurich’s global trade network.

---

## Research Focus

- How is Zurich spatially embedded within global trade systems?
- How stable are its top trading partners over time?
- Does export specialisation increase structural concentration risk?
- How diversified is Zurich’s import architecture?
- Are trade relationships proportionally symmetric across partner size?

---

## Analytical Architecture

The analysis combines several complementary perspectives:

- **Spatial mapping** (global export and import architecture)
- **Growth–volatility modelling** (scale vs stability trade-offs)
- **Ranking dynamics** (hierarchical persistence of top partners)
- **Sectoral time-series analysis** (multi-facet structural intensity)
- **Log–log symmetry modelling** (export–import proportionality)
- **Temporal heatmaps** (network persistence and shock sensitivity)
- **Radar plots** (regional structural shifts, 2016 vs 2024)
- **Waffle matrices** (100-tile zero-sum structural allocation)
- **Largest Remainder method** for discrete proportional accuracy

The workflow is fully reproducible via **R Markdown**, integrating data processing, modelling logic, and visualisation in a unified pipeline.

---

## Methodological Characteristics

- Data cleaning and harmonisation in **R**
- Country and sector-level aggregation
- Log-scale transformations for cross-country comparability
- Descriptive growth and volatility metrics
- Multi-scale analytical framework (absolute, logarithmic, zoom)
- Geospatial mapping using `sf` and `rnaturalearth`
- Structural allocation modelling for waffle matrices

The study is explicitly descriptive and exploratory in nature.

---

## Repository Structure

- `zurich_trade_paper.Rmd` – Main reproducible analysis document
- `README.md` – Project overview

- `data/`
  - `raw/` – Original datasets
  - `processed/` – Cleaned and aggregated datasets

- `scripts/` – Modular R scripts for processing and visualisations

- `visuals/` – Exported figures used in the paper

- `output/` – Final knitted outputs (PDF)

---

## Data Source

Canton of Zurich – Statistical Office  
"Warenimporte und -exporte im Kanton Zürich"

Supplementary GDP data were incorporated for contextual interpretation.

---

## Authors

Elena Fuchs  
Divya Shori

