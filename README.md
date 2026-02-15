# Zurich Trade Analysis (2016–2024)

This project provides an exploratory structural analysis of the Canton of Zurich’s international trade relationships between 2016 and 2024.

Using R and the tidyverse ecosystem, the study examines export and import patterns through spatial mapping, ranking dynamics, sectoral aggregation, and growth–volatility modelling. The focus lies on identifying structural hierarchies, concentration patterns, and asymmetries in Zurich’s global trade architecture rather than on causal inference.

---

## Research Questions

- How is Zurich spatially integrated into global trade networks?
- How stable are its top trading partners over time?
- Is trade becoming more concentrated or diversified?
- Which partner countries exhibit stable maturity versus dynamic volatility?

---

## Methodological Overview

- Data cleaning and transformation in **R**
- Country and sector-level aggregation
- Log-scaled transformations for cross-country comparability
- Descriptive growth and volatility metrics
- Geospatial mapping using `sf` and `rnaturalearth`
- Reproducible workflow via **R Markdown**

The analysis is explicitly exploratory and descriptive in nature.

---

## Repository Structure

- `zurich_trade_paper.Rmd` – Main reproducible analysis document
- `README.md` – Project description and overview

- `data/`
  - `raw/` – Original datasets
  - `processed/` – Cleaned and aggregated datasets used in analysis

- `scripts/` – Modular R scripts for data processing and visualisations

- `visuals/` – Exported figures used in the paper

- `output/` – Final knitted outputs (e.g. PDF)

---

## Data Source

Canton of Zurich – Statistical Office  
"Warenimporte und -exporte im Kanton Zürich"

Supplementary GDP data were used for contextual interpretation.

---

## Authors

Elena Fuchs  
Divya Shori
