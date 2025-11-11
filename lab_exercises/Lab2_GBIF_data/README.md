# GBIF Distribution Modelling Course

## Overview

This repository contains a modular short course on accessing, cleaning, and mapping GBIF biodiversity occurrence data in R. Each session is delivered as an R Markdown (`.Rmd`) notebook that students can open in RStudio, execute chunk by chunk, and knit to HTML for handouts. Static `.md` and `.html` versions are checked in alongside supporting datasets, images, and exported tables used during the exercises.

If you are new to the material, work through the modules in the sequence below. Each notebook is largely self-contained, but they share example datasets and gradually layer in more advanced workflows, so taking them in order provides the smoothest learning curve.

## Quick Start

1. **Clone or download** this repository to your local machine.
2. **Open RStudio** and set your working directory to the repository root (`Session > Set Working Directory > To Source File Location` after opening a notebook).
3. **Install core packages** listed in `setup.Rmd` (chiefly `rgbif`, `leaflet`, `sf`, `terra`, `tidyverse`).
4. **Open the first notebook** (`setup.Rmd`) and execute chunks manually (most are `eval=FALSE` so you control what runs). Knit to HTML if you want reference notes.

Each notebook references local assets using relative paths, so running from the project root is important.

## Learning Path

- **1. Setup and R Essentials** — `setup.Rmd` / `setup.html`  
  Install libraries, configure RStudio, set working directories, and review handy snippets (clearing the workspace, bounding boxes, file I/O). This primes students for the rest of the workshop.

- **2. Introductory GBIF Access** — `gbif_intro.Rmd` / `gbif_intro.html`  
  Query the GBIF API with `rgbif`, obtain species keys, download occurrences, and visualise point data with `leaflet`. Introduces saving occurrence tables for downstream modelling.

- **3. Expanded Demo and Downloads** — `gbif_demo.Rmd` / `gbif_demo.html`  
  Explores richer search patterns, bounding boxes, multi-species examples, and (new) `occ_download()` workflows that return citable DOIs for student reports. Reinforces data export patterns for tools like MaxEnt.

- **4. Occurrence Data Quality** — `data_quality.Rmd` / `data_quality.html`  
  Loads a curated workbook with intentional data issues, demonstrates column subsetting, spatial joins with `sf`, and visual checks to flag suspect coordinates or mismatched country codes.

- **5. Environment Layers** — `environment.Rmd` / `environment.html`  
  Connects GBIF occurrences with WorldClim climate rasters using `terra` and `geodata`, overlays points, crops/masks rasters, and produces figures suitable for species distribution modelling narratives.

- **6. Mapping Toolkit** — `mapping.Rmd` / `mapping.html`  
  Focuses on cartography: reading/writing occurrence tables, creating publication-ready maps (Norway and South Africa case studies), and experimenting with colour palettes for multi-species displays.

- **7. GBIF SQL Downloads** — `gbif_sql.Rmd` / `gbif_sql.html`  
  Introduces GBIF’s download SQL service for large-scale aggregations; includes template queries, download polling, and import snippets for analytical workflows beyond single-species pulls.

Students can pause after any module—the datasets and scripts are designed so later notebooks can be run independently once prerequisites are installed.

## Repository Layout

- `setup/`, `gbif_intro/`, `gbif_demo/`, `data_quality/`, `environment/`, `mapping/`, `gbif_sql/` — Notebook directories containing `.Rmd`, `.md`, `.html`, and supporting figures.
- `data/` and `environment/` — Cached occurrence exports, shapefiles, rasters, and example text files used throughout the lessons.
- `images/` — Shared artwork (logos, banners) embedded in multiple outputs.
- `mapping/`, `gbif_demo/`, etc. — Subdirectories with rendered PNGs referenced by the manuals.

Each `.Rmd` is the authoritative source; regenerate its `.html` or `.md` counterpart by knitting in RStudio. Images regenerate when code chunks that write PNGs are executed.

## Suggested Teaching Flow

1. **Orientation**: Show `README.md` and `setup.html`.
2. **Hands-on access**: Use `gbif_intro.Rmd` live, then assign `gbif_demo.Rmd` as extended practice (encourage students to request their own DOIs).
3. **Quality assurance**: Walk through `data_quality.Rmd`, highlighting common pitfalls in occurrence data.
4. **Environmental context**: Pair `environment.Rmd` and `mapping.Rmd` for spatial analysis and visualisation techniques.
5. **Scaling up**: Close with `gbif_sql.Rmd` to illustrate enterprise-scale queries and reporting.

## Additional Tips

- **Credentials**: Students need GBIF.org accounts to run `occ_download()` or SQL downloads—store credentials in `GBIF_USER`, `GBIF_PWD`, and `GBIF_EMAIL` environment variables before executing those chunks.
- **Data refresh**: The repository bundles sample exports; re-run downloads periodically to ensure exercises use current GBIF records.
- **Troubleshooting**: Many chunks are marked `eval=FALSE`. Remove the flag or run interactively when ready. Use `?function_name` in R for inline documentation.
- **Extending the course**: Add new notebooks alongside existing modules and link them in this README to keep the pathway up to date.

With this overview you can jump into any notebook knowing how it fits into the broader training sequence. Happy modelling!

