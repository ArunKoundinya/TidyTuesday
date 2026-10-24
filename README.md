# Tidy Tuesday

My solutions to the weekly [TidyTuesday](https://github.com/rfordatascience/tidytuesday) data challenge, in R and Python.

**Site:** https://arunkoundinya.github.io/TidyTuesday/

| Week | Analysis | Tool | Charts |
|------|----------|------|--------|
| 29 Oct 2024 | [Monster Movies](https://arunkoundinya.github.io/TidyTuesday/tidy-analysis/29102024/) | R | Dual-axis line, bar charts |
| Bonus | [Crimes Against Women in India](https://arunkoundinya.github.io/TidyTuesday/tidy-analysis/22072024/) | R | Choropleth maps, correlation |
| 21 May 2024 | [Carbon Majors Emissions](https://arunkoundinya.github.io/TidyTuesday/tidy-analysis/21052024/) | R | Stacked area with custom background |
| 23 Apr 2024 | [Objects Launched into Space](https://arunkoundinya.github.io/TidyTuesday/tidy-analysis/23042024/) | R | Line charts, racing bar animation |
| 2 Apr 2024 | [Du Bois Challenge](https://arunkoundinya.github.io/TidyTuesday/tidy-analysis/02042024/) | R | Recreation of Du Bois plate 37 |
| 12 Mar 2024 | [Fiscal Sponsors](https://arunkoundinya.github.io/TidyTuesday/tidy-analysis/12032024/) | Python | Scatter, annotated histograms |
| 5 Mar 2024 | [Trash Wheel Collection](https://arunkoundinya.github.io/TidyTuesday/tidy-analysis/05032024/) | Python | Line charts, heat map |
| 20 Feb 2024 | [R Consortium ISC Grants](https://arunkoundinya.github.io/TidyTuesday/tidy-analysis/20022024/) | Python | Clustering, word clouds |
| 13 Feb 2024 | [Valentine's Day Consumer Data](https://arunkoundinya.github.io/TidyTuesday/tidy-analysis/13022024/) | Python | Line and column charts |
| 30 Jan 2024 | [Groundhog Predictions](https://arunkoundinya.github.io/TidyTuesday/tidy-analysis/30012024/) | Python | Density, line and bar charts |
| 16 Jan 2024 | [US Polling Places](https://arunkoundinya.github.io/TidyTuesday/tidy-analysis/16012024/) | Python | Line chart |
| 14 Nov 2023 | [Diwali Sales Dashboard](https://arunkoundinya.github.io/TidyTuesday/tidy-analysis/14112023/) | R | Quarto dashboard |

## How the site is built

A [Quarto](https://quarto.org/) website. Each analysis lives in `tidy-analysis/<ddmmyyyy>/`.

- `theme.scss` – look and feel, shared with the portfolio, Canvass And Analyze and AI Basics with AK
- `index.qmd` + `_templates/*.ejs` – home page: latest chart, filterable gallery, all-weeks table
- `_filters/tt-meta.lua` – the dataset / tools strip under each title, from front matter
- `scripts/seo.py` – runs after render; canonical URLs and structured data

### Adding a week

Create `tidy-analysis/<ddmmyyyy>/index.qmd` with front matter like:

```yaml
title: "Monster Movies"
subtitle: "One-line takeaway, shown on the card"
date: "2024-10-31"
description: "One or two sentences for search results and link previews."
categories: [R, bar chart]        # first category is the language: R or Python
image: HorrorPlot1.png            # gallery thumbnail
tt-week: "2024-10-29"
dataset: https://github.com/rfordatascience/tidytuesday/blob/main/data/2024/2024-10-29/readme.md
tools: "R · ggplot2"
charts: "Dual-axis line, bar charts"
```

### Rendering and publishing

The Python posts run when rendered; `requirements.txt` pins the library versions they were written against.

```bash
python3.12 -m venv .venv && .venv/bin/pip install -r requirements.txt
export QUARTO_PYTHON=$PWD/.venv/bin/python
quarto publish gh-pages
```

The Diwali dashboard also needs the R packages `tidyverse`, `scales`, `sf`, `gganimate`, `av` and `gt`.
