# Smart Study Pattern Analysis & Academic Productivity Prediction

**Student:** Bhukya Pooja · **Roll No.:** 1601-24-771-003  
**Subject:** Introduction to Data Science (IDS) · **Semester:** 5, B.Tech

This R case study examines study patterns, absences, previous failures, and Mathematics performance using the UCI Student Performance Mathematics dataset. It includes a reproducible report and an interactive Shiny dashboard. The source data is observational and concerns Portuguese secondary-school students; it is not CBIT student data.

## Project files

- `app.R` — Shiny dashboard entry point.
- `analysis.R` — shared import, validation, preprocessing, statistics, models, plots, and exports. Both the app and report use this pipeline.
- `student_analysis.Rmd` — reproducible academic report; knitting regenerates the HTML and outputs.
- `student_analysis.html` — previously generated HTML; regenerate from the R Markdown file after running the current pipeline.
- `data/student-mat.csv` — original Mathematics data, semicolon-separated. The analysis does not edit it.
- `figures/` — generated charts.
- `results/` — generated quality, descriptive, correlation, model, coefficient, importance, and prediction tables.

## Dataset

The Mathematics file is part of the UCI Student Performance dataset. The pipeline reads it with `read.csv(..., sep = ";")`, checks required fields, and derives ordered study-time labels from `studytime`: 1 = less than 2 hours, 2 = 2–5 hours, 3 = 5–10 hours, and 4 = more than 10 hours. The target `G3` is the final grade on a 0–20 scale. G1 and G2 are earlier-period grades.

The report and dashboard calculate dimensions, data types, missing values, empty strings, duplicate rows, grade bounds, negative absences, zero grades, and the zero-grade/absence pattern from the actual input file. Zero-grade rows are retained.

## Requirements

R (RStudio-compatible) and these R packages:

- `shiny`
- `ggplot2`
- `dplyr`
- `randomForest`
- `rmarkdown` and `knitr` (for report rendering)

Install missing packages from the RStudio Console:

```r
install.packages(c("shiny", "ggplot2", "dplyr", "randomForest", "rmarkdown", "knitr"))
```

## Open and launch

1. Open the `R_project_student_analysis` folder as the RStudio project/work directory.
2. From the RStudio Console, launch the dashboard:

```r
shiny::runApp()
```

The app reads the dataset via a project-relative path and calculates its metrics during startup. Its tabs cover overview, study patterns, absences/failures, correlations and model interpretation, grade prediction, interactive filters, and findings/limitations.

## Regenerate the report and outputs

From the project root in RStudio:

```r
rmarkdown::render("student_analysis.Rmd", output_file = "student_analysis.html")
```

This sources `analysis.R`, recreates the figures and CSV tables under `figures/` and `results/`, then writes the HTML report. Set the working directory to the project root before rendering. The report and dashboard share the same analysis functions and seeded 80/20 split.

## Models and metrics

The evaluation compares a training-mean baseline, linear regression Model A, linear regression Model B, random-forest Model A, and random-forest Model B. Model A excludes G1 and G2; Model B includes both prior grades. The target G3 is never used as a predictor. Models use a seeded 80/20 split with identical test records; preprocessing is learned on training rows. Metrics are held-out MAE, RMSE, and R². The report and dashboard also show five-fold RMSE with preprocessing refitted in each fold.

Existing `results/model_results.csv` contains previously rendered values, including Model B linear RMSE 2.463 / R² 0.743 and Model B random-forest RMSE 1.972 / R² 0.835. These are historical project outputs, **not verified results from the current shared pipeline**. Re-render the report in R to refresh and verify all result files; do not treat the old values as current until then.

## Interpretation and limitations

- Correlation and group differences are associations, not causal effects.
- G1 and G2 are strong predictors but may not be available for early support decisions.
- One test split does not show that a model is universally best; use cross-validation and external validation for stronger evidence.
- The records do not represent all factors affecting performance or another school's student population.
- The dashboard supports dataset exploration; it is not a validated academic intervention or a tool for identifying real students.

## Troubleshooting

- **Dataset not found:** set the working directory to the project root, and confirm `data/student-mat.csv` exists.
- **Required package missing:** install it with `install.packages("packageName")`, restart R, then rerun.
- **Dashboard reports missing packages:** install `shiny`, `ggplot2`, `dplyr`, and `randomForest`.
- **Report cannot find `analysis.R`:** open the project root in RStudio and render again.
- **Old HTML/results:** render the current R Markdown file to rebuild report outputs.
- **Port in use:** run `shiny::runApp(port = 3839)` or another free local port.
