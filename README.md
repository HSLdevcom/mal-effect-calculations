# mal-effect-calculations

This repository contains scripts for the impact assessment of the MAL Plan.
Learn more about the MAL Plan here on
[our website](https://www.hsl.fi/hsl/mal).

## Usage

1. Collect input data:
    - Forecast data and model results from
      [`helmet-model-system`](https://hsldevcom.github.io/helmet-ui/)
    - Spatial data for some of the assessments and visualisations (please
      review `Helmet-ja-mittariajot\LUEMINUT.txt` in MAL Teams)
1. Clone this repository.
1. In RStudio, select `File > Open Project... > mal-effect-calculations.Rproj`.
1. Set correct paths in `config.yml` file. 
1. Run `renv::restore()`. Now, you are using packages listed in `renv.lock`.
1. Create `\data`, `\figures`, and `\results` folders.
1. To assess normal model runs, run `scripts/figures/run.R`.
1. To assess agent model runs, run `scripts/accessibility/run.R`.

## Data management

Paths to directories and files inside this repository are referenced relatively
via `here` package. Paths to external input data are defined in `config.yml`.
These include for example helmet-model-system data and result files.

Spatial input data in stored in `data` folder. 

Results from scripts and analysis-friendly tidy data are saved to `results`
folder. Plots, graphs, and maps are saved to `figures` folder.

Assessment scenarios are controlled in `scenarios.tsv`. Agent model scenarios
are defined in `config.yml` and they can be changed in
`scripts/accessibility/run.R`.

## Version control

We using R-package `renv` that locks versions of packages. When doing analysis
first time you should load packages with command:
`renv::restore(repos = "https://cloud.r-project.org", clean = TRUE)`. If
packages are already loaded, use command `renv::restore()`.

R version is set in
[`renv.lock`](https://github.com/HSLdevcom/mal-effect-calculations/blob/main/renv.lock)
and is currently `4.3.0`. This is not changed by `renv`, so you might want to
check R/RStudio Global Options.

Further instructions about using `renv` during development and collaboration
can be found on [their website](https://rstudio.github.io/renv/index.html).

## Authors

- Atte Supponen, [atte-wsp](https://github.com/atte-wsp)
- Jens West, [zptro](https://github.com/zptro)
- Johanna Piipponen, [johpiip](https://github.com/johpiip)
