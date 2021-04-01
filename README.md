# mal-effect-calculations
This repository contains scripts for the impact assessment of the MAL Plan.
Learn more about the MAL Plan here: https://www.hsl.fi/hsl/mal

## How to run
1. Clone this repository.
2. In RStudio, select `File > Open Project... > mal-effect-calculations.Rproj`.
3. Set correct environment for `config.yml` file. 
4. Run `renv::restore()`. Now, you are using packages listed in `renv.lock`.

## Data management
Raw data is stored in HSL Sharepoint and needs to be copied to local `/data` folder.
Scripts have relative paths to data directory and sub-folders.

Results from scripts and analysis-friendly tidy data is kept saved to results sub-folder.

Non relative data paths are set `config.yml` file. 
These include for example Helmet-model-system data and result files.

Config file environments are used to distinguish for example different MAL planning versions or other projects. 
Environment is set via `Sys.setenv(R_CONFIG_ACTIVE = "V2030_ve0_rm")` command. 

Pipeline example for data processing is stored in `R/run.R`.

## Version control
We using R-package `renv` that locks versions of packages. When doing analysis first time you should
load packages with command: `renv::restore(repos = "https://cloud.r-project.org", clean = TRUE)`.
If packages are already loaded, use command `renv::restore()`.

R-version is `4.0.4`. This is not changed by `renv`, so you might want to check R/RStudio Global Options.

Further instructions about using `renv` during development and collaboration,
see: https://rstudio.github.io/renv/index.html

## Authors
- Atte Supponen, [atte-wsp](https://github.com/atte-wsp)
- Jens West, [zptro](https://github.com/zptro)
- Johanna Piipponen, [johpiip](https://github.com/johpiip)
