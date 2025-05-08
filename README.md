# Gender discrepancies at the Inquisitorial trial in Giaveno, Italy, 1335

[![R language](https://img.shields.io/badge/language-R-blue)](https://www.r-project.org/)
[![Stata language](https://img.shields.io/badge/language-Stata-blue)](https://www.stata.com/)
[![ERC funding](https://img.shields.io/badge/funding-ERC-green)](https://cordis.europa.eu/project/id/101000442)
[![Research Council of Finland funding](https://img.shields.io/badge/funding-Research_Council_of_Finland-green)](https://research.fi/en/results/funding/81442)

This repository contains the data and code to replicate the results of the article:
- Salihović, D., & Estévez, J. L. (forthcoming) 'Gender Bias in Medieval Inquisitions and Its Place in Shaping the Knowledge About the Heterodox'. _Social Science History_.

## Purpose of the repository

This repository aims to provide transparency and reproducibility for the research conducted on the Inquisitorial trial in Giaveno, Italy, in 1335. 
It includes all data and code used in the analysis presented in the article.
For an overview of the data, see [here](https://github.com/joseluisesna/Heretic_women_in_Giaveno_1335/blob/main/data/README.md).

## Software requirements

The R code in this repository reproduces the descriptive analysis, network simulation, and Cox regression models from the article. 
The analysis was conducted using R version 4.5.0 in RStudio 2024.12.1+563. 
The following R packages (with their respective versions) were used:
- [data.table](https://rdatatable.gitlab.io/data.table/) (1.17.0)
- [tidyr](https://tidyr.tidyverse.org/) (1.3.1)
- [dplyr](https://dplyr.tidyverse.org/) (1.1.4)
- [scales](https://scales.r-lib.org/) (1.4.0)
- [ggplot2](https://ggplot2.tidyverse.org/) (3.5.2)
- [ggpubr](https://rpkgs.datanovia.com/ggpubr/) (0.6.0)
- [ggstatsplot](https://indrajeetpatil.github.io/ggstatsplot/) (0.13.0)
- [igraph](https://r.igraph.org/) (2.1.4)
- [netseg](https://mbojan.github.io/netseg/) (1.0-3)
- [MatchIt](https://kosukeimai.github.io/MatchIt) (4.7.1)
- [cobalt](https://ngreifer.github.io/cobalt/) (4.6.0)
- [pracma](https://cran.r-project.org/web/packages/pracma/index.html) (2.4.4)
- [WRS2](https://cran.r-project.org/web/packages/WRS2/index.html) (1.1-6)
- [survey](http://cran.fhcrc.org/web/packages/survey/index.html) (4.4-2)
- [survival](https://github.com/therneau/survival) (3.8-3)
- [broom](https://broom.tidymodels.org/articles/broom.html) (1.0.8)
- [bshazard](https://cran.r-project.org/web/packages/bshazard/index.html) (1.2)
- [modelsummary](https://modelsummary.com/) (2.3.0)
- [coxme](https://cran.r-project.org/web/packages/coxme/index.html) (2.2-22)
- [splines](https://www.rdocumentation.org/packages/splines/versions/3.6.2) (4.5.0)
- [performance](https://easystats.github.io/performance/) (0.13.0)
- [sjPlot](https://strengejacke.github.io/sjPlot/) (2.8.17)
- [rstanarm](https://github.com/stan-dev/rstanarm/tree/feature/survival) (2.36.0.9000)
- [brms](https://paulbuerkner.com/brms/index.html) (2.22.0)
- [bayesplot](https://mc-stan.org/bayesplot/index.html) (1.12.0)
- [parallel](https://stat.ethz.ch/R-manual/R-devel/library/parallel/doc/parallel.pdf) (4.4.1)
- [posterior](https://cran.r-project.org/web/packages/posterior/index.html) (1.6.1)

The accelerated-failure time (AFT) models were estimated in StataNow 19:
- [Stata](https://www.stata.com/)

## File list

The repository includes four R scripts, one Stata script, and a data folder:
- [0_Castellario_gender_theme.R](https://github.com/joseluisesna/Heretic_women_in_Giaveno_1335/blob/main/0_Castellario_gender_theme.R)
- [1_Castellario_network_analyses.R](https://github.com/joseluisesna/Heretic_women_in_Giaveno_1335/blob/main/1_Castellario_network_analyses.R)
- [2_Castellario_person_day_data_observation.R](https://github.com/joseluisesna/Heretic_women_in_Giaveno_1335/blob/main/2_Castellario_person_day_data_observation.R)
- [3_Castellario_Cox_models.R](https://github.com/joseluisesna/Heretic_women_in_Giaveno_1335/blob/main/3_Castellario_Cox_models.R)
- [4_Castellario_AFT_models.do](https://github.com/joseluisesna/Heretic_women_in_Giaveno_1335/blob/main/4_Castellario_AFT_models.do)
- [data](https://github.com/joseluisesna/Heretic_women_in_Giaveno_1335/tree/main/data)

## Instructions for use

- Ensure you have the required version of R and RStudio installed.
- Install the necessary packages using the specified versions.
- Load the data from the data/data.RData file.
- Run the R scripts in the order listed to replicate the analysis and results.

- Optionally, ensure you have StataNow version 18, 18.5 or 19 installed.
- Load the data3.csv from the project directory.
- Run the provided Stata .do file to estimate the accelerated failure-time (AFT) models. 

## Source

The data was collected from Merlo, Grado G. 1977. _Eretici e inquisitori nella società piemontese del Trecento: con l’edizione dei processi tenuti a Giaveno dall’inquisitore Alberto De Castellario (1335) e nelle Valli di Lanzo dall’inquisitore Tommaso Di Casasco (1373)_. Turin: Claudiana Editrice. 
The original Latin manuscript is held at the Archives of the Order of Preachers in Rome (Archivio generale dell’Ordine dei Predicatori, Rome, MS II.64, ff. 1r-111v).

## Funding

This research received support from the European Research Council (ERC), under the European Union’s Horizon 2020 research and innovation program (grant agreement No. 101000442, project [“Networks of Dissent: Computational Modelling of Dissident and Inquisitorial Cultures in Medieval Europe”](https://cordis.europa.eu/project/id/101000442)).
José Luis Estévez was also supported by the Research Council of Finland (AKA) (grant numbers [360022](https://research.fi/en/results/funding/81442) and [364382](https://research.fi/en/results/funding/81092)).

## Citation

- Salihović, D., & Estévez, J. L. (forthcoming) 'Gender Bias in Medieval Inquisitions and Its Place in Shaping the Knowledge About the Heterodox'. _Social Science History_.

## Contact information

For any questions, please contact:
- Davor Salihović (davor.salihovic@gmail.com)
- José Luis Estévez (joseluisesna@gmail.com)
