# A global study of the association between internet adoption and adolescent mental health and well-being

This repository ([GitHub](https://github.com/digital-wellbeing/global-wbmh) / [OSF](https://osf.io/ys7m9/)) contains the code and synthetic datasets required to reproduce all analyses reported in our manuscript, *A global study of the association between internet adoption and adolescent mental health and well-being* (Vuorre & Przybylski, in preparation). These analyses are presented in the [Online analysis supplement](https://digital-wellbeing.github.io/global-wbmh).

## Materials

- [Preprint](https://doi.org/10.31234/osf.io/9tbjy)  
  - A publicly available version of our manuscript in advance of peer-review and formal publication
- [GitHub repository](https://github.com/digital-wellbeing/global-wbmh)  
  - A version controlled repository containing all the raw data and code in this project
- [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7004054.svg)](https://doi.org/10.5281/zenodo.7004054)
  - An archived permanent copy of the GitHub repository

## Reproducibility

The analyses were conducted in R, and can be reproduced locally:

1. Clone the [github repo](https://github.com/digital-wellbeing/global-wbmh)
  - Terminal: `git clone https://github.com/digital-wellbeing/global-wbmh.git`  
      OR
  - RStudio: File -> New Project -> Version Control -> Git -> use the URL from above

2. Prepare the R environment
  - Terminal: `Rscript -e "renv::restore()"`  
      OR
  - RStudio: Click renv -> Restore Library in the Packages panel

3. Render the source file `ms.Rmd`
  - Terminal: `Rscript -e 'rmarkdown::render("ms.Rmd")'`  
      OR
  - RStudio: Open the file and click Knit/Render

If you encounter problems, please [open an issue](https://github.com/digital-wellbeing/global-wbmh/issues).


The project repo includes the GBD dataset, code to download the ITU dataset, and a synthetic mock version of the GWP dataset to enable reproducing all our computations. **The models take several hours/days each to run**---depending on your local computing resources---and therefore the rendering process can take several days. For this reason, the build will fail after having cleaned the data. Then, run `models.R` with settings specific to your environment/cluster. Once that is done you can render the source file again and it should work.
