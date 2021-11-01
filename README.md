# A global study of the association between internet adoption and adolescent mental health and well-being

This repository ([GitHub](https://github.com/digital-wellbeing/global-wbmh) / [OSF](https://osf.io/ys7m9/)) contains the code and synthetic datasets required to reproduce all analyses reported in our manuscript, *A global study of the association between internet adoption and adolescent mental health and well-being* (Vuorre & Przybylski, in preparation). These analyses are presented in the [Online analysis supplement](https://digital-wellbeing.github.io/global-wbmh).

## Materials

- [Preprint]()  
  - A publicly available version of our manuscript in advance of peer-review and formal publication
- [GitHub repository](https://github.com/digital-wellbeing/global-wbmh)  
  - A version controlled repository containing all the raw data and code in this project
- [OSF repository](https://osf.io/ys7m9/)  
  - An archived permanent copy of the GitHub repository
- [Online analysis supplement](https://digital-wellbeing.github.io/global-wbmh)
  - The output document of our analyses

## Reproducibility

The code that we used to clean and analyse the data are in `index.Rmd`. To run all the code and compile the resulting document ([the online analysis supplement](https://digital-wellbeing.github.io/global-wbmh)), run `rmarkdown::render("index.Rmd")` in R or click "Knit" in the RStudio IDE.

### Docker

To ensure the reproducibility of our analyses, you can use Docker:

Build the Docker image
```
docker build \
    --build-arg R_VERSION=4.1.1 \
    --build-arg RENV_VERSION=0.14.0 \
    -t global-wbmh .
```

Run the container and render output
```
docker run \
    --rm \
    -v "$(pwd):/home/" \
    -v "/home/renv/library" \
    -e MAX_CORES=1 \
    global-wbmh \
    R -e 'renv::restore(prompt = FALSE); rmarkdown::render("index.Rmd")'
```
