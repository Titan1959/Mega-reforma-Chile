# Ejecutar una vez en RStudio. Requiere conexión a CRAN.
paquetes <- c('readxl','dplyr','tidyr','readr','jsonlite','rvest','ggplot2','scales','knitr','rmarkdown')
faltan <- setdiff(paquetes,rownames(installed.packages()))
if(length(faltan)) install.packages(faltan,repos='https://cloud.r-project.org')
