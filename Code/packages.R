# Package names
packages <- c("tidyverse","janitor","data.table","ggplot2", "readr", "knitr", "stringr","qwraps2","openxlsx","readxl",
              "DBI", "conflicted", "svglite", "DescTools", "wordcloud2", "pacman", "odbc","bookdown",
              "lubridate", "psych", "corrplot", "car", "lubridate", "SMLE", "kableExtra", "rsvg", "flextable","targets",
              "DescTools","tidyr", "parallel", "DiagrammeR", "DiagrammeRsvg", "gt", "gapminder", "spatstat","Hmisc", "RColorBrewer")


#Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
lapply(packages, require, character.only = TRUE)

#---------------------------------------------------------------------------------------------------
# Set conflict prefer 
#---------------------------------------------------------------------------------------------------
conflict_prefer("filter", "dplyr")
#> [conflicted] Will prefer dplyr::filter over any other package
conflict_prefer("lag", "dplyr")
conflict_prefer("first", "dplyr")

#> [conflicted] Will prefer dplyr::lag over any other package
