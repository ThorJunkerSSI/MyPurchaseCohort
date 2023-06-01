
# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint


# Set target options:
tar_option_set(
  packages = c("tidyverse","janitor","data.table","ggplot2", "readr", "knitr", "stringr","qwraps2","openxlsx","readxl",
               "DBI", "conflicted", "svglite", "DescTools", "wordcloud2", "pacman", "odbc","bookdown",
               "lubridate", "psych", "corrplot", "car", "lubridate", "SMLE", "kableExtra", "rsvg", "flextable",
               "DescTools","tidyr", "parallel", "DiagrammeR", "DiagrammeRsvg", "gt", "gapminder", "spatstat","Hmisc", "RColorBrewer"),
  # packages that your targets need to run
  format = "rds", # default storage format
  error = "null"
  # Set other options as needed.
)

#---------------------------------------------------------------------------------------------------
# Set conflict prefer 
#---------------------------------------------------------------------------------------------------
conflicted::conflict_prefer("select", "dplyr") 
conflicted::conflict_prefer("filter", "dplyr")
#> [conflicted] Will prefer dplyr::filter over any other package
conflicted::conflict_prefer("lag", "dplyr")
conflicted::conflict_prefer("first", "dplyr")
conflicted::conflict_prefer("summarize", "dplyr") 
conflicted::conflict_prefer("summarise", "dplyr") 


# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

#Name the order of priority in datasources
datasource_priority_lists <- c("frida", "purchased_itemname")

#Define the target variables that we want to extract from each datasource
target.vars <- c("itemnumber", "itemname", "productname", "group","custom_group", "KJ", "protein", "fat","carbohydrates","weight", "weight_unit",
                 "food_list","chem_list")

#Specify the names of those target variables in the Frida datasource
old.names_frida <- c("itemnumber", "itemname", "navn",  "gruppe","custom_groups", "energy_kj", "protein", "fat",
                     "available_carbohydrates", NA,  NA, NA, NA)

tar_source("all_functions.R")

# Replace the target list below with your own:
list(
  tar_target(name = cpd_data, command = "../Data/ConsumerReceiptData_sim.csv", format = "file"),
  tar_target(name = matchscore_cutoff, command = 6),
  tar_target(name = frida_match_data, command = file.path("..","Data","Frida.xlsx"), format = "file"),
  ##enrich cpd
  tar_target(
    name = cpd,
    command = cpd_import(cpd_data)),
  tar_target(
    name = cpd2,
    command = fix_weight_in_cpd_amount(cpd)),
  tar_target(
    name = cpd_proddata,
    command = create_weight_volume_eco_country_vars(cpd2)),
  tar_target(
    name = itemname_datasource,
    command = cpd_itemname_measure(cpd_proddata)),
  tar_target(
    name = frida_match,
    command = import_produkt_and_frida_stems(frida_match_data)),
  tar_target(
    name = frida_match2,
    command = enrich_cpd_with_frida_data(frida_match, cpd_proddata)
  ),
 tar_target(
   name = frida_datasource,
   command = match_results(frida_match2, matchscore_cutoff = matchscore_cutoff,
                                old.names = old.names_frida, target.vars = target.vars)
   
 ),
  tar_target(
    name = final_enrich_datasource,
    command = combine_datasources(frida_datasource,itemname_datasource)
  ),
  tar_target(
    name = cpd_enriched,
    command = cpd_join_datasources(cpd_data = cpd_proddata, datasources = final_enrich_datasource)
  ),#,
  tar_target(
    name = cpd_participants,
    command = cpd_join_datasources(cpd_data = cpd2, datasources = final_enrich_datasource)
  ),
  tar_target(
    name = cpd_enriched_final,
    command = cpd_enriched_clean(cpd_enriched = cpd_enriched, cpd_original = cpd2)
  ))