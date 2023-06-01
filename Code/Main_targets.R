##################################################################
# This is the main script that will execute all the sub-functions 
# and create a summary using R-markdown. 
# This script also has the function to import the newest data-versions
# of all the different data-sources.
##################################################################

##########################
# Set working directory and UserID
##########################
my_directory <- paste0(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(my_directory)


source("packages.R")

## Create folders to save results 
dir.create(file.path("../Data","Validating"))

##########################
# Update datasources (Not available in GitHub Version)
##########################


#####################
#Run Pipeline
#####################
system.time(tar_make())


##############
greedy_product_list <- tar_read(frida_datasource) %>% group_by(productname,group)%>% tally()%>%arrange(-n)

greedy_group_list <- tar_read(frida_datasource) %>% group_by(group)%>% tally()%>%arrange(-n)

##
write.table(greedy_product_list, file = file.path("..","Data","Validating/greedy_product_list.csv"),sep = ";", row.names = FALSE, dec=".")

##
write.table(greedy_group_list, file = file.path("..","Data", "Validating/greedy_list.csv"),sep = ";", row.names = FALSE, dec=".")

