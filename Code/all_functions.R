
#############################
#fix_utf8_and_casing_function
#############################
fix_utf8_and_casing_function<-function(x){
  u<-stringr::str_replace_all(tolower(x),"ã¥|å","aa")
  y<-stringr::str_replace_all(tolower(u),"Ã¼|ü","uu")
  z<-stringr::str_replace_all(tolower(y),"ã¸|ø|õ|ô|ö","oe")
  q<-stringr::str_replace_all(tolower(z),"ã¦|æ|ä","ae")
  q}


#---------------------------------------------------------------------------------------------------
##function that import cpd datasets
##--------------------------------------------------------------------------


cpd_import<-function(cpd_data){
  cpd <- readr::read_csv(cpd_data) %>% 
    rename_all(tolower)%>%  mutate_if(is.character, ~fix_utf8_and_casing_function(.))
  cpd
}

#############################
#Import kemiluppen data (Not available in Github Version)
#############################

##############################################
#Import gs1 and gpc. Then combine the two data (Not available in Github Version)
##############################################

########################################
#Download and edit open food facts data (Not available in Github Version)
########################################


#################################
#Clean the openfoodfacts dataset (Not available in Github Version)
#################################

#############################
#fix_weight_in_cpd_amount
#############################
# Function to fix weight inconsistencies in cpd_amount and aggregate data on receipt level
fix_weight_in_cpd_amount <- function(cpd) {
  
  # Rename all columns to lowercase and calculate necessary variables
  cpd1 <- cpd %>%
    rename_all(tolower) %>%
    mutate(
      is_after_dot = str_detect(amount, "\\.[0-9]|\\.[0-9][1-9]"),
      itemprice = as.numeric(itemprice),
      amount = as.numeric(amount),
      discount = as.numeric(rabat),
      total_price = amount * itemprice,
      itemname = tolower(itemname),
      weight_amount = case_when(
        is_after_dot ~ as.numeric(amount),
        str_detect(itemname, "betjent|vej selv|delikate|koedtorvet|fiskeafdeling|kartofler vaskede|vaegt|fragt") ~ as.numeric(amount),
        str_detect(itemname, "kartofler") & itemprice == 0.01 ~ as.numeric(amount)
      ),
      amount = if_else(is.na(weight_amount), amount, 1),
      total_price = if_else(is.na(weight_amount) | itemprice > 20, total_price, itemprice),
      original_price = total_price - discount
    ) %>%
    # Group by relevant variables and aggregate data on receipt level
    group_by(participant, receipt, itemname, itemnumber, category, itemprice, discount, purchdate, weekno, monthno, yearno, merchantname, weight_amount,
             gender, dateofbirth) %>%
    dplyr::summarise(amount = sum(amount), total_price = sum(total_price), total_original_price = sum(original_price)) %>%
    arrange(participant, amount, purchdate) %>%
    filter(amount != 0)
  
  # Calculate weight to price ratio
  cpd1$weight_price_ratio <- NA_real_
  cpd1$weight_price_ratio[!is.na(cpd1$weight_amount)] <- cpd1$total_price[!is.na(cpd1$weight_amount)] / cpd1$weight_amount[!is.na(cpd1$weight_amount)]
  cpd1$weight_amount[str_detect(cpd1$itemname, "vej( |-)selv( |-)slik") & str_detect(cpd1$merchantname, "Loevbjerg")] <- cpd1$itemprice[str_detect(cpd1$itemname, "vej( |-)selv( |-)slik") & str_detect(cpd1$merchantname, "Loevbjerg")] / 100
  
  # Calculate total order price value per receipt
  receipts1 <- cpd1 %>%
    group_by(participant, receipt, purchdate) %>%
    dplyr::summarise(totalorderpricevalue = sum(total_price))
  
  cpd2 <- left_join(cpd1, receipts1) %>%
    mutate(
      itemnumber = str_replace_all(itemnumber, "-|_", ""),
      gtin_length = str_count(itemnumber, "[:digit:]"),
      itemname = tolower(itemname)
    ) %>% 
    filter(!(amount<0| gtin_length>14| itemnumber==""| itemnumber=="BLANK"|itemnumber=="VOUCHER"| itemname=="" |itemname==" " | itemprice<0))%>%
    filter(is.na(weight_amount)|weight_amount>0) %>%##exclude returns not on the same receipt, remove 0, and product with no GTIN possibly include gtin_length<2, gtin_length>14
    mutate(itemnumber_fixed=paste0(str_flatten(rep(0,(14-gtin_length)),collapse=""),itemnumber),
           itemnumber=itemnumber_fixed)%>% select(-itemnumber_fixed)
  
  cpd2 
}

#---------------------------------------------------------------------------------------------------
##Enrich consumerdata from product names and create combined product database with variables derived directly from product names
#---------------------------------------------------------------------------------------------------

create_weight_volume_eco_country_vars<-function(cpd2){
  ##create dataset with only product names and itemnumbers for name match
  cpd_match_name <- cpd2%>% 
    group_by(itemname, itemnumber) %>% dplyr::summarize(count_item=1, total_items=sum(amount))%>%arrange(desc(total_items))

  ##extract and remove dosage information and filter less relevant merchants
  ## create product dataset where each product is unique. product dataset contains aggregated purchase frequencies as well for later use.
  ## code be improved even more with dosage and pack combos for i.e. gajol, and a few other changes beer carlsb. 1883
  #cpd_proddata<- cpd_match_name%>%
  suppressWarnings({cpd_proddata<- cpd_match_name%>%
    mutate(
      newname= str_squish(itemname),
      newname=str_replace(newname,"(\\&)","og"),
      newname=str_replace(newname,"(a38)","acidophilus tykmaelk"),
      newname=str_replace(newname,"(\\?)","1/2"),
      newname=str_replace(newname,"\\b1883\\b","33cl"),
      newname=str_replace(newname,"REMA 1000|\\?|^ |_|\\?|\\&|vaegt",""),
      newname=str_replace(newname,"(\\#)",""),
           ## detect eco products from the product name
           eco= case_when(
             str_detect(newname,"(konventionel)|(\\bikke oe.o[:alpha:]*\\b)")~ "non eco", 
             str_detect(newname,"\\boe.o[:alpha:]*\\b")~ "eco", 
             TRUE                      ~ "unknown"),
           ## detect country from the product name
           country=case_when(
             str_detect(newname,"(^|\\s+)d(k|ansk(e|))(\\s+|$)")~ "dk", 
             str_detect(newname,"(^|\\s+)udl(\\.|and(|ske))(\\s+|$)")~ "other",
             str_detect(newname,"(^|\\s+)tysk(|land(e|ske|))(\\s+|$)")~ "other", 
             TRUE                      ~ "unknown"),
           ##remove eco and country from name
           newname=str_replace(tolower(newname),"(^|\\s+)(d(k|ansk(e|))|udl(\\.|and(|ske))|tysk(|land(e|ske|)))\\b|(loese|kugle konventionel)|
                               (\\bikke oe.o\\b|\\boe.o\\b)",""),
           ##get information about fat % or alcohol % or product type from products
           product_type_raw=str_extract(tolower(newname), "\\b\\d{1,4}\\s*[.,\\/]?\\s*\\d{0,4}%?\\b|\\b[smlx](\\/)[smlx]?\\b"),
           product_type_meassure=str_extract(product_type_raw, "\\d{1,4}\\s*[.,\\/]?\\s*\\d{0,4}"),
           product_type_unit=str_replace(product_type_raw, "\\d{1,4}\\s*[.,\\/]?\\s*\\d{0,4}", ""),
           ## get information about packages i.e. 6 pak, number of products sold i.e. 20 aeg etc.
           pieces_raw=str_extract(tolower(newname), "(?<=\\b|^|\\s)\\d{1,4}(\\s+|)(x|\\*|fl|st[yk]|pcs|ds\\.|bk|bakke|[-](pk|pa[:alpha:]k|pack[:alpha:]|pak)|\\?s|rl)(?=\\s|\\b|$)"),
           pieces=ifelse(str_detect(tolower(newname), "aeg") & str_detect(tolower(newname), "\\b(([smlx](\\/)[smlx]?)|([smlx]))\\b|$"),
                         str_extract(tolower(newname), "(?<=\\b|^|\\s)\\d{1,4}"),
                         str_extract(pieces_raw, "(?<=\\b|^|\\s)\\d{1,4}")),
           ##remove the information already extracted above
           newname=str_replace(newname, "\\b\\d{1,4}\\s*(x|\\*|fl|st[yk]|pcs|hb|bk|pk|pak|pack|\\?s|rl|[smlx](\\/)[smlx]?)[\\d\\s.,\\/]*[%+]?\\b|([smlx](\\/)[smlx]?)(\\s+|$)\\d{1,4}\\s*(x|\\*|fl|st[yk]|pcs|hb|bk|pk|pak|pack|\\?s|rl|[smlx](\\/)[smlx]?)", ""),
           newname=str_replace(newname, "(?<=\\b|^|\\s)\\d{1,4}(\\s+|)(x|\\*|fl|st[yk]|pcs|bk|[-](pk|pak|pack|pak)|\\?s|rl|[smlx](\\/)[smlx]?)", ""),
           ## identify multipliers
           ##multiplier=str_extract(tolower(newname),"[[:digit:]]{1,4}(s+|)(x|\\*|pk|-|\\/)(s+|)[[:digit:]]{1,4}((\\.|\\,|x|-|\\*|\\/)([[:blank:]]|)([[:digit:]]{1,4})|)"),
           number_name=str_extract(tolower(newname), "(?<=\\b|^|\\s)\\d{1,4}(\\s*[.,\\/]?\\s*\\d{0,4})?\\s*(kg\\.|kg|kilo|kil|g[ra]{0,2}m?\\.|
                                   l(ite|iter)?\\.|cl|ml|fl|st[yk]|pcs|hb|bk(,|\\.|\\s|\\b)|[-]|(pk\\.|pak|pack|\\?s|rl)|t[b]|mc|xl|[smlx]{1}(\\/[smlx]{1})?)?(?=\\s|\\b|$)"),
           number_name = gsub(",", ".", gsub("\\.", "", number_name)),
           ##extract not weight units here
           dosage=str_extract(tolower(newname), "(?<=^|\\s|\\b)(\\d+(\\.|,|x|-|\\*|\\/)*\\d*\\s*mg)(?=\\s|$|\\.|,|\\b)"),
           unit_raw=str_extract(tolower(number_name), "(kg\\.|kg|kilo|kil|g[ra]{0,2}m?\\.|l(ite|iter)?\\.|cl|ml|fl|st[yk]|pcs|hb|bk(,|\\.|\\s|\\b)|[-]|(pk\\.|pak|pack|\\?s|rl)|t[b]|mc|xl|[smlx]{1}(\\/[smlx]{1})?)(?=$|\\.|\\b)"),
           measure_raw=str_remove(tolower(number_name), "(kg\\.|kg|kilo|kil|g[ra]{0,2}m?\\.|l(ite|iter)?\\.|cl|ml|fl|st[yk]|pcs|hb|bk(,|\\.|\\s|\\b)|[-]|(pk\\.|pak|pack|\\?s|rl)|t[b]|mc|xl|[smlx]{1}(\\/[smlx]{1})?)(?=$|\\.|\\b)"),
           measure_raw=str_replace(measure_raw,"x","*"), ##replaces x with * to calculate measure once numeric
           newname=str_replace(tolower(newname), "(?<=\\b|^|\\s)\\d{1,4}(\\s+|)([.,\\/]|x|-|\\*|\\/|\\+|%)?\\s*\\d{0,4}?([.,\\/]|x|-|\\*|\\/|\\+|%)?(\\s+|$)|
                               (?<=\\b|^|\\s)\\d{1,4}(\\s+|)(x|\\*|fl|st[yk]|pcs|bk|[-]|(pk|pak|pack|\\?s|rl)(\\.|)|[smlx](\\/[smlx])?)(\\s+|$)|
                               [.,\\-\\/*\\+%]|\\b(m|i|vores|med|salling|gestus|og|til|a|c|s|first|loegismose)\\b", ""),
           newname= str_squish(newname),
           adjustment=case_when( 
             str_detect(unit_raw,"(cl)")~ "10", 
             str_detect(unit_raw,"(kg\\.|kg|kilo|kil)")~ "1000", 
             str_detect(unit_raw,"(^)(l(ite|iter|\\..|))")~ "1000", 
             str_detect(unit_raw,"(ml)")~ "1", 
             str_detect(unit_raw,"(%|\\+)")~ "1", 
             TRUE                      ~ "1"),
           measure_raw=str_squish(measure_raw),
           measure= case_when(
             str_detect(measure_raw,"\\/") ~ as.numeric(str_extract_all(measure_raw,"[[:digit:]]{1,4}(s+|)")[[1]][1])/as.numeric(str_extract_all(measure_raw,"[[:digit:]]{1,4}(s+|)")[[1]][2]), 
             str_detect(measure_raw,"\\*")~ as.numeric(str_extract_all(measure_raw,"[[:digit:]]{1,4}(s+|)")[[1]][1])*as.numeric(str_extract_all(measure_raw,"[[:digit:]]{1,4}(s+|)")[[1]][2]), 
             ## we take the mean of the ranges i.e. 1100-1200g
             str_detect(measure_raw,"-")~ (as.numeric(str_extract_all(measure_raw,"[[:digit:]]{1,4}(s+|)")[[1]][1])+as.numeric(str_extract_all(measure_raw,"[[:digit:]]{1,4}(s+|)")[[1]][2]))/2,
             is.na(measure_raw) ~ as.numeric(NA)),
           final_unit=case_when( ##not converted yet!)
             str_detect(unit_raw,"(cl)")~ "volume_ml", 
             str_detect(unit_raw,"(g(r|ra|ram|\\.|)|(kg\\.|kg|kilo|kil))")~ "weight_g", 
             str_detect(unit_raw,"(ml|cl|l(ite|iter|\\.|))")~ "volume_ml", 
             str_detect(unit_raw,"(bk(\\,|\\.|\\s| |))")~ "bakke_box", 
             TRUE                      ~ "unknown"),
           adjustment=as.numeric(adjustment))%>%
    rename(name=newname)})
  
  cpd_proddata
  
} 

cpd_itemname_measure <- function(cpd_data) {
  cpd_data1 <- cpd_data %>%
    rename(pieces_count = pieces, weight_value = measure) %>%
    mutate(weight_value = as.character(weight_value),
           pieces_count = case_when(str_detect(itemname, "aeg") & is.na(pieces_count) ~ as.numeric(weight_value),
                                    is.na(pieces_count) ~ as.numeric(pieces_count),
                                    TRUE ~ as.numeric(pieces_count)),
           total_weight_value = as.numeric(weight_value) * as.numeric(pieces_count),
           total_weight = as.character(case_when(str_detect(unit_raw, "(^|\\s)g(r|ra|ram|m|\\.|)") ~ total_weight_value,
                                                 str_detect(unit_raw, "(kg\\.|kg|kilo|kil)") ~ total_weight_value * 1000,
                                                 str_detect(unit_raw, "\\b(l(ite|iter|\\.|))") ~ total_weight_value * 1000,
                                                 str_detect(unit_raw, "cl") ~ total_weight_value * 10,
                                                 str_detect(unit_raw, "ml") ~ total_weight_value,
                                                 TRUE ~ total_weight_value)),
           weight_unit = case_when(str_detect(unit_raw, "(cl)") ~ "ml",
                                   str_detect(unit_raw, "(g(r|ra|ram|m|\\.|)|(kg\\.|kg|kilo|kil))") ~ "grm",
                                   str_detect(unit_raw, "(ml|cl|l(ite|iter|\\.|))") ~ "ml"),
           datasource = "purchased_itemname") %>%
    select(itemname, itemnumber, pieces_count, weight_value, total_weight, weight_unit, datasource)
  
  cpd_data1
}
#---------------------------------------------------------------------------------------------------
##import frida stems and nutricient information from frida 
#---------------------------------------------------------------------------------------------------

import_produkt_and_frida_stems<-function(frida_match_data){
  
  # Read frida_nutrient_data.xlsx and preprocess the data
  frida_nutrient <- read_excel(file.path("..", "Data", "frida_nutrient_data.xlsx"), sheet = "frida_nutrient_data") %>%
    rename_all(tolower) %>%
    mutate_all(fix_utf8_and_casing_function) %>%
    mutate(
      across(where(is.character), tolower),  # Convert character columns to lowercase
      across(where(is.character), str_squish),  # Remove excess whitespace
      across(everything(), ~str_replace(., "^na$", "empty"))  # Replace "na" with "empty"
    ) %>%
    rename(foodgroup_eng = foodgroup)  # Rename the 'foodgroup' column to 'foodgroup_eng'
  
  # Read frida_match_data and preprocess the data
  frida_match1 <- read_excel(frida_match_data, sheet = "Word_stems (18042023)") %>%
    rename_all(tolower) %>%
    mutate_all(fix_utf8_and_casing_function) %>%
    mutate(
      across(where(is.character), tolower),  # Convert character columns to lowercase
      across(where(is.character), str_squish),  # Remove excess whitespace
      across(everything(), ~str_replace(., "^na$", "empty"))  # Replace "na" with "empty"
    )
  
  # Join frida_match1 with frida_nutrient based on matching columns
  frida_match <- frida_match1 %>%
    left_join(frida_nutrient, by = c("nummer" = "foodid"))
  
  frida_match
}

#---------------------------------------------------------------------------------------------------
##enrich_with_frida_data
#---------------------------------------------------------------------------------------------------

#############################
#match_names_function
#############################

enrich_cpd_with_frida_data <- function(frida_match, cpd_proddata){
  
  ##Create output dataset for match function to iterate to iterate over
  frida_stems <- c(5, 6, 6, 3, 2, 2, 0.1, 0.1, 0.1 ,2 , 0.1)
  names(frida_stems) <- c("brand_group_stem","brand_specific_stem","generic_specific_stem","generic_group_stem",
                          "generic_type_stem","generic_taste_stem","specific_taste_stem","processing_stem","eco_stem", "light_stem","unit")
  
  
  ##The function takes a preproccessed product name, the original full itemname and matches to a number of predifined word stems
  match_names_function <- function(i, cpd_proddata, frida_match, frida_stems){
     match_db <- apply(frida_match[,names(frida_stems)], 2, function(x){
      str_count(cpd_proddata$name[i], x)
    })
    
    match_frida <- bind_cols(frida_match %>% 
                                select(priority, lower_percent_match,upper_percent_match),
                              cpd_proddata[i,] %>% 
                                select(itemname,product_type_unit,product_type_meassure)) %>%
      mutate_all(~str_replace((.),"empty","0"))%>%
      mutate(
        priority = as.numeric(priority),
        percent_match=case_when(
          lower_percent_match == 0 & upper_percent_match == 0 ~ 0,
          product_type_meassure>= lower_percent_match & product_type_meassure<upper_percent_match ~ 1,
          TRUE ~ 0))
    
    #bind to matrix
    new_match_db<-cbind(match_db, match_frida$priority,match_frida$percent_match)
    
    ## update frida stems with the two new stems
    frida_stems <- c(5,6,6,3,2,2,0.1,0.1,0.1,2,0.1,0.1,0.1)
    
    
    weighted_matches <- as.matrix(new_match_db) %*% frida_stems
    #The match in frida with the highest score
    row_select <- which.max(weighted_matches)
    

    frida_match2 <- frida_match[row_select,] %>% 
      mutate(match_score = max(weighted_matches, na.rm = TRUE),
             itemname = cpd_proddata$itemname[i],
             itemnumber = as.character(cpd_proddata$itemnumber[i]),
             name = cpd_proddata$name[i],
             total_items = cpd_proddata$total_items[i],
             frida_name = navn) %>%
      dplyr::select(-navn, itemname, itemnumber, name, match_score, total_items,frida_name , everything() )
    
    frida_match2
  }
  
  #Parallel this step since it can be time consuming. It is currently running with 3 cores 
  #but can easily be adjusted to work with more.
  cl <-  parallel::makeCluster(3)
  clusterEvalQ(cl, {library(dplyr); library(stringr)})
  
  cpd_frida <- rbindlist(parLapply(cl, 1:nrow(cpd_proddata), match_names_function, cpd_proddata = cpd_proddata, frida_match = frida_match,
                                   frida_stems = frida_stems))
  
  cpd_frida
}

#############################
#match_results
#############################

match_results<-function(cpd_frida1, matchscore_cutoff, old.names, target.vars){
  
  #Only accept matches with a score above a certain limit
  low_matched_prod<-cpd_frida1%>% filter(match_score<matchscore_cutoff)
  
  ##
  write.table(low_matched_prod, file = file.path("..","Data/Validating/low_matched_prod.csv"),sep = ";", row.names = FALSE, dec=".")
  
  ## find most frequent problems (greedy)
  low_matched_names<-cpd_frida1%>% mutate(count_var=1)%>%filter(match_score<matchscore_cutoff) %>% group_by(name)  %>%
    summarise(total_products_matched=sum(count_var),total_items_matched=sum(total_items))%>% arrange(-total_items_matched)
  
  ## save contacts pairs
  write.table(low_matched_names, file = file.path("..", "Data/Validating/low_matched_name_for_stems.csv"), sep = ";", row.names = FALSE, dec=".")
  
  ##find most frequent unmatched words
  unique_low_matched<-as.data.frame(table(unlist(str_split(low_matched_prod$name," ")))) %>% arrange(-Freq)
  
  ## save contacts pairs
  write.table(unique_low_matched, file = file.path("..", "Data/Validating/low_matched_words.csv"), sep = ";", row.names = FALSE, dec=".") 
  
  
  
  #Rename data column so they match with other datasources
  cpd_frida2 <- cpd_frida1 %>%
    filter(match_score >= matchscore_cutoff) %>%
    rename_with( ~ target.vars[which(!is.na(old.names_frida))], .cols = old.names_frida[!is.na(old.names_frida)]) %>%
    dplyr::select(target.vars[!is.na(old.names_frida)]) %>%
    mutate(datasource = "frida")
  
  cpd_frida2
}

#---------------------------------------------------------------------------------------------------
##add_gpc_data_to_gs1 (Not available in Github version)
#---------------------------------------------------------------------------------------------------



#---------------------------------------------------------------------------------------------------
## prepare measure dataset to enable exposure quantification
#---------------------------------------------------------------------------------------------------
combine_measures <-function(data){
  ## prepare measure dataset to enable exposure quantification
  cpd_measures1<-data %>%  
    #select(itemnumber,itemname,pieces,total_items,measure,netweight_gs1,netweightmeasurementunitcode_gs1,unit_raw,bruto_vaegt,
    #kilde_gs1,kilde_frida,kilde_kemiluppen,foodname_frida, foedevarenavn_frida)%>%
    rename(pieces1=pieces)%>%
    mutate(pieces=case_when(
      (str_detect(itemname,"aeg")&is.na(pieces1))~ as.numeric(measure), 
      is.na(pieces1)~ as.numeric(pieces1),
      TRUE~as.numeric(pieces1)),
      ##                                      replace_na(pieces1,"1"))%>%
      measure_adjusted1=as.numeric(measure)*as.numeric(pieces),
      bruto_adjusted=(as.numeric(bruto_vaegt))*as.numeric(pieces),
      netweight_gs1=str_replace(netweight_gs1,"\\.",""),
      measure_adjusted=case_when(
        str_detect(unit_raw,"(^|\\s)g(r|ra|ram|m|\\.|)")~ measure_adjusted1, 
        str_detect(unit_raw,"(kg\\.|kg|kilo|kil)")~ measure_adjusted1*1000,
        str_detect(unit_raw,"\\b(l(ite|iter|\\.|))")~ measure_adjusted1*1000,
        str_detect(unit_raw,"cl")~ measure_adjusted1*10, 
        str_detect(unit_raw,"ml")~ measure_adjusted1, 
        TRUE~measure_adjusted1),
      weight1=case_when(
        !is.na(netweight_gs1)~ as.numeric(netweight_gs1),
        !is.na(measure) & is.na(netweight_gs1)~ measure_adjusted1,
        is.na(measure) & is.na(netweight_gs1)&!(bruto_vaegt=="empty")~ bruto_adjusted),
      weight_unit2=case_when(
        !is.na(netweightmeasurementunitcode_gs1)~ netweightmeasurementunitcode_gs1,
        !is.na(unit_raw)& is.na(netweightmeasurementunitcode_gs1)~ unit_raw,
        !(bruto_vaegt=="empty")& is.na(unit_raw)& is.na(netweightmeasurementunitcode_gs1)~ "grm"),
      items_total=total_items*as.numeric(pieces),
      weight_unit1=tolower(weight_unit2),
      weight_unit=case_when( 
        str_detect(weight_unit1,"(cl)")~ "ml", 
        str_detect(weight_unit1,"(g(r|ra|ram|m|\\.|)|(kg\\.|kg|kilo|kil))")~ "grm", 
        str_detect(weight_unit1,"(ml|cl|l(ite|iter|\\.|))")~ "ml"))%>% select(-measure_adjusted1,-weight_unit2,-weight_unit1)
  
  table(cpd_measures1$unit_raw)
  
  cpd_measures1}

## join to full dataset to get get one price, number of items, and weight volume for each purchase

combine_datasources <- function(...){
  
  combined_datasources <- do.call("bind_rows", lapply(list(...), function(x) mutate(x, across(everything(),as.character))))
  combined_datasources <- type_convert(combined_datasources)
  combined_datasources
}



### Combine datasources and standardize their group names. Then for each product select GS1 information if available, else Open Food Facts and 
#lastly frida. 
cpd_enriched_clean <- function(cpd_enriched,cpd_original){
  
  #Perform the mapping
  cpd1 <- cpd_enriched %>% 
    group_by(itemname, itemnumber) %>% 
    arrange(factor(datasource, levels = c("gs1", "openfoodfacts", "frida","purchased_itemname")), desc(datasource)) %>% 
    slice(1)
  
  cpd_original2 <- cpd_original %>% left_join(cpd1 %>% mutate(itemnumber = as.character(itemnumber)), by = c("itemname", "itemnumber")) %>%
    #dplyr::select(-pieces.x, -pieces.y, -dosage, -unit_raw, -measure_raw, -measure, -final_unit, -adjustment, -name, -total_items, -count_item,
    #              -gtin_length,-weight_amount,-weight_unit, -weight_price_ratio, -totalorderpricevalue, -product_type_meassure, -product_type_unit,
    #              -pieces_raw) %>%
    dplyr::select(-gtin_length,-product_type_meassure, -product_type_unit,-dosage, -unit_raw, -measure_raw, -measure, -final_unit,
                  -adjustment, -name, -total_items, -count_item,-pieces_raw) %>%
    relocate(any_of(c("participant", "itemnumber", "itemname", "productname", "group", "custom_group")))
  
  write.csv(cpd_original2, "../Data/ConsumerReceiptData_Enriched.csv", row.names =FALSE)
  
  return(cpd_original2)
  
}


cpd_join_datasources <- function(cpd_data, datasources){
  
  #Split datasources based on whether it should match on itemnumber or itemname
  datasources_list <- split(datasources, datasources$datasource %in% c("frida","purchased_itemname"))
  
  cpd_enriched_itemname <- datasources_list[["TRUE"]] %>% group_by(itemname) %>% slice(1) %>% ungroup() %>%
    dplyr::select(-itemnumber) %>%
    right_join(cpd_data, by = c("itemname"))
  
  
  cpd_enriched <- cpd_enriched_itemname
  cpd_enriched
}


