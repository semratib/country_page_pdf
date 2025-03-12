#####################################
### Run reports ####

library(tidyverse)

# Get country reference dataset
response <- GET("https://xmart-api-public-uat.who.int/REFMART/REF_COUNTRY")
resultTxt <- content(response, "text")
resultJson <- fromJSON(resultTxt, flatten = T)
country_ref <- resultJson$value %>%
  mutate(m49code = case_when(
    CODE_ISO_NUMERIC<10 ~ paste0("00", CODE_ISO_NUMERIC),
    CODE_ISO_NUMERIC>=10 & CODE_ISO_NUMERIC<=99 ~ paste0("0", CODE_ISO_NUMERIC),
    TRUE ~ as.character(CODE_ISO_NUMERIC)
  ))

data_avail <- xlsx::read.xlsx("input/data_availability.xlsx", sheetName = "Sheet1")

final_m49list <- data_avail %>% 
  filter(all_data==1 & WHO_LEGAL_STATUS=="M") %>% 
  pull(m49code)

## 183 Member states have all data available
# For the 11 Member states with missing data for LE and COD there is an alternate version of the report - see below 

# Broke up the running of the reports to spot check and fix things as needed
test_m49list = final_m49list[151:183]
test_m49list = final_m49list[3]

for (i in test_m49list){
  
  m49code <- i
  country_iso <- country_ref %>% filter(m49code==i) %>% pull(CODE_ISO_3)
  country_ <- country_ref %>% filter(m49code==i) %>% pull(NAME_SHORT_EN)
  
  message(country_)
  
  param_list <- list(
    m49code = m49code,
    country_ = country_,
    country_iso = country_iso
  )
    
  country_nopunct <- str_remove_all(country_, " | [:punct:]")
  
  output_file <-  paste0("output/", m49code, "_", country_nopunct,".pdf")
  
  rmarkdown::render("report.Rmd", output_file = output_file,
                    params = param_list)
  
}


# FYI Long country names that might run off the first page
# c("862", "826", "583", "418", "408", "364", "180", "068"))


## Alternate version for countries without LE and COD data (usually small population member states) 
# run this for c("020", "212", "184", "492", "520", "570", "584", "585",  "659", "674", "798")
for (i in c("020")){
  
  m49code <- i
  country_iso <- country_ref %>% filter(m49code==i) %>% pull(CODE_ISO_3)
  country_ <- country_ref %>% filter(m49code==i) %>% pull(NAME_SHORT_EN)
  
  message(country_)
  
  param_list <- list(
    m49code = m49code,
    country_ = country_,
    country_iso = country_iso
  )
  
  # country_nopunct <- str_remove_all(country_, " | [:punct:]")
  
  output_file <-  paste0("output/", m49code, "_", country_,"_test.pdf")
  
  rmarkdown::render("report_version_smallpop.Rmd", output_file = output_file,
                    params = param_list)
  
}



