
## Determine which countries have data available and should be run

library(httr)
library(jsonlite)
library(tidyverse)

load("data_req.rda")

# Get list of countries with SDG data
url <- paste0("https://xmart-api-public.who.int/DATA_/RELAY_MAY2023?$apply=groupby((DIM_GEO_CODE_M49,DIM_GEO_CODE_TYPE))")
response <- GET(url)
resultTxt <- content(response, "text")
resultJson <- fromJSON(resultTxt, flatten = T)
m49list<- resultJson$value %>%
  filter(DIM_GEO_CODE_TYPE=="COUNTRY") %>% select(m49code = DIM_GEO_CODE_M49) %>%
  mutate(sdg_data = 1)


# Get list of countries with 3B data
url <- paste0("https://xmart-api-public.who.int/DATA_/RELAY_3B_DATA?$apply=groupby((DIM_GEO_CODE_M49,DIM_GEO_CODE_TYPE))")
response <- GET(url)
resultTxt <- content(response, "text")
resultJson <- fromJSON(resultTxt, flatten = T)
m49list_3b <- resultJson$value %>%
  filter(DIM_GEO_CODE_TYPE=="COUNTRY") %>% select(m49code = DIM_GEO_CODE_M49) %>%
  mutate(tripb_data = 1)


# Get list of countries with LE/HALE data
iso3_lehale <- lehale %>% distinct(iso3) %>%
  mutate(lehale_data = 1)


# Get list of countries with Cause of death data
iso3_cod <- cod21 %>% distinct(DIM_COUNTRY_CODE) %>% rename(iso3 = DIM_COUNTRY_CODE) %>%
  mutate(cod_data = 1)


# Get list of countries with population data
iso3_pop <- pop_age %>%
  mutate(m49code = case_when(
    `Location code`<10 ~ paste0("00", `Location code`),
    `Location code`>=10 & `Location code`<=99 ~ paste0("0", `Location code`),
    TRUE ~ as.character(`Location code`)
  )) %>%
  distinct(m49code) %>%
  mutate(pop_data = 1)


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


# Create master list of countries and which data they have available
data_avail <- country_ref %>% 
  select(iso3 = CODE_ISO_3, m49code, country = NAME_SHORT_EN, WHO_LEGAL_STATUS) %>%
  left_join(m49list) %>%
  left_join(m49list_3b) %>%
  left_join(iso3_lehale) %>%
  left_join(iso3_cod) %>%
  left_join(iso3_pop) %>%
  mutate(all_data = ifelse(sdg_data==1 & tripb_data==1 & lehale_data==1 & cod_data==1 & pop_data==1, 1, 0)) %>%
  mutate(all_data = ifelse(is.na(all_data), 0, 1)) %>% 
  arrange(m49code)


# Save dataset
xlsx::write.xlsx(data_avail, "input/data_availability.xlsx")