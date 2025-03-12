# Create datasets
library(tidyverse)
library(whoville)
library(httr)
library(jsonlite)


# GHE Cause of death data --------------------------------------------------------------------------------------

#cod_total <- read.csv("input/data_export_DEX_CMS_GHE_FULL_20241211.csv") %>%  
  # rename(DIM_COUNTRY_CODE = `X...DIM_COUNTRY_CODE`) %>% 

url <- paste0("https://xmart-api-public.who.int/DEX_CMS/GHE_FULL?$filter=DIM_AGEGROUP_CODE%20eq%20'TOTAL'%20and%20DIM_YEAR_CODE%20eq%202021")
response <- GET(url)
resultTxt <- content(response, "text")
resultJson <- fromJSON(resultTxt, flatten = T)
cod21 <- resultJson$value %>% 
  mutate(sex = case_when(
    DIM_SEX_CODE=="TOTAL" ~ "Both sexes", 
    DIM_SEX_CODE=="FEMALE" ~ "Female", 
    DIM_SEX_CODE=="MALE" ~ "Male")) %>%
  mutate(country = iso3_to_names(DIM_COUNTRY_CODE),
         region = iso3_to_regions(DIM_COUNTRY_CODE)) %>% 
  mutate(region2 = case_when(
    region=="WPR" ~ "Western Pacific Region",
    region=="SEAR" ~ "South-East Asia Region",
    region=="EMR" ~ "Eastern Mediterranean Region",
    region=="EUR" ~ "European Region",
    region=="AMR" ~ "Region of the Americas",
    region=="AFR" ~ "African Region")) %>% 
  rename(FLAG_CAUSEGROUP = DIM_CAUSE_GROUP,
         VAL_DEATHS_COUNT_NUMERIC = VAL_DTHS_COUNT_NUMERIC,
         VAL_DEATHS_RATE100K_NUMERIC = VAL_DTHS_RATE100K_NUMERIC)



url <- paste0("https://xmart-api-public.who.int/DEX_CMS/GHE_FULL?$filter=DIM_AGEGROUP_CODE%20eq%20'TOTAL'%20and%20DIM_YEAR_CODE%20eq%202000")
response <- GET(url)
resultTxt <- content(response, "text")
resultJson <- fromJSON(resultTxt, flatten = T)
cod00 <- resultJson$value %>% 
  mutate(sex = case_when(
    DIM_SEX_CODE=="TOTAL" ~ "Both sexes", 
    DIM_SEX_CODE=="FEMALE" ~ "Female", 
    DIM_SEX_CODE=="MALE" ~ "Male")) %>%
  mutate(country = iso3_to_names(DIM_COUNTRY_CODE),
         region = iso3_to_regions(DIM_COUNTRY_CODE)) %>% 
  mutate(region2 = case_when(
    region=="WPR" ~ "Western Pacific Region",
    region=="SEAR" ~ "South-East Asia Region",
    region=="EMR" ~ "Eastern Mediterranean Region",
    region=="EUR" ~ "European Region",
    region=="AMR" ~ "Region of the Americas",
    region=="AFR" ~ "African Region")) %>% 
  rename(FLAG_CAUSEGROUP = DIM_CAUSE_GROUP,
         VAL_DEATHS_COUNT_NUMERIC = VAL_DTHS_COUNT_NUMERIC,
         VAL_DEATHS_RATE100K_NUMERIC = VAL_DTHS_RATE100K_NUMERIC)

# cod21<- cod_total %>% filter(DIM_YEAR_CODE==2021)
# cod00<- cod_total %>% filter(DIM_YEAR_CODE==2000)


# Population data -------------------------------------------------------------------------------------------------

# Population files downloaded from here: https://population.un.org/wpp/downloads?folder=Standard%20Projections&group=Population

pop2023 <- readxl::read_excel("input/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx", sheet = "Estimates", skip=16) %>% 
  filter(Year==2023) %>% 
  select("Index", "Variant", 
         country = "Region, subregion, country or area *", 
         "Location code", 
         iso3 = "ISO3 Alpha-code", 
         iso2 = "ISO2 Alpha-code",                                                                               
         "Type", "Parent code", 
         year = "Year", 
         totalpop_jan = "Total Population, as of 1 January (thousands)", 
         totalpop_jul = "Total Population, as of 1 July (thousands)")

pop2050 <- readxl::read_excel("input/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx", sheet = "Medium variant", skip=16) %>% 
  filter(Year==2050) %>% 
  select("Index", "Variant", 
         country = "Region, subregion, country or area *", 
         "Location code", 
         iso3 = "ISO3 Alpha-code", 
         iso2 = "ISO2 Alpha-code",                                                                               
         "Type", "Parent code", 
         year = "Year", 
         totalpop_jan = "Total Population, as of 1 January (thousands)", 
         totalpop_jul = "Total Population, as of 1 July (thousands)")

pop <- rbind(pop2023, pop2050) %>% 
  mutate(m49code_ = case_when(
    `Location code`<10 ~ paste0("00", `Location code`),
    `Location code`>=10 & `Location code`<=99 ~ paste0("0", `Location code`),
    TRUE ~ as.character(`Location code`)
  ))
# write.csv(pop, "population_estimates.csv")
# cod21 <- cod19
# rm(cod19)

popfem2023 <- readxl::read_excel("input/WPP2024_POP_F02_3_POPULATION_5-YEAR_AGE_GROUPS_FEMALE.xlsx", sheet = "Estimates", skip=16) %>% 
  filter(Year>=2000) %>% 
  mutate(sex="Female")
popfem2050 <- readxl::read_excel("input/WPP2024_POP_F02_3_POPULATION_5-YEAR_AGE_GROUPS_FEMALE.xlsx", sheet = "Medium variant", skip=16) %>% 
  filter(Year<=2050) %>% 
  mutate(sex="Female")

popmal2023 <- readxl::read_excel("input/WPP2024_POP_F02_2_POPULATION_5-YEAR_AGE_GROUPS_MALE.xlsx", sheet = "Estimates", skip=16) %>% 
  filter(Year>=2000) %>% 
  mutate(sex="Male")
popmal2050 <- readxl::read_excel("input/WPP2024_POP_F02_2_POPULATION_5-YEAR_AGE_GROUPS_MALE.xlsx", sheet = "Medium variant", skip=16) %>% 
  filter(Year<=2050) %>% 
  mutate(sex="Male")

pop_age_ <- rbind(popfem2023, popfem2050, popmal2023, popmal2050) %>% 
  select(-c("ISO2 Alpha-code", "SDMX code**", "Notes")) %>%
  rename(iso3 = "ISO3 Alpha-code", country = "Region, subregion, country or area *", year = "Year")

pop_age <- pop_age_ %>% 
  pivot_longer(cols = -c(Index, Variant, country, `Location code`, iso3, Type, `Parent code`, year, sex)) %>% 
  mutate(ageg = ifelse(name %in% c("85-89", "90-94", "95-99", "100+"), "85+", name),
         value = as.numeric(value)) %>% 
  group_by(Index, country, `Location code`, iso3, `Parent code`, year, sex, ageg) %>%
  summarise(total_pop = sum(value)) %>% 
  ungroup() %>% 
  mutate(m49code_ = case_when(
        `Location code`<10 ~ paste0("00", `Location code`),
        `Location code`>=10 & `Location code`<=99 ~ paste0("0", `Location code`),
        TRUE ~ as.character(`Location code`)
        ))

# write.csv(pop_age, "population_estimates_byage.csv")


# LE & HALE data with CI (from GHO) -------------------------------------------------------------------

getdata <- function(ind){
  x <- GET(url = paste("https://ghoapi.azureedge.net/api/", ind, sep=""))
  x2 = fromJSON(rawToChar(x$content))
  x3 <- x2[2]$value
  return(x3)
}

le <- getdata("WHOSIS_000001")
hale <- getdata("WHOSIS_000002") 

lehale <- rbind(le, hale) %>% 
  mutate(sex = case_when(
    Dim1=="SEX_BTSX" ~ "Both sexes",
    Dim1=="SEX_MLE" ~ "Male",
    Dim1=="SEX_FMLE" ~ "Female",
  )) %>% 
  select(-c(TimeDimensionBegin, TimeDimensionEnd, Date, Comments, Dim1, Dim1Type, Dim2Type, Dim2, Dim3Type, Dim3, DataSourceDimType, DataSourceDim, Id)) %>% 
  rename(iso3 = SpatialDim, year = TimeDim) %>% 
  mutate(country = whoville::iso3_to_names(iso3))


# Other datasets -------------------------------------------------------------------------------------

ind_labels <- openxlsx::readWorkbook("input/indicator_labels WHS 2.xlsx")

country_table <- read.csv("input/country_table.csv")


# Save final collection of datasets ------------------------------------------------------------------
save(cod00, cod21, lehale, pop, pop_age, ind_labels, country_table, file = "data_req.rda")
