#SYSTEMIC TRADE INTERCONNECTEDNESS.
  #Cunto, G. & Santana, D. (202X)

#Clear data
rm(list = ls())

#SCRIPT 1: DOWNLOAD DATA-----

#Install and Load Packages------

##Install imfr package (out of CRAN)-----
install.packages("devtools")
require(devtools)
install_github("christophergandrud/imfr")

##Install packages----------
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(imfr)) install.packages("imfr")
if(!require(rdbnomics)) install.packages("rdbnomics")
if(!require(countrycode)) install.packages("countrycode")
if(!require(igraph)) install.packages("igraph")
if(!require(BBmisc)) install.packages("BBmisc")
if(!require(ggraph)) install.packages("ggraph")
if(!require(graphlayouts)) install.packages("graphlayouts")
if(!require(corrplot)) install.packages("corrplot")
if(!require(circlize)) install.packages("circlize")

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")

if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install(version = "3.14")
BiocManager::install("ComplexHeatmap")
if(!require(grid)) install.packages("grid")
if(!require(gridBase)) install.packages("gridBase")
if(!require(rticles)) install.packages("rticles")

##Load packages--------
library(tidyverse)
library(imfr)
library(rdbnomics)
library(countrycode)
library(igraph)
library(BBmisc)
library(ggraph)
library(graphlayouts)
library(corrplot)
library(circlize)
library(ComplexHeatmap)
library(grid)
library(gridBase)
library(rticles)

#Area Codes-----

#DOTS Area
imf_dots_areas <- as_tibble(imf_codes("CL_AREA_DOT")) #Supposedly deprecate dbut works fine

#Add FMI-WEO groups
advanced_economies <- c("AUS", "AUT", "BEL", "CAN", "CYP", "CZE","DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HKG", "ISL", "ISR", "IRL", "ITA", "JPN", "KOR", "LVA", "LTU", "LUX", "MAC", "MLT", "NLD", "NZL", "NOR", "PRT", "PRI", "SMR", "SGP", "SVK", "SVN", "ESP", "SWE", "CHE", "TWN" , "GBR", "USA")
emerging_asia <- c("BGD", "BTN", "BRN", "KHM", "CHN", "FJI", "IND", "IDN", "KIR", "LAO", "MYS", "MDV", "MHL", "FSM", "MNG", "MMR", "NRU", "NPL", 'PLW', "PNG", "PHL", "WSM", "SLV", "LKA", "THA", "TLS", "TON", "TUV", "VUT", "VNM")
emerging_europe <- c("ALB", "BLR", "BIH", "BGR", "HRV", "HUN", "XKX", "MDA", "MNE", "MKD", "POL", "ROU", "RUS", "SRB", "TUR", "UKR")
emerging_latam <- c("ATG", "ARG", "ABW", "BHS", "BRB", "BLZ", "BOL", "BRA", "CHL", "COL", "CRI", "DMA", "DOM", "ECU", "SLV", "GRD", "GTM", "GUY", "HTI", "HND", "JAM", "MEX", "NIC", "PAN", "PRY", "PER", "KNA", "LCA", "VCT", "SUR", "TTO", "URY", "VEN")
emerging_meca <- c("AFG", "DZA", "ARM", "AZE", "BHR", "DJI", "EGY", "GEO", "IRN", "IRQ", "JOR", "KAZ", "KWT", "KGZ", "LBN", "LBY", "MRT", "MAR", "OMN", "PAK", "QAT", "SAU", "SOM", "SDN", "SYR", "TJK", "TUN", "TKM", "ARE", "UZB", "PSE", "YEM")
emerging_ssafrica <- c("AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF", "TCD", "COM", "COG", "COD", "CIV", "GNQ", "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "MDG", "MWI", "MLI", "MUS", "MOZ", "NAM", "NER", "NGA", "RWA", "STP", "SEN", "SYC", "SLE", "ZAF", "SSD", "TZA", "TGO", "UGA", "ZMB", "ZWE")

#Add higher level groups
emerging_economies <- c(emerging_asia, emerging_europe, emerging_latam, emerging_meca, emerging_ssafrica)
imf_weo_countries <- c(advanced_economies, emerging_economies)

#Create country list
data(codelist)

#Check for IMF monitored countries not in list
imf_weo_countries[c(which(imf_weo_countries %in% codelist$iso3c == FALSE))]

#Check for DOTS monitored countries not in list
imf_dots_areas %>% 
  filter(!(codes %in% codelist$iso2c)) %>%
  print(n = nrow(.))

##Create country list----
country_set <- codelist %>%
  select(country.name.en, iso2c, iso3c, imf, continent, region) %>%
  mutate(iso2c = if_else(country.name.en == "Kosovo", "XK", iso2c),
         iso3c = if_else(country.name.en == "Kosovo", "XKX", iso3c),
         continent = if_else(country.name.en == "Kosovo", "Europe", continent)) %>%
  mutate(weo_group_mayor = case_when(iso3c %in% advanced_economies ~ "Advanced Economies",
                                     iso3c %in% emerging_economies ~ "Emerging Economies",
                                     !iso3c %in% imf_weo_countries ~ "WEO Not-Monitored"),
         weo_group_region = case_when(iso3c %in% advanced_economies ~ "Advanced Economies",
                                      iso3c %in% emerging_asia ~ "Emerging and Developing Asia",
                                      iso3c %in% emerging_europe ~ "Emerging and Developing Europe",
                                      iso3c %in% emerging_latam ~ "Latin America and the Caribbean",
                                      iso3c %in% emerging_meca ~ "Middle East and Central Asia",
                                      iso3c %in% emerging_ssafrica ~ "Sub-Saharan Africa",
                                      !iso3c %in% imf_weo_countries ~ "WEO Not-Monitored")) %>%
  filter(!is.na(imf) & !is.na(iso2c), iso2c %in% c(c(imf_codes("CL_AREA_DOT"))$codes))

rm(imf_weo_countries, advanced_economies, emerging_economies, emerging_asia, emerging_europe, emerging_latam, emerging_meca, emerging_ssafrica)
rm(codelist)

##Former countries dataframe----
former_countries <- tibble(country.name.en = c("Former Czechoslovakia", "East Germany", "Serbia and Montenegro", "Netherlands Antilles", "Former U.S.S.R.", "Yemen Arab Rep.", "Yemen, P.D. Rep.", "Former Yugoslavia" ),
                           iso2c = c("CSH", "DE2", "CS", "AN", "SUH","1C_473", "1C_459", "YUC"),
                           iso3c = c("CSH", "DDD", "SCG", "ANT", "SUN", "YAR", "YMD", "YUG"),
                           imf = c(200, 278, 891, 530, 810, "NA", 720, 891),
                           continent = c("Europe", "Europe", "Europe", "Americas", "Europe", "Asia", "Asia", "Europe"),
                           region = c("Europe and Central Asia", "Europe and Central Asia", "Europe and Central Asia", "Latin America & Caribbean", "Europe and Central Asia", "Middle East & North Africa", "Middle East & North Africa",  "Europe and Central Asia"),
                           weo_group_mayor = "Former Countries",
                           weo_group_region = "Former Countries")

##Definitive country set-----
country_set <- rbind(country_set, former_countries)

#Biltaeral Exports Dataset-------

##Set years to import data----
start_year <- 1995
end_year <- 2021

##DOTS Database-----
imf_dots_base <- tibble()

#Vector to keep track of download and resume in case of interruption
countries_leftout <- c("TW")

###Download loop----
for(i in c(country_set$iso2c)[! country_set$iso2c %in% countries_leftout]){
  
  dots <- imf_dataset(database_id = "DOT", 
                      indicator = "TXG_FOB_USD", 
                      ref_area = i, 
                      freq = "A", 
                      start = start_year,
                      end = end_year,
                      return_raw = TRUE) %>% filter(`@FREQ` == "A", `@COUNTERPART_AREA` %in% c(country_set$iso2c)) %>% #CHECK
    mutate(Obs = map(Obs, as.data.frame)) %>% #CHECK
    unnest(Obs) %>%  #CHECK
    mutate(`@REF_AREA` = as.character(`@REF_AREA`)) %>% #Setting country names as characters 
    replace_na(list(`@REF_AREA` = "NA")) #Normal Download 
  
  if ("X..TIME_PERIOD" %in% colnames(dots) | "X.OBS_VALUE" %in% colnames(dots)) { #If conditional to check if there are repeating columns with trade values
    
    col_combine_1 <- c("@TIME_PERIOD","X.TIME_PERIOD") #Names of the columns that will be combined (Time)
    
    col_combine_2 <- c("@OBS_VALUE" , "X.OBS_VALUE" ) #Names of the columns that will be combined (Trade values)
    
    dots <- cbind('@TIME_PERIOD' = na.omit(unlist(dots[,col_combine_1])),
                  dots[,!colnames(dots) %in% col_combine_1, drop = FALSE])  #Combining the columns
    
    dots <- cbind('@OBS_VALUE' = na.omit(unlist(dots[,col_combine_2])),
                  dots[,!colnames(dots) %in% col_combine_2, drop = FALSE]) #Combining the columns
  }
  
  dots <- dots %>% mutate(`@OBS_VALUE` = as.numeric(`@OBS_VALUE`), `@UNIT_MULT` = as.numeric(`@UNIT_MULT`)) %>% #Setting the trade values as numeric variables
    mutate(`@OBS_VALUE_2` = `@OBS_VALUE`*10^(`@UNIT_MULT`)) %>% #Calculating trade values in normal dollars (not millions)
    select(c(`@REF_AREA`,`@COUNTERPART_AREA`,`@TIME_PERIOD`,`@OBS_VALUE_2`)) #Selecting desired columns/data 
  
  
  imf_dots_base <- rbind(imf_dots_base, dots) #Adding the downloaded data to the aggregate dataset
  countries_leftout <- c(countries_leftout,i) #IDK
  rm(dots) #Removing dots
}


##Change column names for ease of use
imf_dots_base <- imf_dots_base %>% 
  left_join(country_set[, c(2,3)], by = c(`@REF_AREA` = "iso2c")) %>%
  left_join(country_set[, c(2,3)], by = c(`@COUNTERPART_AREA` = "iso2c")) %>%
  rename(jurisdiction_iso3 = iso3c.x,
         jurisdiction_iso2 = `@REF_AREA`,
         counterpart_iso3 = iso3c.y,
         counterpart_iso2 = `@COUNTERPART_AREA`,
         year = `@TIME_PERIOD`,
         exports = `@OBS_VALUE_2`) %>%
  drop_na(exports)


##DOTS Database (Taiwan) "Mirror Data"-----
imf_dots_tw <- tibble()
#Vector to keep track of download and resume in case of interruption
countries_leftout_tw <- c("TW")

###Download loop for TW
for(i in c(country_set$iso2c)[! country_set$iso2c %in% countries_leftout_tw]){
  
  dots <- imf_dataset(database_id = "DOT", 
                   indicator = "TMG_CIF_USD", 
                   ref_area = i, 
                   freq = "A", 
                   start = start_year,
                   end = end_year,
                   return_raw = TRUE) %>% filter(`@FREQ` == "A", `@COUNTERPART_AREA` %in% c(country_set$iso2c)) %>% #CHECK
    mutate(Obs = map(Obs, as.data.frame)) %>% #CHECK
    unnest(Obs) %>%  #CHECK
    mutate(`@REF_AREA` = as.character(`@REF_AREA`))  %>% #Setting country names as characters
  replace_na(list(`@REF_AREA` = "NA")) #Normal Download 
  
  if ("X..TIME_PERIOD" %in% colnames(dots) | "X.OBS_VALUE" %in% colnames(dots)) { #If conditional to check if there are repeating columns with trade values
    
    col_combine_1 <- c("@TIME_PERIOD","X.TIME_PERIOD") #Names of the columns that will be combined (Time)
    
    col_combine_2 <- c("@OBS_VALUE" , "X.OBS_VALUE" ) #Names of the columns that will be combined (Trade values)
    
    dots <- cbind('@TIME_PERIOD' = na.omit(unlist(dots[,col_combine_1])),
                  dots[,!colnames(dots) %in% col_combine_1, drop = FALSE])  #Combining the columns
    
    dots <- cbind('@OBS_VALUE' = na.omit(unlist(dots[,col_combine_2])),
                  dots[,!colnames(dots) %in% col_combine_2, drop = FALSE]) #Combining the columns
  }
  
  dots <- dots %>% mutate(`@OBS_VALUE` = as.numeric(`@OBS_VALUE`), `@UNIT_MULT` = as.numeric(`@UNIT_MULT`))   %>% #Setting the trade values as numeric variables
    mutate(`@OBS_VALUE_2` = `@OBS_VALUE`*10^(`@UNIT_MULT`)) %>% #Calculating trade values in normal dollars (not millions)
    select(c(`@REF_AREA`,`@COUNTERPART_AREA`,`@TIME_PERIOD`,`@OBS_VALUE_2`)) #Selecting desired columns/data 
  
  imf_dots_tw <- rbind(imf_dots_tw, dots)
  countries_leftout_tw <- c(countries_leftout_tw,i)
  rm(dots)
  
}

#Change column names for ease of use
imf_dots_tw_summa <- imf_dots_tw %>% filter(`@COUNTERPART_AREA` == "TW") %>% 
  left_join(country_set[, c(2,3)], by = c(`@REF_AREA` = "iso2c")) %>%
  left_join(country_set[, c(2,3)], by = c(`@COUNTERPART_AREA` = "iso2c")) %>%
  rename(jurisdiction_iso3 = iso3c.y,
         jurisdiction_iso2 = `@COUNTERPART_AREA`,
         counterpart_iso3 = iso3c.x,
         counterpart_iso2 = `@REF_AREA`,
         year = `@TIME_PERIOD`, 
         exports = `@OBS_VALUE_2`) %>%
  drop_na(exports)

##DOTS Database (Definitive)------
imf_dots <- tibble()
imf_dots <- rbind(imf_dots_base, imf_dots_tw_summa)

#Inflation adjustment-----

##Import USA GDP Deflator and set 100 to latest year----
usa_gdp_deflator <- imf_dataset(database_id = "IFS",
                                indicator = "NGDP_D_IX",
                                ref_area = "US",
                                freq = "A",
                                start = start_year,
                                end = end_year) %>% as_tibble() %>% 
  mutate(`value` = as.numeric(`value`)) %>% 
  mutate(deflator = value/filter(., date == end_year)$value*100) 

colnames(usa_gdp_deflator)[1] <- "year"


##Transform imf_dots to add inflation adjusted exports----
imf_dots <- imf_dots %>%
  left_join(., usa_gdp_deflator, by = "year") %>%
  mutate(exports_cons = exports/deflator*100) %>%
  select(year, jurisdiction_iso2, counterpart_iso2, exports, exports_cons, jurisdiction_iso3, counterpart_iso3)

