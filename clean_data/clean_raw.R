#============================================#
# MDRO carriage amongst returning travellers #
#              Clean raw dataset             #
#============================================#

#clean environment 
rm(list = ls())

# set working directory
setwd('~/Documents/nBox/git_projects/TRAVELER-main/')

# load libraries
library(WDI)
library(dplyr)
library(stringr)
library(RCurl)

############################
# load raw data 
dat.raw = read.csv('~/Documents/nBox/git_projects/TRAVELER-main/data/combined.csv', sep = ',')

############################
# cleaning 

#### make all characters caps 
dat.raw[, unlist(apply(dat.raw, 2, is.character))] = apply(dat.raw[, unlist(apply(dat.raw, 2, is.character))], 2, toupper)
#### remove all spaces before and after characters
dat.raw[, unlist(apply(dat.raw, 2, is.character))] = apply(dat.raw[, unlist(apply(dat.raw, 2, is.character))], 2, str_trim)

#### rename dataframe to more intuitive names 
colnames(dat.raw) = c('study_id', 'author_yr', 'outcome_type', 'outcome_def', 'outcome_value', 'outcome_unit', 'travel_yr_value', 'travel_yr_unit', 'destination_codeORincomelvl', 
                      'destination_name', 'destination_region_raw', 'sample_size', 'sample_type', 'sample_size_sqrt', 'resistance_type_specify', 'comment', 'origin_name')

dat.raw$outcome_type[grep('ACQUISITION RATE OF RETURNING TRAVELLER', dat.raw$outcome_type)] = 'human_acquisition_origin'
dat.raw$outcome_type[grep('ANIMAL', dat.raw$outcome_type)] = 'animal_prevalence_destination'
dat.raw$outcome_type[grep('COMMUNITY', dat.raw$outcome_type)] = 'human_prevalence_destination'
dat.raw$outcome_type[grep('PERSISTANCE RATE OF RETURNING TRAVELLER', dat.raw$outcome_type)] = 'human_persistence_origin'

dat.raw$resistance_type = NA
dat.raw$resistance_type[grep('MDR (ESBL AMPC CPR)', dat.raw$resistance_type_specify)] = '3GCR_CR'
dat.raw$resistance_type[grep('ESBL', dat.raw$resistance_type_specify)] = '3GCR'
dat.raw$resistance_type[grep('CMY', dat.raw$resistance_type_specify)] = '3GCR'
dat.raw$resistance_type[grep('CTX', dat.raw$resistance_type_specify)] = '3GCR'
dat.raw$resistance_type[grep('SHV', dat.raw$resistance_type_specify)] = '3GCR'
dat.raw$resistance_type[grep('MOX', dat.raw$resistance_type_specify)] = '3GCR'
dat.raw$resistance_type[grep('CEFEPIME', dat.raw$resistance_type_specify)] = '3GCR'
dat.raw$resistance_type[grep('AMPICILLIN', dat.raw$resistance_type_specify)] = '3GCR'
dat.raw$resistance_type[grep('CEFTRIAXONE', dat.raw$resistance_type_specify)] = '3GCR'
dat.raw$resistance_type[grep('CEFTAZIDIME', dat.raw$resistance_type_specify)] = '3GCR'
dat.raw$resistance_type[grep('CEFOTAXIME', dat.raw$resistance_type_specify)] = '3GCR'
dat.raw$resistance_type[grep('THIRD GEN CEPHALOSPORIN', dat.raw$resistance_type_specify)] = '3GCR'
dat.raw$resistance_type[grep('FLUORO', dat.raw$resistance_type_specify)] = 'FLQR'
dat.raw$resistance_type[grep('NALIDIXIC ACID', dat.raw$resistance_type_specify)] = 'FLQR'
dat.raw$resistance_type[grep('CIPROFLOXACIN', dat.raw$resistance_type_specify)] = 'FLQR'
dat.raw$resistance_type[grep('TETRA', dat.raw$resistance_type_specify)] = 'TTXR'
dat.raw$resistance_type[grep('CHLORAMPHENICOL', dat.raw$resistance_type_specify)] = 'CHLRPHR'
dat.raw$resistance_type[grep('TRIMETHOPRIM_SULFAMETHOXAZOLE', dat.raw$resistance_type_specify)] = 'BCTR'
dat.raw$resistance_type[grep('GENTAMICIN', dat.raw$resistance_type_specify)] = 'AMNGR'
dat.raw$resistance_type[grep('MDR', dat.raw$resistance_type_specify)] = 'unspecified'

#### remove ' in travel years
dat.raw$travel_yr_value = gsub("'", "", dat.raw$travel_yr_value)

#### destination names are sometimes continents, sometimes countries 
#### expand this into country, region, then match this to world bank data to get GDP 
countries = WDI_data$country[,c('iso2c', 'country', 'region')]
countries = as.data.frame(apply(countries, 2, toupper))
colnames(countries) = c('country_code', 'country_name', 'region')
countries = countries[-which(countries$region == 'AGGREGATES'),]

dat.raw$destination_country = NA
dat.raw$destination_country_code = NA
dat.raw$destination_region = NA

dat.raw$destination_name[grep('EGYPT', dat.raw$destination_name)] = "EGYPT, ARAB REP."
dat.raw$destination_name[grep('CARIBBEAN', dat.raw$destination_name)] = "LATIN AMERICA & CARIBBEAN"

for (i in 1:nrow(dat.raw)) {
  destination = dat.raw$destination_name[i]
  row = which(countries$country_name == destination)
  if (length(row > 0)) {
    dat.raw$destination_country[i] = countries$country_name[row]
    dat.raw$destination_country_code[i] = countries$country_code[row]
    dat.raw$destination_region[i] = countries$region[row]
  } else {
    # do nothing 
  }
}

# table(dat.raw$destination_name[is.na(dat.raw$destination_country)]) # regions unable to match to world back countries 

####clean up the regions which don't have countries 
dat.raw$destination_region[grep('BOLIVIA', dat.raw$destination_name)] = 'LATIN AMERICA & CARIBBEAN'
dat.raw$destination_region[grep('LATIN AMERICA & CARIBBEAN', dat.raw$destination_name)] = 'LATIN AMERICA & CARIBBEAN'
dat.raw$destination_region[grep('EAST ASIA & PACIFIC', dat.raw$destination_name)] = 'EAST ASIA & PACIFIC'
dat.raw$destination_region[grep('EUROPEAN UNION', dat.raw$destination_name)] = 'EUROPEAN UNION'
dat.raw$destination_region[grep('SOUTH EUROPE', dat.raw$destination_name)] = 'EUROPEAN UNION'
dat.raw$destination_region[grep('EU', dat.raw$destination_name)] = 'EUROPEAN UNION'
dat.raw$destination_region[grep('ASIA', dat.raw$destination_name)] = 'ASIA' # not able to match to GDP because not a region/ country in world bank 
dat.raw$destination_region[grep('ASIA OUTSIDE SEA', dat.raw$destination_name)] = 'ASIA OUTSIDE SEA'  # not able to match to GDP because not a region/ country in world bank 
dat.raw$destination_region[grep('ASIA SOUTHEAST', dat.raw$destination_name)] = 'EAST ASIA & PACIFIC (EXCLUDING HIGH INCOME)'
dat.raw$destination_region[grep('CENTRAL EUROPE AND THE BALTICS', dat.raw$destination_name)] = 'CENTRAL EUROPE AND THE BALTICS'
dat.raw$destination_region[grep('EUROPE & CENTRAL ASIA (EXCLUDING HIGH INCOME)', dat.raw$destination_name)] = 'EUROPE & CENTRAL ASIA (EXCLUDING HIGH INCOME)'
dat.raw$destination_region[grep('WORLD', dat.raw$destination_name)] = 'WORLD'
dat.raw$destination_region[grep('OUTSIDE EU', dat.raw$destination_name)] = 'WORLD'
dat.raw$destination_region[grep("MIDDLE EAST & NORTH AFRICA", dat.raw$destination_name)] = "MIDDLE EAST & NORTH AFRICA"
dat.raw$destination_region[grep('SOUTH ASIA', dat.raw$destination_name)] = 'SOUTH ASIA'
dat.raw$destination_region[grep('AFRICA', dat.raw$destination_name)] = 'AFRICA' # not able to match to GDP because not a region/ country in world bank 
dat.raw$destination_region[grep('EAST AFRICA', dat.raw$destination_name)] = 'EAST AFRICA' # not able to match to GDP because not a region/ country in world bank 
dat.raw$destination_region[grep('CENTRAL AFRICA', dat.raw$destination_name)] = 'CENTRAL AFRICA' # not able to match to GDP because not a region/ country in world bank 
dat.raw$destination_region[grep("SUB-SAHARAN AFRICA", dat.raw$destination_name)] = "SUB-SAHARAN AFRICA"

#### origin names are sometimes chicken
#### expand this into country, region, then match this to world bank data to get GDP 
dat.raw$origin_country = NA
dat.raw$origin_country_code = NA
dat.raw$origin_region = NA

for (i in 1:nrow(dat.raw)) {
  origin = dat.raw$origin_name[i]
  row = which(countries$country_name == origin)
  if (length(row > 0)) {
    dat.raw$origin_country[i] = countries$country_name[row]
    dat.raw$origin_country_code[i] = countries$country_code[row]
    dat.raw$origin_region[i] = countries$region[row]
  } else {
    # do nothing 
  }
}

# table(dat.raw$origin_name[is.na(dat.raw$origin_country)]) # those unable to match to world back countries 

#### add GDP for each destination and origin country, if no country then region, according to year of travel (if study over multiple years, take the latest year)
#table(dat.raw$travel_yr_value)
GDP <- WDI( # get GDP from world bank
  country = "all",
  indicator = "NY.GDP.PCAP.KD",
  start = 1998, # data starts from 1998
  end = 2018,   # data ends from 2018
  extra = FALSE,
  cache = NULL,
  latest = NULL,
  language = "en"
)
GDP[, unlist(apply(GDP, 2, is.character))] = apply(GDP[, unlist(apply(GDP, 2, is.character))], 2, toupper)

substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))} # function to extract last n characters from string

## classification of income levels July 1, 2020 by world bank
income.class = t(data.frame(Low.income	= c(0, 1036),
                            Lower.middle.income	= c(1036, 4045),
                            Upper.middle.income = c(4046, 12535),
                            High.income	= c(12535, 9999999999)))

gdp.match = as.data.frame(t(apply(dat.raw, 1, function(x){ # for each row 
  
  ## origin GDP 
  if (x[['origin_country_code']] %in% GDP[['iso2c']]){ # first search for the country
    origin_gdp = as.numeric(GDP[['NY.GDP.PCAP.KD']][which(GDP[['iso2c']] == x[['origin_country_code']] & # find the right origin country 
                                                            GDP[['year']] == as.numeric(substrRight(as.character(x[['travel_yr_value']]), 4)))]) # find the right year
  } else if (x[['origin_region']] %in% GDP[['country']]){ # second search for the region 
    origin_gdp = as.numeric(GDP[['NY.GDP.PCAP.KD']][which(GDP[['country']] == x[['origin_region']] & # find the right origin country 
                                                            GDP[['year']] == as.numeric(substrRight(as.character(x[['travel_yr_value']]), 4)))]) # find the right year
  } else {
    origin_gdp = NA
  }
  
  if (!is.na(origin_gdp)) {
    origin_incomelvl = rownames(income.class)[which(origin_gdp < income.class[,2] & origin_gdp > income.class[,1])]
  } else {
    origin_incomelvl = NA
  }
  
  ## destination GDP 
  if (x[['destination_country_code']] %in% GDP[['iso2c']]){ # first search for the country
    destination_gdp = as.numeric(GDP[['NY.GDP.PCAP.KD']][which(GDP[['iso2c']] == x[['destination_country_code']] & # find the right origin country 
                                                                 GDP[['year']] == as.numeric(substrRight(as.character(x[['travel_yr_value']]), 4)))]) # find the right year
    
    if (is.na(destination_gdp)){
      destination_gdp = as.numeric(GDP[['NY.GDP.PCAP.KD']][which(GDP[['iso2c']] == x[['destination_country_code']] & # find the right origin country 
                                                                   GDP[['year']] == as.numeric(substrRight(as.character(x[['travel_yr_value']]), 4)) - 1)]) # find the right year
      
    }
    
  } else if (x[['destination_region']] %in% GDP[['country']]){ # second search for the region 
    destination_gdp = as.numeric(GDP[['NY.GDP.PCAP.KD']][which(GDP[['country']] == x[['destination_region']] & # find the right origin country 
                                                                 GDP[['year']] == as.numeric(substrRight(as.character(x[['travel_yr_value']]), 4)))]) # find the right year
  } else {
    destination_gdp = NA
  }
  
  if (!is.na(destination_gdp)) {
    destination_incomelvl = rownames(income.class)[which(destination_gdp < income.class[,2] & destination_gdp > income.class[,1])]
  } else {
    destination_incomelvl = NA
  }
  
  return(c(origin_gdp = origin_gdp, origin_incomelvl = origin_incomelvl, destination_gdp = destination_gdp, destination_incomelvl = destination_incomelvl))
})))
dat.raw = cbind.data.frame(dat.raw, gdp.match)

#### get columns for analysis 
clean_dat = dat.raw[ ,c('author_yr', 'outcome_type', 'outcome_value', 'travel_yr_value', 'sample_size',
                        'destination_name', 'destination_country','destination_country_code', 'destination_region', 'destination_incomelvl', 'destination_gdp',
                        'origin_name', 'origin_country', 'origin_country_code', 'origin_region', 'origin_incomelvl', 'origin_gdp',
                        'resistance_type', 'resistance_type_specify')]

#### ensure correct data types for each column 
clean_dat$outcome_value = as.numeric(clean_dat$outcome_value)
clean_dat$sample_size = as.numeric(clean_dat$sample_size)
clean_dat$origin_gdp = as.numeric(clean_dat$origin_gdp)
clean_dat$destination_gdp = as.numeric(clean_dat$destination_gdp)

#### check missing data
# tocheck = clean_dat[,c('study_id', 'outcome_type', 'outcome_value', 'travel_yr_value', 'sample_size',
#                        'destination_name',  'destination_country','destination_country_code', 'destination_region', 'destination_incomelvl', 'destination_gdp',
#                        'origin_name', 'origin_country', 'origin_country_code', 'origin_region', 'origin_incomelvl', 'origin_gdp')] # ignore last 2 columns - expected to have missing data
# rows_human_prevalence_destination = which(tocheck$outcome_type == 'human_prevalence_destination') # these rows should not have origin data
# rows_animal_prevalence_destination = which(tocheck$outcome_type == 'animal_prevalence_destination') 
# rows_prevalence_destination = c(rows_human_prevalence_destination, rows_animal_prevalence_destination)
# prevalence_destination = tocheck[rows_prevalence_destination,]
# prevalence_destination_origin = prevalence_destination[, grep('origin', colnames(prevalence_destination))]
# all(is.na(prevalence_destination_origin)) # all should be missing
# prevalence_destination_NOTorigin = prevalence_destination[, -grep('origin', colnames(prevalence_destination))]
# prevalence_destination_missing = prevalence_destination_NOTorigin[!complete.cases(prevalence_destination_NOTorigin),]
# View(prevalence_destination_missing)
# 
# rows_human_acquisition = which(tocheck$outcome_type == 'human_acquisition_origin')
# acquisition = tocheck[rows_human_acquisition,]
# View(acquisition)

############################
# save cleaned data 
write.csv(clean_dat, file = 'data/clean_dat.csv')
