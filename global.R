#Exploring Mortality Data Beta - Global variables and Deployment
#24/02/16
#Written by Alexander Stuteley

#URL and deployment code below
#https://astu221.shinyapps.io/appui/
#rsconnect::setAccountInfo(name='astu221',token='BBD3DBBF58B9983E437EBA4050F6D08A',secret='9HqyBu78UJ+iz/ogSCRZppO87tfr24qtRBZ8Qzkm')
#setwd('C:/Users/astut/Desktop/appui')
#setwd('C:/Users/Alex/Desktop/appui')
#rsconnect::deployApp()

##cat("\014")
##flushes current console

#script containing all global variables

library(shiny)

#dataframe holding current output
#full <- NULL
#separator <- NULL
#plotdata <- NULL

#vector containing title name
title <- "Population Data Visualiser - Beta"

#vector containing countries
countrytext <- c("New Zealand","Cook Islands","Other")

#vector containing population numbers
poptext <- c(1,2,3,4)

#vector containing Cook Islands issues to select from
cookissuetext <- c("Select from below","Total Mortality","Neoplasms")

#vector containing the issues to select from
issuetext <- c("Select from below","Mortality - Total",
               "Mortality - Pearce Amenable","Mortality - Pearce Non-Amenable",
               "Mortality - Raymont Amenable","Mortality - Raymont Non-Amenable",
               "Mortality - Raymont Avoidable","Mortality - Raymont Non-Avoidable",
               "Mortality - Tobias Amenable","Mortality - Tobias Non-Amenable",
               "Mortality - MoH Amenable","Mortality - MoH Non-Amenable",
               "Cancer - Gastric Death","Cancer - Gastric Incidence","Cancer - Priority","B.Y.O.")

#vector of all mortality issues
mort <- c("Mortality - Total",
          "Mortality - Pearce Amenable","Mortality - Pearce Non-Amenable",
          "Mortality - Raymont Amenable","Mortality - Raymont Non-Amenable",
          "Mortality - Raymont Avoidable","Mortality - Raymont Non-Avoidable",
          "Mortality - Tobias Amenable","Mortality - Tobias Non-Amenable",
          "Mortality - MoH Amenable","Mortality - MoH Non-Amenable")

#vector containing priority cancer list
prioritytext <- c("Colorectal","Cervical","Female Breast","Leukaemia","Melanoma","Prostate","Lung",
                  "Hodgkin Lymphoma","Non-Hodgkin Lymphoma","Myeloproliferate & Myelodysplastic")

#the following code will be how each dataset is read in

csvs <<- read.csv("csvs.csv", header = TRUE)

#data_proj <- read.csv("data/denominator/proj/projtotalnat.csv", header = T, sep = ";")
#data_projm <- read.csv("data/denominator/proj/projmaorinat.csv", header = T, sep = ";")
#data_projnm <- data_proj
#data_projnm[ , 3:20] <- data_projnm[ , 3:20] - data_projm[ , 3:20]
#write.csv(data_projnm, file = "projnonmaorinat.csv", row.names = FALSE, quote = FALSE)

data_popcen_cook <- read.csv("data/denominator/popcencook.csv",header=T,sep=";")
data_mort_cook <- read.csv("data/numerator/mortcook.csv",header=T,sep=";")
data_neo_cook <- read.csv("data/numerator/neocook.csv",header=T,sep=";")

deptext <- c("None","Decile 1","Decile 2","Decile 3","Decile 4","Decile 5","Decile 6","Decile 7","Decile 8","Decile 9","Decile 10","Quintile 1","Quintile 2","Quintile 3","Quintile 4","Quintile 5")

data_popestdisc95_total_nat <- read.csv("data/denominator/popestdisc95totalnat.csv",header=T)
data_popestdisc90_maori_nat <- read.csv("data/denominator/popestdisc90maorinat.csv",header=T)
data_popestdisc90_nonmaori_nat <- read.csv("data/denominator/popestdisc90nonmaorinat.csv",header=T,sep=';')
data_popest91_total_nat <- read.csv("data/denominator/popest91totalnat.csv",header=T)
data_popest91_maori_nat <- read.csv("data/denominator/popest91maorinat.csv",header=T)
data_popest91_nonmaori_nat <- read.csv("data/denominator/popest91nonmaorinat.csv",header=T,sep=';')

data_popcen_total_nat <- read.csv("data/denominator/popcentotalnat.csv",header=T,sep=";")
data_popcen_maori_nat <- read.csv("data/denominator/popcenmaorinat.csv",header=T,sep=";")
data_popcen_nonmaori_nat <- read.csv("data/denominator/popcennonmaorinat.csv",header=T,sep=";")
data_popcen_pacific_nat <- read.csv("data/denominator/popcenpacificnat.csv",header=T,sep=";")
data_popcen_asian_nat <- read.csv("data/denominator/popcenasiannat.csv",header=T,sep=";")

data_popest_total_reg <- read.csv("data/denominator/poptotalreg.csv",header=T,sep=";")
data_popest_total_tla <- read.csv("data/denominator/poptotaltla.csv",header=T,sep=";")
data_popest_total_dhb <- read.csv("data/denominator/poptotaldhb.csv",header=T,sep=",")

data_standard <- read.csv("data/standard/refpopext.csv",header=T)

#vector containing the geographic types to select from
geotext <- c("National","Sub-National")
subgeotext <- c("Regional Council","TLA","DHB","Rohe")
regtext <- c("Northland","Auckland","Waikato","Bay of Plenty","Gisborne","Hawke's Bay","Taranaki",
             "Manawatu-Wanganui","Wellington","Tasman","Nelson","Marlborough","West Coast",
             "Canterbury","Otago","Southland","Area Outside")
tlatext <- c("Far North","Whangarei","Kaipara","Rodney","North Shore",
             "Waitakere","Auckland","Manakau","Papakura","Franklin","Thames-Coromandel","Hauraki","Waikato",
             "Matamata-Piako","Hamilton",
             "Waipa","Otorohanga","South Waikato","Waitomo","Taupo","Western BOP","Tauranga",
             "Rotorua","Whakatane","Kawerau","Opotiki","Gisborne","Wairoa","Hastings","Napier",
             "Central Hawke's Bay","New Plymouth","Stratford","South Taranaki","Ruapehu","Wanganui",
             "Rangitikei","Manawatu","Palmerston North","Tararua","Horowhenua","Kapiti Coast",
             "Porirua","Upper Hutt","Lower Hutt","Wellington","Masterton","Carterton","South Wairarapa",
             "Tasman","Nelson","Marlborough","Kaikoura","Buller","Grey","Westland","Hurunui",
             "Waimakariri","Christchurch","Banks Peninsula","Selwyn","Ashburton","Timaru","Mackenzie",
             "Waimate",
             "Chatham Islands","Waitaki","Central Otago","Queenstown Lakes","Dunedin","Clutha",
             "Southland","Gore","Invercargill","Area outside")
dhbtext <- c("Northland","Waitemata","Auckland","Counties Manukau","Waikato","Lakes","Bay of Plenty",
              "Tairawhiti","Taranaki","Hawke's Bay","Midcentral","Whanganui","Capital and Coast",
              "Hutt","Wairarapa","Nelson Marlborough","West Coast","Canterbury","South Canterbury",
              "Southern")#,"Area outside DHB")


#vector containing accuracy increasing methods
extext <- c("None","3-year Rolling Average","3-year Period","Census year +/- 1")

#vector containing the age grouping types
grouptext <- c("Total","5-year bands","Other")

#vector containing selections for age grouping
selectgrouptext <- c("Total","00-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
"45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")

#vector with colnames
by5years <- c("Years","00-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")

#vector names
outnames <- c("Year","Factor","X00.04","X05.09","X10.14","X15.19","X20.24","X25.29","X30.34","X35.39","X40.44",
                          "X45.49","X50.54","X55.59","X60.64","X65.69","X70.74","X75.79","X80.and.over","Total")

#vectors containing type of standard, followed by potential choices within external and internal
stdtypetext <- c("External","Internal")
extstdtext <- c("WHO","Segi","Maori - Census 2001","Maori - Census 2006","Maori - Census 2013")

#these vectors add options based on what ethnicity is chosen for population 1
#censusM <- c("WHO","Segi","Maori-Census 2001","Maori-Census 2006","Maori-Census 2013")
censusP <- c("WHO","Segi","Maori - Census 2001","Maori - Census 2006","Maori - Census 2013",
             "Pacific - Census 2001","Pacific - Census 2006","Pacific - Census 2013")
censusA <- c("WHO","Segi","Maori - Census 2001","Maori - Census 2006","Maori - Census 2013",
             "Asian - Census 2001","Asian - Census 2006","Asian - Census 2013")

#these are internal standard vectors which default to Population 1 and update based on number of populations
intstd1 <- c("Population 1")
intstd2a <- c("Population 1 (default)","Population 2")
intstd2b <- c("Population 1")
intstd3 <- c("Population 1 (default)","Population 2","Population 3")
intstd4 <- c("Population 1 (default)","Population 2","Population 3","Population 4")

#vector containing the ethnicities to select from
eth <- c("Total","Maori","Non-Maori","Pacific","Asian")
caneth <- c("Total","Maori","Non-Maori")

#vector containing the gender options
gen <- c("Total","Male","Female")

#vector containing Census years
cyears <- c(1951,1956,1961,1966,1971,1976,1981,1986,1991,1996,2001,2006,2013)

#the default age range for the age sliders for each population
range_lower <- 1948
range_upper <- 2013

#byo default
byo_lower <- 1948
byo_upper <- 2013

byo_eth <- "Total"

#projected defaults
proj_lower <- 2012
proj_upper <- 2038

#below are sets of year ranges for each ethnicity 
#they will dynamically update sliders once we know exactly what data we have
#currently only by ethnicity and geographic type as they are likely the only two major bottlenecks
#but that depends on how good the data is for the issue
cook_range_lower <- 2007
cook_range_upper <- 2012

total_nat_range_lower <- 1996
total_nat_range_upper <- 2013
total_reg_range_lower <- 2006
total_reg_range_upper <- 2013
total_tla_range_lower <- 2006
total_tla_range_upper <- 2013
total_dhb_range_lower <- 2006
total_dhb_range_upper <- 2013

maori_nat_range_lower <- 1996
maori_nat_range_upper <- 2013
maori_reg_range_lower <- 2006
maori_reg_range_upper <- 2013
maori_tla_range_lower <- 2006
maori_tla_range_upper <- 2013
maori_dhb_range_lower <- 2006
maori_dhb_range_upper <- 2013

pacific_nat_range_lower <- 1996
pacific_nat_range_upper <- 2013
pacific_reg_range_lower <- 1996
pacific_reg_range_upper <- 2013
pacific_tla_range_lower <- 1996
pacific_tla_range_upper <- 2013
pacific_dhb_range_lower <- 1996
pacific_dhb_range_upper <- 2013

asian_nat_range_lower <- 1996
asian_nat_range_upper <- 2013
asian_reg_range_lower <- 1996
asian_reg_range_upper <- 2013
asian_tla_range_lower <- 1996
asian_tla_range_upper <- 2013
asian_dhb_range_lower <- 1996
asian_dhb_range_upper <- 2013

gastric_death_range_lower <- 1948
gastric_death_range_upper <- 2012
gastric_inc_range_lower <- 1963
gastric_inc_range_upper <- 2013

priority_range_lower <- 2010
priority_range_upper <- 2012

