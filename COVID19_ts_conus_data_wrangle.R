#load libraries
require(caTools)
library(tidycensus)
library(tidyverse)
library(dplyr)
library(stringr)
library(usmap)
library(readxl)
library(sf)
library(reshape2)
library(cdlTools)
library(tigris)

#study period
#------------------------------------------------------------------------------------------------------------------------
startDate <- "2020-04-13"
endDate <- "2020-09-08"

#------------------------------------------------------------------------------------------------------------------------


#complete list of counties
#------------------------------------------------------------------------------------------------------------------------

data("fips_codes")

fips_codes<-fips_codes[!(fips_codes$state=="AK" | 
                           fips_codes$state=="HI" |
                           fips_codes$state=="AS" |
                           fips_codes$state=="GU" |
                           fips_codes$state=="MP" |
                           fips_codes$state=="PR" |
                           fips_codes$state=="UM" |
                           fips_codes$state=="VI"),]

fips_codes$FIPS <- paste0(fips_codes$state_code, fips_codes$county_code) 

print(paste0("fips_codes: ", length(unique(fips_codes$FIPS))))

#census stuff
#------------------------------------------------------------------------------------------------------------------------

#get your own key!
census_api_key("[your key]")

#get variable lookup table
v18 <- load_variables(2018, "acs5", cache = TRUE)
#------------------------------------------------------------------------------------------------------------------------



#####################
##county-level data##
#####################

#population data - county-level static (clsPop)
#------------------------------------------------------------------------------------------------------------------------
#get census data
Tot_pop <- subset(get_acs(geography = "county", 
                          variables = c(var = "B01003_001"), 
                          year = 2018),
                  select = -c(NAME,variable, moe))
Pop_o_60 <- subset(get_acs(geography = "county", 
                           variables = c(var = "S0101_C01_028E"), 
                           year = 2018),
                   select = -c(NAME,variable, moe))
Pop_m <- subset(get_acs(geography = "county", 
                        variables = c(var = "B01001_002"), 
                        year = 2018),
                select = -c(NAME,variable, moe))
Pop_white <- subset(get_acs(geography = "county", 
                            variables = c(var = "B01001A_001"), 
                            year = 2018),
                    select = -c(NAME,variable, moe))
Pop_black <- subset(get_acs(geography = "county", 
                            variables = c(var = "B01001B_001"), 
                            year = 2018),
                    select = -c(NAME,variable, moe))
Pop_AmIndAlNat <- subset(get_acs(geography = "county", 
                                 variables = c(var = "B01001C_001"), 
                                 year = 2018),
                         select = -c(NAME,variable, moe))
Pop_asia <- subset(get_acs(geography = "county", 
                           variables = c(var = "B01001D_001"), 
                           year = 2018),
                   select = -c(NAME,variable, moe))
Pop_NaHaPaIs <- subset(get_acs(geography = "county", 
                               variables = c(var = "B01001E_001"), 
                               year = 2018),
                       select = -c(NAME,variable, moe))
Income <- subset(get_acs(geography = "county", 
                         variables = c(var = "S1901_C01_012E"), 
                         year = 2018),
                        select = -c(NAME,variable,moe))

#impute NM mean for missing value
Income[is.na(Income$estimate),]$estimate <- mean((Income[(substr(Income$GEOID,1,2)=="35"),]$estimate), na.rm = TRUE)


Bachelor <- subset(get_acs(geography = "county",
                           variables = c(var = "B16010_041"),
                           year = 2018),
                        select = -c(NAME,variable,moe))

#impute NM mean for missing value
Bachelor[is.na(Bachelor$estimate),]$estimate <- mean((Bachelor[(substr(Bachelor$GEOID,1,2)=="35"),]$estimate), na.rm = TRUE)

# #bind together
                 
clsPop <- left_join(
  left_join(
    left_join(
      left_join(
        left_join(
          left_join(
            left_join(
              left_join(
                left_join(
                  Tot_pop,Pop_o_60, by = c("GEOID" = "GEOID")
                ), Pop_m, by = c("GEOID" = "GEOID")
              ), Pop_white, by = c("GEOID" = "GEOID")
            ), Pop_black, by = c("GEOID" = "GEOID")
          ), Pop_AmIndAlNat, by = c("GEOID" = "GEOID")
        ), Pop_asia, by = c("GEOID" = "GEOID")
      ), Pop_NaHaPaIs, by = c("GEOID" = "GEOID")
    ), Income, by = c("GEOID" = "GEOID")
  ), Bachelor, by = c("GEOID" = "GEOID")
)                    

clsPop$sFIPS <- substr(clsPop$GEOID,0,2)
colnames(clsPop) <- c("FIPS", "Tot_pop", "Pop_o_60", "Pop_m", "Pop_white", "Pop_black", "Pop_AmIndAlNat", "Pop_asia", "Pop_NaHaPaIs", "Income", "Bachelor", "sFIPS")  

any(is.na(clsPop))


#hospitals, nursing homes, universities and colleges - county-level static (clsHnu)
#------------------------------------------------------------------------------------------------------------------------
#hospitals
hosp <- read.csv("./rawdata/Hospitals.csv")
hospFreq <- as.data.frame(table(hosp$COUNTYFIPS))
hospFreq$Var1 <- as.character(hospFreq$Var1)

#nursing homes
nurs <- read.csv("./rawdata/Nursing_Homes.csv")
nursFreq <- as.data.frame(table(nurs$COUNTYFIPS))
nursFreq$Var1 <- as.character(nursFreq$Var1)

#universities&colleges
univ <- read.csv("./rawdata/Colleges_and_Universities.csv")
univFreq <- as.data.frame(table(univ$COUNTYFIPS))
univFreq$Var1 <- as.character(univFreq$Var1)


#combine
clsHnu <- full_join(hospFreq, full_join(nursFreq, univFreq, by = c("Var1" = "Var1")), by = c("Var1" = "Var1"))
colnames(clsHnu) <- c("FIPS", "hospitals", "nursing", "universities")

#add FIPS
clsHnu$FIPS <- str_pad(clsHnu$FIPS, 5, pad="0")

#------------------------------------------------------------------------------------------------------------------------

# COVID-19 data - county-level dynamic (cldCo)
#------------------------------------------------------------------------------------------------------------------------
#cases
url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
cTable <- read.csv(url(url))

cTable<-cTable[!(cTable$Province_State=="Diamond Princess" | cTable$Province_State=="Grand Princess" | cTable$Province_State=="Puerto Rico" |           #drop rows
                   cTable$Province_State=="Northern Mariana Islands" | cTable$Province_State=="Virgin Islands" | cTable$Province_State=="American Samoa" |
                   cTable$Province_State=="Recovered" | cTable$Province_State=="Alaska" | cTable$Province_State=="Hawaii"| cTable$Province_State=="Guam" | 
                   is.na(cTable$FIPS) | cTable$FIPS>80000),]
cTable <- subset(cTable, select = -c(UID, iso2, iso3, code3, Admin2, Province_State, Country_Region, Lat, Long_, Combined_Key)) #drop columns
colnames(cTable) <- c("FIPS", as.numeric(seq(as.Date("2020/1/22"), as.Date(Sys.Date()-2), by = "day"))) #column names
countyCase <- melt(cTable, id.var = "FIPS")    #convert table to long format
colnames(countyCase) <- c("FIPS","date", "cases") #column names
countyCase$date <- as.numeric(countyCase$date)  #convert date to numeric

#from aggregated counts to actual counts
countyCase  <- countyCase [order(countyCase $FIPS, countyCase $date), ]
diff2 <- function(x) diff(c(0, x))
countyCase $actual <- c(unlist(t(aggregate(cases~FIPS, countyCase , diff2)[, -1])))
countyCase [order(as.numeric(rownames(countyCase ))),]
countyCase$actual <- ifelse(countyCase$actual < 0, 0, countyCase$actual) #eliminate negatives (6 in total so far due to bad case reporting)
countyCase$date <- as.Date(countyCase$date, origin = "2020-01-21")  #convert date to date type
colnames(countyCase) <- c("FIPS","date", "caseCum", "caseNew") #column names

#days since first case (temporal distance to day of COVID-19 onset)
zeroes <- rowSums(cTable == 0) #number of days since onset for each county
firstCaseTable <- cTable  #initialize table
firstCaseTable$zeroes <- zeroes   
for (i in 2:(length(firstCaseTable)-1)) {
  firstCaseTable[,i] <- i-2 - firstCaseTable$zeroes
}
firstCaseTable <- firstCaseTable[1:(length(firstCaseTable)-1)]
colnames(firstCaseTable) <- c("FIPS", as.numeric(seq(as.Date("2020/1/22"), as.Date(Sys.Date()-2), by = "day"))) #column names
firstCaseTable <- melt(firstCaseTable, id.var = "FIPS")    #convert table to long format
colnames(firstCaseTable) <- c("FIPS","date", "daysSinceC") #column names
firstCaseTable$date <- as.numeric(firstCaseTable$date)  #convert date to numeric
firstCaseTable$date <- as.Date(firstCaseTable$date, origin = "2020-01-21")  #convert date to date type
countyCase <- inner_join(countyCase, firstCaseTable) #combine
countyCase <- countyCase[countyCase$date >= startDate & countyCase$date <= endDate,]  #date range = study period

#deaths
url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
dTable <- read.csv(url(url))
dTable<-dTable[!(dTable$Province_State=="Diamond Princess" | dTable$Province_State=="Grand Princess" | dTable$Province_State=="Puerto Rico" |           #drop rows
                   dTable$Province_State=="Northern Mariana Islands" | dTable$Province_State=="Virgin Islands" | dTable$Province_State=="American Samoa" |
                   dTable$Province_State=="Recovered" | dTable$Province_State=="Alaska" | dTable$Province_State=="Hawaii"| dTable$Province_State=="Guam" |
                   is.na(dTable$FIPS) | dTable$FIPS>80000),]
dTable <- subset(dTable, select = -c(UID, Population, iso2, iso3, code3, Admin2, Province_State, Country_Region, Lat, Long_, Combined_Key)) #drop columns

colnames(dTable) <- c("FIPS", as.numeric(seq(as.Date("2020/1/22"), as.Date(Sys.Date()-2), by = "day"))) #column names
countyDeath <- melt(dTable, id.var = "FIPS")    #convert table to long format
colnames(countyDeath) <- c("FIPS","date", "deaths") #column names
countyDeath$date <- as.numeric(countyDeath$date)  #convert date to numeric

#from aggregated counts to actual counts
countyDeath  <- countyDeath [order(countyDeath$FIPS, countyDeath$date), ]
diff2 <- function(x) diff(c(0, x))
countyDeath $actual <- c(unlist(t(aggregate(deaths~FIPS, countyDeath , diff2)[, -1])))
countyDeath [order(as.numeric(rownames(countyDeath ))),]

countyDeath$actual <- ifelse(countyDeath$actual < 0, 0, countyDeath$actual) #eliminate negatives (6 in total so far due to bad case reporting)
countyDeath$date <- as.Date(countyDeath$date, origin = "2020-01-21")  #convert date to date type
colnames(countyDeath) <- c("FIPS","date", "deathCum", "deathNew") #column names

#days since first death (temporal distance to day of COVID-19 onset)
zeroes <- rowSums(dTable == 0) #number of days since onset for each county
firstDeathTable <- dTable  #initialize table
firstDeathTable$zeroes <- zeroes   
for (i in 2:(length(firstDeathTable)-1)) {
  firstDeathTable[,i] <- i-2 - firstDeathTable$zeroes
}
firstDeathTable <- firstDeathTable[1:(length(firstDeathTable)-1)]
colnames(firstDeathTable) <- c("FIPS", as.numeric(seq(as.Date("2020/1/22"), as.Date(Sys.Date()-2), by = "day"))) #column names
firstDeathTable <- melt(firstDeathTable, id.var = "FIPS")    #convert table to long format
colnames(firstDeathTable) <- c("FIPS","date", "daysSinceD") #column names
firstDeathTable$date <- as.numeric(firstDeathTable$date)  #convert date to numeric
firstDeathTable$date <- as.Date(firstDeathTable$date, origin = "2020-01-21")  #convert date to date type

countyDeath <- inner_join(countyDeath, firstDeathTable) #combine
countyDeath <- countyDeath[countyDeath$date >= startDate & countyDeath$date <= endDate,]  #date range = study period

#combine case and death data
cldCo <- full_join(countyCase, countyDeath, by = c("FIPS", "date"))
cldCo <- cldCo %>%  mutate(FIPS = as.character(FIPS))
cldCo$FIPS <- str_pad(cldCo$FIPS, 5, pad="0")
cldCo <- subset(cldCo, select = -c(caseCum, deathCum)) #drop columns


# deal with NA values
cldCo$deathNew[is.na(cldCo$deathNew)] <- 0        #new deaths
cldCo$daysSinceD[is.na(cldCo$daysSinceD)] <- min(firstDeathTable$daysSinceD)    #days since first death

#------------------------------------------------------------------------------------------------------------------------


####################
##state-level data##
####################

#get state-level population data - state-level static (slsPop)
#------------------------------------------------------------------------------------------------------------------------
#population
slsPop <- subset(get_acs(geography = "state", 
                           variables = c(var = "B01003_001"), 
                           year = 2018),
                   select = -c(variable, moe))
#add FIPS
slsPop$FIPS <- substr(slsPop$GEOID,0,2)

#------------------------------------------------------------------------------------------------------------------------
#get dynamic state-level hospitalization data from the COVID Tracking Project - state-level dynamic (sldHo)
#https://covidtracking.com/data/download
#------------------------------------------------------------------------------------------------------------------------

url <- "https://covidtracking.com/api/v1/states/daily.csv"
sldHo <- read.csv(url(url))

sldHo$fips <- str_pad(sldHo$fips, 2, pad="0") #add preceeding zeroes to fips
sldHo <- transform(sldHo, date = as.Date(as.character(date), "%Y%m%d")) #transform date
sldHo <- left_join(sldHo, slsPop, by = c("fips" = "GEOID")) #join statePop with sdt
sldHo$hospRate <- (sldHo$hospitalizedIncrease/sldHo$estimate) *100000 #compute hospitalization rate

#drop columns
sldHo <- subset(sldHo, select = c(fips, state, date, hospRate))
names(sldHo)[names(sldHo) == 'fips'] <- 'FIPS'

sldHo <- sldHo[sldHo$date >= startDate & sldHo$date <= endDate,]  #date range = study period
sldHo$hospRate[is.na(sldHo$hospRate)] <- 0

#------------------------------------------------------------------------------------------------------------------------
#combine all tables
ct <- left_join(
  left_join(
    left_join(
      left_join(
        fips_codes,
        clsPop, by=c("FIPS" = "FIPS")), 
      cldCo, by = c("FIPS" = "FIPS")), 
    clsHnu, by = c("FIPS" = "FIPS")),
  sldHo, by = c("sFIPS" = "FIPS", "date"))

#deal with missing values
ct$universities[is.na(ct$universities)] <- 0
ct$hospitals[is.na(ct$hospitals)] <- 0
ct$nursing[is.na(ct$nursing)] <- 0

#deal with two insubordinate counties
ct <- ct[!(ct$FIPS == "51515" | ct$FIPS == "46113"),] 

#diagnostics
length(unique(ct$FIPS))
any(is.na(ct))
which(is.na(ct)) 

#delete unnecessary columns
ct <- subset(ct, select = -c(county_code, state_name, sFIPS, state.y))

#write
write.csv(ct,"./outputs/countyTable_timeSeries_conus.csv", row.names = FALSE)





