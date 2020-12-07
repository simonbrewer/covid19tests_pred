#load libraries
#------------------------------------------------------------------------------------------------------------------------
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
library(httr)
library(forecastML)

#define readxl_online function
#------------------------------------------------------------------------------------------------------------------------
readxl_online <- function(url, type = NULL, ...) {
  test <- stringr::str_detect(url, "[.]xls|[.]zip")
  if (test == FALSE) {
    print(message("Expecting file extension of type .xlsx, .xls or .zip. Check the URL or the data source for the correct file extension and use the type argument"))
  }
  # test for individual file extensions for xls use look forward, xls not
  # followed by x
  t1 <- stringr::str_detect(url, "[.]xlsx")
  t2 <- stringr::str_detect(url, "[.]xls(?!x)")
  tz <- stringr::str_detect(url, "[.]zip")
  if (t1 == TRUE) {
    type = ".xlsx"
  }
  if (t2 == TRUE) {
    type = ".xls"
  }
  if (tz == TRUE) {
    httr::GET(url, write_disk("tmp.zip", overwrite = TRUE))
    tmp <- unzip("tmp.zip")
    # On osx more than one file name is returned, select first element.
    df <- readxl::read_excel(tmp[[1]])
    return(df)
  }
  if (!is.null(type)) {
    type = type
    
  }
  df <- httr::GET(url, write_disk(paste0("tmp", type), overwrite = TRUE))
  df <- readxl::read_excel(paste0("tmp", type))
  
}

#study period
#------------------------------------------------------------------------------------------------------------------------
startDate <- "2020-04-13"
endDate <- "2020-09-08"

#------------------------------------------------------------------------------------------------------------------------

#complete list of counties
#------------------------------------------------------------------------------------------------------------------------
data("fips_codes")
fips_codes<-fips_codes[(fips_codes$state=="AR" | 
                           fips_codes$state=="CA" |
                           fips_codes$state=="ND" |
                           fips_codes$state=="WI" |
                           fips_codes$state=="NY" |
                           fips_codes$state=="CT" |
                           fips_codes$state=="MI" |
                           fips_codes$state=="WA"),]
fips_codes$FIPS <- paste0(fips_codes$state_code, fips_codes$county_code) 
print(paste0("fips_codes: ", length(unique(fips_codes$FIPS))))


#census stuff
#------------------------------------------------------------------------------------------------------------------------

#get your own key!
census_api_key("[your key]")

#get variable lookup table
#v18 <- load_variables(2018, "acs5", cache = TRUE)
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


Bachelor <- subset(get_acs(geography = "county",
                           variables = c(var = "B16010_041"),
                           year = 2018),
                   select = -c(NAME,variable,moe))

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
colnames(cTable) <- c("FIPS", as.numeric(seq(as.Date("2020/1/22"), as.Date(Sys.Date()-1), by = "day"))) #column names
countyCase <- melt(cTable, id.var = "FIPS")    #convert table to long format
colnames(countyCase) <- c("FIPS","date", "cases") #column names
countyCase$date <- as.numeric(countyCase$date)  #convert date to numeric

#from aggregated counts to actual counts
countyCase  <- countyCase [order(countyCase$FIPS, countyCase$date), ]
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
colnames(firstCaseTable) <- c("FIPS", as.numeric(seq(as.Date("2020/1/22"), as.Date(Sys.Date()-1), by = "day"))) #column names
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

colnames(dTable) <- c("FIPS", as.numeric(seq(as.Date("2020/1/22"), as.Date(Sys.Date()-1), by = "day"))) #column names
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
colnames(firstDeathTable) <- c("FIPS", as.numeric(seq(as.Date("2020/1/22"), as.Date(Sys.Date()-1), by = "day"))) #column names
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


#Gather county-level testing counts - dynamic
#------------------------------------------------------------------------------------------------------------------------

#NY
url <- "https://health.data.ny.gov/api/views/xdss-u53e/rows.csv?accessType=DOWNLOAD&api_foundry=true"
NY <- read.csv(url(url))
colnames(NY) <- c("date", "county", "pos", "cumPos", "cTest", "cumTest")  #rename columns
NY$county <- as.character(NY$county)
NY$date <- as.Date(NY$date, format = "%m/%d/%Y")  #Format date
NY$cTest <- gsub(",","",NY$cTest)     #formate tests column
NY$cTest <- as.numeric(as.character(NY$cTest))  #convert to numeric
NY <- NY[NY$date >= startDate & NY$date <= endDate,]  #date range = study period
NY <- subset(NY, select = c(date, county, cTest)) #drop columns
NY$FIPS <- as.integer(usmap::fips("NY", county = c(as.character(NY$county))))  #convert county to FIPS

view(NY %>% count(county))  #diagnostic: how many records per county

#CT
url <- "https://data.ct.gov/api/views/qfkt-uahj/rows.csv?accessType=DOWNLOAD&api_foundry=true"
CT <- read.csv(url(url))
colnames(CT) <- c("date", "county", "cTest")  #rename columns
CT$county <- as.character(CT$county)
CT<-CT[!(CT$county=="Pending address validation"),] #remove records "pending address validation"
CT$date <- as.Date(CT$date, format = "%m/%d/%Y")  #Format date
CT$cTest <- gsub(",","",CT$cTest)     #formate tests column
CT <- CT[CT$date >= startDate & CT$date <= endDate,]  #date range = study period
CT <- subset(CT, select = c(date, county, cTest)) #drop columns
CT<-CT[!(CT$county=="Pending address validation" | is.na(CT$county)),] #remove records "correctional", Unknown"
CT$FIPS <- str_pad(as.integer(usmap::fips("CT", county = c(as.character(CT$county)))), 5, pad="0")  #convert county to FIPS

view(CT %>% count(county))  #diagnostic: how many records per county

#MI

#update URL
#---------------------------------------------------------------------------------------------------------------------
url <- "https://www.michigan.gov/documents/coronavirus/Diagnostic_Tests_by_Result_and_County_2020-11-14_707800_7.xlsx"
#---------------------------------------------------------------------------------------------------------------------

MI <- readxl_online(url)
colnames(MI) <- c("county", "date", "neg", "pos", "cTest")  #rename columns
MI$county <- as.character(MI$county)
MI <- subset(MI, select = c(date, county, cTest)) #drop columns
MI<-MI[!(MI$county=="Correctional" | MI$county=="Unknown"),] #remove records "correctional", Unknown"
MI$date <- as.Date(MI$date, format = "%m/%d/%Y")  #Format date
MI <- MI[MI$date >= startDate & MI$date <= endDate,]  #date range = study period
MI$county[MI$county=="St Clair"]<-"St. Clair"   #fix punctuation
MI$county[MI$county=="St Joseph"]<-"St. Joseph" #fix punctuation
indDetroit = which(MI$county == "Detroit City")
indWayne = which(MI$county == "Wayne")
MI$cTest[indWayne] <- MI$cTest[indWayne] + MI$cTest[indDetroit] #combine Detroit City and Wayne county
MI<-MI[!(MI$county=="Detroit City"),] #remove records "Detroit City"
MI$FIPS <- as.integer(usmap::fips("MI", county = c(as.character(MI$county))))  #convert county to FIPS

view(MI %>% count(county))  #diagnostic: how many records per county

MI <- fill_gaps(MI, date_col = 1, frequency = '1 day', groups = 'FIPS', static_features = c('county'))
MI$cTest[is.na(MI$cTest)] <- 0

#WA
url <- "https://www.doh.wa.gov/Portals/1/Documents/1600/coronavirus/data-tables/PUBLIC_Tests_by_Specimen_Collection.xlsx"
WA <- readxl_online(url)
colnames(WA) <- c("county", "date", "positive", "posIncomplete", "negative")  #rename columns
WA$county <- as.character(WA$county)
WA <- WA[!(WA$county=="Unassigned"),] #remove records "Unassigned"
WA$cTest <- WA$positive + WA$negative #combine columns
WA$date <- as.Date(WA$date)  #Format date
WA <- WA[WA$date >= startDate & WA$date <= endDate,]  #date range = study period
WA <- subset(WA, select = c(date, county, cTest)) #drop columns
WA$FIPS <- as.integer(usmap::fips("WA", county = c(as.character(WA$county))))  #convert county to FIPS

view(WA %>% count(county))  #diagnostic: how many records per county


# # Wisconsin
# ########################################
# #WI
# url <- "https://opendata.arcgis.com/datasets/5374188992374b318d3e2305216ee413_12.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"
# WI <- read.csv(url(url))
# WI <- subset(WI, select = c(ï..GEOID, DATE, NAME, TEST_NEW)) #drop columns
# colnames(WI) <- c("FIPS", "DATE", "county", "cTest")  #rename columns
# WI$DATE <- as.Date(WI$DATE)  #Format date
# WI <- WI[WI$DATE >= startDate & WI$DATE <= endDate,]  #date range = study period


# # North Dakota
# ########################################
# url <- "https://www.health.nd.gov/sites/www/files/documents/Files/MSS/coronavirus/charts-data/PublicUseData.csv"
# ND <- read.csv(url(url))


#Coronascraper 
#old file: startDate - 7/22
CS1 <- read.csv("./rawdata/timeseries-tidy.csv")
CS1 <- subset(CS1, select = c(county, level, state, country, date, type, value)) #drop columns
CS1$DATE <- substr(as.character(CS1$date), 1, 11) #Format date
CS1$DATE <- as.Date(CS1$DATE)  #Format date
CS1 <- CS1[(CS1$level=="county"),] #only keep counties
CS1 <- CS1[(CS1$country=="United States"),] #only keep USA
CS1 <- CS1[(CS1$type=="tested"),] #only keep tests
#CS1 <- CS1[!(CS1$county=="Dukes County, Nantucket County"),] #remove records "pending address validation"
CS1$county <- as.character(CS1$county)
CS1 <- subset(CS1, select = -c(level,country,date,type)) #drop columns
states <- c("Arkansas", "California", "North Dakota", "Wisconsin")
dfListCS1 <- c()
for (i in states) {
  state <- CS1[(CS1$state==i),] #state
  state$FIPS <- str_pad(as.integer(usmap::fips(unique(state$state), county = c(as.character(state$county)))), 5, pad="0")  #convert county to FIPS
  dfListCS1 <- rbind(dfListCS1,state) # add it to your list
}
tests1 <- as.data.frame(dfListCS1)

#from aggregated counts to actual counts
tests1  <- tests1[order(tests1$FIPS, tests1$DATE), ]
diffCS1 <- function(x) diff(c(0, x))
tests1$actual <- c(unlist(t(aggregate(value~FIPS, tests1, diffCS1)[, -1])))
tests1 <- tests1[tests1$DATE >= startDate & tests1$DATE <= "2020-07-22",]  #date range = study period
tests1 <- subset(tests1, select = c(DATE, county, actual, FIPS)) #drop columns
colnames(tests1) <- c("date", "county", "cTest", "FIPS")  #rename columns
#tests1$cTest[tests1$cTest<0] <- 0
tests1 <- fill_gaps(tests1, date_col = 1, frequency = '1 day', groups = 'FIPS', static_features = c('county'))
tests1$cTest[is.na(tests1$cTest)] <- 0

view(tests1 %>% count(FIPS))  #diagnostic: how many records per county

#new file: 7/23 - endDate
#---------------------------------------------------------------
#download data: https://coronadatascraper.com/timeseries.csv.zip
#---------------------------------------------------------------

CS2 <- read.csv("./rawdata/timeseries.csv")
CS2 <- subset(CS2, select = c(county, level, state, country, date, tested)) #drop columns
CS2 <- CS2[(CS2$level=="county"),] #only keep counties
CS2 <- CS2[(CS2$country=="United States"),] #only keep USA
CS2$county <- as.character(CS2$county)
CS2 <- CS2[complete.cases(CS2), ] #eliminate NAs
CS2$date <- as.Date(CS2$date)  #Format date
states <- c("Arkansas", "California", "North Dakota", "Wisconsin")
dfListCS2 <- c()
for (i in states) {
  state <- CS2[(CS2$state==i),] #state
  state$FIPS <- str_pad(as.integer(usmap::fips(unique(state$state), county = c(as.character(state$county)))), 5, pad="0")  #convert county to FIPS
  dfListCS2 <- rbind(dfListCS2,state) # add it to your list
  print(i)
}
tests2 <- as.data.frame(dfListCS2)
tests2$date <- as.Date(tests2$date)  #Format date

#from aggregated counts to actual counts
tests2  <- tests2[order(tests2$FIPS, tests2$date), ]
diffCS2 <- function(x) diff(c(0, x))
tests2$actual <- c(unlist(t(aggregate(tested~FIPS, tests2, diffCS2)[, -1])))
tests2 <- tests2[tests2$date >= "2020-07-23" & tests2$date <= endDate,]  #date range = study period
tests2 <- subset(tests2, select = c(date, county, actual, FIPS)) #drop columns
colnames(tests2) <- c("date", "county", "cTest", "FIPS")  #rename columns

view(tests2 %>% count(FIPS))  #diagnostic: how many records per county

#combine
tests <- rbind(tests1, tests2)

tests$cTest[tests$cTest<0] <- 0

view(tests %>% count(FIPS))  #diagnostic: how many records per county


print("setdiff(tests1 - tests2)")
print(setdiff(unique(tests1$FIPS), unique(tests2$FIPS)))
print(setdiff(unique(tests2$FIPS), unique(tests1$FIPS)))



#combine all tables
df = do.call(rbind, list(NY, CT, MI, WA, tests))
df$cTest <- as.numeric(df$cTest)
#df$cTest[df$cTest<0] <- 0


#------------------------------------------------------------------------------------------------------------------------


####################
##state-level data##
####################

#get state-level population data - state-level static (sldHo)
#------------------------------------------------------------------------------------------------------------------------
#population
slsPop <- subset(get_acs(geography = "state", 
                           variables = c(var = "B01003_001"), 
                           year = 2018),
                   select = -c(variable, moe))
#add FIPS
slsPop$FIPS <- substr(slsPop$GEOID,0,2)

#add dynamic state-level testing and hospitalization data from the COVID Tracking Project
#https://covidtracking.com/data/download

url <- "https://covidtracking.com/api/v1/states/daily.csv"
sldHo <- read.csv(url(url))

sldHo$fips <- str_pad(sldHo$fips, 2, pad="0") #add preceeding zeroes to fips
sldHo <- transform(sldHo, date = as.Date(as.character(date), "%Y%m%d")) #transform date
sldHo <- left_join(sldHo, slsPop, by = c("fips" = "GEOID")) #join slsPop with sdt
sldHo$hospRate <- (sldHo$hospitalizedIncrease/sldHo$estimate) *100000 #compute hospitalization rate

#drop columns
sldHo <- subset(sldHo, select = c(fips, state, date, hospRate))
names(sldHo)[names(sldHo) == 'fips'] <- 'FIPS'

sldHo <- sldHo[sldHo$date >= startDate & sldHo$date <= endDate,]  #date range = study period
sldHo$hospRate[is.na(sldHo$hospRate)] <- 0



#get state-level testing data - dynamic (sldtd)
#------------------------------------------------------------------------------------------------------------------------
#create date sequence
startDate_1 <- as.Date(startDate) - 1
endDate_1 <- as.Date(endDate) + 1

sequence <- seq(startDate_1, endDate_1, by = "day")
year <- format(as.Date(sequence, format="%d/%m/%Y"),"%Y")
month <- format(as.Date(sequence, format="%d/%m/%Y"),"%m")
day <- format(as.Date(sequence, format="%d/%m/%Y"),"%d")

dates <- as.data.frame(cbind(cbind(day,month),year))

dates$day <- as.character(dates$day)
dates$month <- as.character(dates$month)
dates$year <- as.character(dates$year)

dflist = list()
for (i in 1:length(month)) {
  urlBase = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/'
  url <- paste0(urlBase,paste(dates[i,2],dates[i,1], dates[i,3], sep='-'), '.csv')
  dat <- read.csv(url(url))
  dat$date <- as.Date(paste(dates[i,3],dates[i,2], dates[i,1], sep='-'))  #
  dflist[[i]] <- dat # add it to your list
}
sldtd = as.data.frame(do.call(rbind, dflist))

#drop columns
sldtd <- subset(sldtd, select = c(Province_State, date, FIPS, People_Tested))

#drop rows 
sldtd<-sldtd[!(sldtd$Province_State=="Diamond Princess" | sldtd$Province_State=="Grand Princess" | sldtd$Province_State=="Puerto Rico" |           #drop rows
                 sldtd$Province_State=="Northern Mariana Islands" | sldtd$Province_State=="Virgin Islands" | sldtd$Province_State=="American Samoa" |
                 sldtd$Province_State=="Recovered" | sldtd$Province_State=="Alaska" | sldtd$Province_State=="Hawaii"| sldtd$Province_State=="Guam" | 
                         is.na(sldtd$FIPS) | sldtd$FIPS>80000),]

#from aggregated counts to actual counts
sldtd  <- sldtd[order(sldtd$FIPS, sldtd$date), ]
diff2 <- function(x) diff(c(0, x))
sldtd$actual <- c(unlist(t(aggregate(People_Tested~FIPS, sldtd, diff2)[, -1])))
sldtd[order(as.numeric(rownames(sldtd))),]
sldtd$FIPS <- str_pad(sldtd$FIPS, 2, pad="0") #add preceeding zeroes

#drop columns
sldtd <- subset(sldtd, select = -c(People_Tested))
names(sldtd)[names(sldtd) == 'actual'] <- 'sTest'  #rename columns

sldtd$sTest[sldtd$sTest < 0] <- 0
sldtd <- sldtd[sldtd$date >= startDate & sldtd$date <= endDate,]  #date range = study period

#------------------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------------
#combine all tables
ct <-
  left_join(
    left_join(
      left_join(
        left_join(
          left_join(
            left_join(
              fips_codes,
            clsPop, by=c("FIPS" = "FIPS")), 
          cldCo, by = c("FIPS" = "FIPS")), 
        clsHnu, by = c("FIPS" = "FIPS")),
      sldHo, by = c("sFIPS" = "FIPS", "date")),
    sldtd, by = c("sFIPS" = "FIPS", "date")),
  df, by = c("FIPS", "date"))


#deal with missing values
ct$universities[is.na(ct$universities)] <- 0
ct$hospitals[is.na(ct$hospitals)] <- 0
ct$nursing[is.na(ct$nursing)] <- 0

#diagnostics
length(unique(ct$FIPS))
any(is.na(ct))
which(is.na(ct)) 

unique(ct$state.x)

#delete unnecessary columns
ct <- subset(ct, select = -c(county_code, state_code, state_name, county.x, sFIPS, state.y))

colnames(ct)[1] <- "state"
colnames(ct)[24] <- "county"

#omit NA values
nrow(ct)
ct <- na.omit(ct) 
nrow(ct)

#------------------------------------------------------------------------------------------------------------------------
#write to file
write.csv(ct,"./outputs/countyTable_timeSeries.csv", row.names = FALSE)
