library(sf)
library(RColorBrewer)
library(stringr)
library(dplyr)
library(ggplot2)
library(classInt)
library(tidycensus)
library(ggpubr)
library(dplyr)
library(RColorBrewer)


#read file
test <- as.data.frame(read.csv("./outputs/COVID19_tests_pred_ranger.csv"))

#add preceeding zeroes
test$FIPS <- as.character(str_pad(test$FIPS, 5, pad="0"))

#read county geometries
geom <- st_read('./rawdata/CONUS_counties.shp')

#drop columns
drops <- c("STATEFP","COUNTYFP","COUNTYNS","AFFGEOID","NAME","LSAD","CENTROID_X","CENTROID_Y","AREA_GEO") #drop columns
geom <- geom[ , !(names(geom) %in% drops)] #drop columns
geom$GEOID <- as.character(geom$GEOID)


# #Produce daily maps 
# #-------------------------------------------------------------------------------------------------------
# # we select 7 colors from the palette
# pal <- brewer.pal(6, "OrRd") 
# 
# #specify class breaks
# breaks_qt <- classIntervals(c(min(test$pred),test$pred), n = 7, style = "quantile")
# 
# #dates
# date <- c("2020-04-14", "2020-05-14", "2020-06-14", "2020-07-14")
# 
# #loop through dates and produce map
# for (i in date){
#         
#         test1<-test[(test$date==i),]
#         mapObj <- left_join(geom,test1, by = c("GEOID" = "FIPS"))
#         mapObj <- mutate(mapObj, test_cat = cut(pred, breaks_qt$brks)) 
#         #jpeg(paste0('../figures/pred/pred_', date, '.jpg'),width=1500, height=630)
#         ggplot(mapObj) + 
#                 geom_sf(aes(fill=test_cat)) +
#                 scale_fill_brewer(palette = "OrRd")
#         ggsave(paste0('../figures/pred/pred_', i, '.jpg')
#                ,width = 35 
#                ,height = 25 
#                ,units = "cm"
#                #dpi = 300,
#                ,device = "jpg"
#                )
# }        
#------------------------------------------------------------------------------------------------------- 



#Produce map of cumulative counts
#------------------------------------------------------------------------------------------------------- 

sums <- aggregate(test$pred, by=list(Category=test$FIPS), FUN=sum)
mapObj <- left_join(geom,sums, by = c("GEOID" = "Category"))
breaks_qt <- classIntervals(c(min(sums$x),sums$x), n = 7, style = "quantile")
mapObj <- mutate(mapObj, tests_per_1000 = cut(x, breaks_qt$brks)) 
ggplot(mapObj) + 
  geom_sf(aes(fill=tests_per_1000)) +
  scale_fill_brewer(palette = "OrRd")
ggsave('./figures/pred_cumulative.jpg'
       ,width = 35 
       ,height = 25 
       ,units = "cm"
       #dpi = 300,
       ,device = "jpg"
)
dev.off()
st_write(mapObj, './outputs/cumulative.shp')
#------------------------------------------------------------------------------------------------------- 


#Time series plot
#------------------------------------------------------------------------------------------------------- 

test$numtest <-test$pred * 1000

sums <- aggregate(test$numtest, by=list(Category=test$date), FUN=sum)
# Most basic bubble plot
p <- ggplot(sums, aes(x=as.Date(as.character(Category)), y=x)) +
  geom_line() + 
  xlab("date") +
  ylab("#tests") +
  theme_bw()
min <- as.Date(sums$Category[1])
max <- NA
p + scale_x_date(limits = c(min, max))
p + scale_x_date(date_labels = "%b-%d")+
  theme(axis.text.x = element_text(angle=45, hjust = 1))

ggsave('./figures/time_series.jpg'
       ,width = 7 
       ,height = 5 
       ,units = "cm"
       ,dpi = 300
       ,device = "jpg"
)


# time series plot: previous version's predictions vs. current version's predictions
#------------------------------------------------------------------------------------------------------- 
startDate <- "2020-04-13"

#get your own key!
census_api_key("767b7c8f877abf0396b842d5a3f5e92aa5ba05c1")

#get pop
Tot_pop <- subset(get_acs(geography = "county", 
                          variables = c(var = "B01003_001"), 
                          year = 2018),
                  select = -c(NAME,variable, moe))

#read previous version's predictions
test_old <- as.data.frame(read.csv("./old/covid19tests_pred-master v1/COVID19_tests_pred_ranger.csv"))

#date range = study period
test <- test[test$date >= startDate & test$date <= max(test_old$date),]  

#pad FIPS code with leading zero
test$FIPS <- str_pad(test$FIPS, 5, pad="0")
test_old$FIPS <- str_pad(test_old$FIPS, 5, pad="0")

#join test, test_old, Tot_pop
dat <-left_join(left_join(test, test_old, by = c("FIPS", "date")), Tot_pop, by = c("FIPS" = "GEOID"))

#multiply predictions with county population
dat$pred.x <- dat$pred.x * dat$estimate
dat$pred.y <- dat$pred.y * dat$estimate

#sum by date      
test.sum <- aggregate(x = dat[c("pred.x", "pred.y")],
                      FUN = sum,
                      by = list(Group.date = dat$date))

#convert to date
test.sum$date <- as.Date(test.sum$Group.date)

# plot
p <- ggplot(test.sum, aes(x=date)) + 
  geom_line(aes(y = pred.x, color = "blue")) + 
  geom_line(aes(y = pred.y, color = "red")) +
  scale_color_discrete(labels = c("previous", "current")) +
  ylab("tests")
  
p

ggsave('./figures/diff_time_series.jpg'
       ,width = 12 
       ,height = 8 
       ,units = "cm"
       ,dpi = 300
       ,device = "jpg"
)

#map: previous version's predictions vs. current version's predictions
#------------------------------------------------------------------------------------------------------- 

#sum by county      
county.sum <- aggregate(x = dat[c("pred.x", "pred.y")],
                      FUN = sum,
                      by = list(Group.date = dat$FIPS))

#compute relative change
county.sum$ratio <- (county.sum$pred.x - county.sum$pred.y)/county.sum$pred.y

#join with countie geometries
mapObj <- left_join(geom,county.sum, by = c("GEOID" = "Group.date"))

#color palette
pal <- brewer.pal(7, "OrRd") # we select 7 colors from the palette
class(pal)

#save map as JPG
png(filename="./figures/change.jpg", width = 900, height = 600)

#plot
g <- plot(mapObj["ratio"], 
     main = "relative change",
     breaks = "quantile", nbreaks = 7,
     pal = pal)
     
dev.off()

