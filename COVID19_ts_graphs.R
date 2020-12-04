library(sf)
library(RColorBrewer)
library(stringr)
library(dplyr)
library(ggplot2)
library(classInt)
library(tidycensus)
library(ggpubr)

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


# #Time series plot using ggpubr
# #------------------------------------------------------------------------------------------------------- 
# 
# ggline(sums, x = "Category", y = "x", xlab = c(sums$Category[1], sums$Category[length(sums$Category)]))

