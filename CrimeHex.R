library(dplyr)
library(hexbin)
library(ggplot2)
library(data.table)
library(lubridate)
library(plotly)
library(ggmap)
library(tigris)
library(acs)
library(stringr)
library(leaflet)
library(gdata)


# Get Population US Cenus
counties <- c(075)
blocks <- block_groups(state = 'CA', county = c(075), cb=FALSE)

tracts <- tracts(state = 'CA', county = c(075), cb=FALSE)

api.key.install(key="")

geo<-geo.make(state=c("CA"),
              county=c(075), tract="*", block.group = "*")

population <- acs.fetch(endyear = 2015, geography = geo, table.number = "B01003", col.names = "pretty")

# Get SF Crime
#crime = data.table::fread("Crime.csv")
crime = data.table::fread("crime.csv")
crime = subset(crime, Category!="" & X <= -122.3656)

sf = get_map(location = "San Francisco, CA", maptype = "roadmap", source = "google", zoom = 11)

map = ggmap(sf)

map

#Hex Bin Crime by Location
# crime$Count <- sample(1:3, nrow(crime), replace = TRUE)
crime$Count <- 1
summary.hexmap <- map + coord_equal() +
  stat_summary_hex(aes(x = X, y = Y, z = Count,
                       fill = cut(..value..,
                                  c(0, 5000, 10000, 15000, 20000,
                                    25000, 30000, 35000, Inf))),
                   fun = sum,
                   #colour = NA,
                   bins = 30,
                   alpha = 0.95,
                   data = crime) +
  scale_fill_brewer(palette = "OrRd",
                    labels = c("<5000", "5000-10000", "10000-15000",
                               "15000-20000", "20000-25000",
                               "25000-30000", "30000-35000",
                               ">35000")) +
  labs(fill = NULL,
       title = "Crimes in SF, by Location")

print(summary.hexmap)


#Print Population Map
pop_df <- data.frame(paste0(str_pad(population@geography$state, 2, "left", pad="0"), 
                            str_pad(population@geography$county, 3, "left", pad="0"), 
                            str_pad(population@geography$tract, 6, "left", pad="0"),
                            str_pad(population@geography$blockgroup, 1, "left", pad="0")),
                     population@estimate[,c("Total Population: Total")],
                     stringsAsFactors = FALSE)

pop_df <- select(pop_df, 1:2)
rownames(pop_df)<-1:nrow(pop_df)
names(pop_df)<-c("GEOID", "total")

pop_merged<- geo_join(blocks, pop_df, "GEOID", "GEOID")

popup4 <- paste0("GEOID: ", pop_merged$GEOID, "-", pop_merged$total)
pal4 <- colorNumeric(
  palette = "Blues",
  domain = pop_merged$total)

# mapSFP<-leaflet() %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addPolygons(data = pop_merged, 
#               fillColor = ~pal4(total), 
#               color = "#b2aeae", # you need to use hex colors
#               fillOpacity = 0.5, 
#               weight = 1, 
#               smoothFactor = 0.2,
#               popup = popup4) %>%
#   addLegend(pal = pal4, 
#             values = pop_merged$total, 
#             position = "bottomright", 
#             title = "Population") 
# mapSFP


#######=======
#convert the blocks to polygons
blockPolys <- sp::polygons(blocks)

#get the projection from blockpolys
prj <- raster::crs(blockPolys)

#create points from the crime df
crimeLocs <- sp::SpatialPoints(data.frame(x=as.numeric(crime$X), y=as.numeric(crime$Y)), raster::crs(prj))

#put in same projection
sp::spTransform(crimeLocs, prj)

#calculate the crime in each block
crime$population <- pop_df[sp::over(crimeLocs, blockPolys), "total"]

#source:http://unconj.ca/blog/custom-hexbin-functions-with-ggplot.html

#get the hexagons for crime
crime.hex <- hexbin::hexbin(crime$X, crime$Y,
                             xbins = 30, IDs = TRUE)

# Deconstruct the relevant hexbin information into a dataset:
hexagons <- data.frame(hexbin::hcell2xy(crime.hex),
                       cell = crime.hex@cell,
                       count = crime.hex@count)

#get the cell ids for each hexagon
crime$cell = crime.hex@cID
 
# nondrug.crime.hexagons <- crime %>%
#   filter(Category != "DRUG/NARCOTIC") %>%
#   group_by(cell) %>%
#   summarise(crime = n()) %>%
#   ungroup() %>%
#   # Cut here, instead of in the ggplot call (as above).
#   mutate(crime.level = cut(crime, c(0, 100, 250, 500,
#                                      1000, 1500, 2000,
#                                      2500, Inf),
#                            labels = c("<100", "100-250",
#                                       "250-500", "500-1000",
#                                       "1000-1500", "1500-2000",
#                                       "2000-2500", ">2500"))) %>%
#   right_join(hexagons, by = "cell") %>%
#   select(cell, x, y, count, crime, crime.level)
# 
# head(nondrug.crime.hexagons, 10)
# 
# manual.hexmap <- map + coord_equal()+
#   geom_hex(aes(x = x, y = y, fill = crime.level),
#            stat = "identity", colour = NA, alpha = 0.75,
#            data = nondrug.crime.hexagons) +
#   scale_fill_brewer(palette = "OrRd") +
#   labs(fill = NULL,
#        title = "Non-drug Crimes in London, by Location (2012-2013)")
# 
# print(manual.hexmap)

#======================

#get the cell centroids as spatialpoints
cellCentroids <- sp::SpatialPoints(data.frame(x=hexagons$x, y=hexagons$y), raster::crs(prj))

#create a buffer around each point using the width of the hexagons
#this is an approximation of the hexagon area. Maybe elliptical is closer or 
#another way of getting a better approximation of hexagon for intersection with 
#block polygons
hexCentroidBuffered <- rgeos::gBuffer(cellCentroids, byid = T, width = diff(crime.hex@xbnds)/crime.hex@xbins)

#https://gis.stackexchange.com/questions/140504/extracting-intersection-areas-in-r

#get the area of each block
 pop_merged$area <- sapply(pop_merged@polygons, FUN=function(x) {slot(x, 'area')})

 #intersect the buffered hex centroids with the block dataframe containing pop
 intxt <- raster::intersect(hexCentroidBuffered, pop_merged)
 
 #intersecct the buffered hex centroids with the block polygons. This is a duplication
 #to get the block polygon IDs. There should be a better way
 intxt1 <- raster::intersect(hexCentroidBuffered, blockPolys)
 
 #copy the data part of the intersected polygons
 intxtData <- intxt@data
 
 #get the hexagon ID and block ID that were intersected to get this polygon
 intxtData[, c("hexID", "blockID")] <- t(data.frame(strsplit(sapply(intxt1@polygons, FUN=function(x) {slot(x, 'ID')}), " ")))
 
 #get the hexagon cellID
 intxtData$cellID <- hexagons$cell[as.numeric(intxtData$hexID)]
 
 #get the area of the resulting intersected polygon
 intxtData$intxtArea <- sapply(intxt@polygons, FUN=function(x) {slot(x, 'area')})
 
 #get the area of the intersected block
 intxtData$blockArea <- sapply(intxtData$GEOID, FUN=function(x) {pop_merged$area[which(pop_merged$GEOID == x)]})
 
 #get the proportion of population of each block that is in each hexagon intersected with
 #i.e intersected area / block area * block population
 intxtData$popRatio <- with(intxtData, intxtArea/blockArea*total)
 
 #get the crime counts in each hexagon
 intxtData$crimeCount <- sapply(intxtData$cellID, FUN=function(x) {hexagons$count[which(hexagons$cell == x)]})

 #convert to a data.table for easier aggregate stat calcs
 intxtData <- data.table(intxtData)
 
 #aggregate the data: mean crimeCount since it is the same for each unique hexagon
 #sum popratio to get the total population under the hexagon
 intxtDataAggd <- intxtData[, .(crimeCount=mean(crimeCount), population=sum(popRatio)), by=list(intxtData$cellID)]
 
 #rename the cellID col to cell to match the hexagon cell for joining later
 names(intxtDataAggd)[1] <- "cell"
 
 #test by visualizing
# plot(hexCentroidBuffered[as.numeric(unique(intxtData$hexID)),], border='blue')
# plot(blocks[as.numeric(intxtData$blockID),], add=TRUE)
 
 #calculate crime.level as crimecount/population
 crime.pop.hexagons <- intxtDataAggd %>%
   # Cut here, instead of in the ggplot call (as above).
   mutate(crime.level = cut(crimeCount/population, 8
                            )) %>%
   right_join(hexagons, by = "cell") %>%
   select(cell, x, y, crimeCount, population, crime.level)
 
 #head(nondrug.crime.hexagons, 10)
 
 #map it
 manual.hexmap <- map + coord_equal() +
   stat_summary_hex(aes(x = x, y = y, z = crimeCount/population,
                        fill = cut(..value..,
                                   8)),
                    #fun = sum,
                    #colour = NA, 
                    bins = 30, 
                    alpha = 0.65,
                    data = crime.pop.hexagons) +
   scale_fill_brewer(palette = "OrRd") +
   labs(fill = NULL,
        title = "Crimes in SF, by Location")
 
 print(manual.hexmap)
 