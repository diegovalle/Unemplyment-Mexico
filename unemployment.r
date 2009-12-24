#pretty maps of the unemployment rate
library(ggplot2)
library(maptools)     # loads sp library too
library(RColorBrewer) # creates nice color schemes
library(classInt)     # finds class intervals for continuous variables
library(reshape)
library(sp)
try_require("maps")

####################################################################
#Change the directories as appropiate
wd <- "C:/Documents and Settings/Diego/My Documents/docs/personal/Math/unemployment"
map.icesi <- "../maps/Mexico.shp"
#########################################################################

setwd(wd)

#WTF, according to the ICESI there's a state called "Baja California Norte"
#lol, the LHC must have caused the download to come from a parallel universe
mx <- readShapeSpatial(map.icesi)
mx.map <- fortify(mx, region = "NAME")

un <- read.csv("unemployment.csv")
total <- subset(un, un$States == "Total")
#Third trimester 2009: 6.2%
ggplot(melt(subset(un, un$States == "Total")),
      aes(substring(variable, 2), I(value/100), group = 1)) +
      geom_line() + ylab("")+ xlab("Quarter") +
      opts(title = "Unemployment Rate in Mexico") +
      scale_y_continuous(formatter = "percent") +
      scale_x_discrete(breaks = 
                    c("2005.I","2006.I","2007.I","2007.I","2008.I","2009.I"))
dev.print(png,"Unemployment Rate.png", width=600, height=600)


#A plot with the states filled in, according to the unemployment rate
un <- subset(un, un$States != "Total")
un$States <- NULL
#when merging order mathers and you have to place the map first!
un.states <- merge(mx.map, un, by.y = "States.ICESI", by.x = "id", all.x = T)
un.m <- melt(un.states,id=c("id", "long" , "lat",
                            "order", "hole", "piece", "group"))
un.m$variable <- substring(un.m$variable,  2)
un.m <- un.m[un.m$variable %in% c("2007.I", "2007.II","2007.III","2007.IV",
                                  "2008.I", "2008.II","2008.III","2008.IV",
                                  "2009.I","2009.II","2009.III"), ]
#Cohuila and Chihuahua have the highest rate at 9.72%
#Oaxaca has the lowest rate with 1.74%
ggplot(un.m, aes(long, lat)) +
       geom_polygon(aes(group = group, fill = I(value/100)), color = I("white")) +
       facet_wrap(~ variable, ncol = 4) +
       theme_bw() + 
       coord_map(project="gilbert") +
       scale_fill_continuous("Unemployment", formatter="percent", 
                             low="yellow", high="red", limits=c(.01,.1)) +
       opts(title = "Unemployment rate in Mexico (2007–2009), by State") +
       scale_y_continuous(breaks = NA) +
       scale_x_continuous(breaks = NA) + xlab("") + ylab("") 
dev.print(png,"Unemployment in Mexico, by state.png", width=800, height=700)

#People tend to pay attention to the big states when we fill them
#so now a plot with bubbles
#http://had.co.nz/stat405/lectures/15-time-space.pdf
un.m <- melt(un,id="States.ICESI")
#un.states <- merge(un.m, mx.map, by.x = "States.ICESI", by.y = "id", all.y = T)
un.m$variable <- substring(un.m$variable,  2)
un.m <- un.m[un.m$variable %in% c("2007.I", "2007.II","2007.III","2007.IV",
                                  "2008.I", "2008.II","2008.III","2008.IV",
                                  "2009.I","2009.II","2009.III"), ]
un.m$variable <- paste(un.m$variable, as.character((rep(total[11:ncol(total)], 
                       each = 32)), sep = " – ") 
un.m$variable <- paste(un.m$variable, "%", sep = "")
mid_range <- function(x) mean(range(x))
centres <- ddply(mx.map, c("id"), summarise,
                 lat = mid_range(lat), long = mid_range(long))

bubble <- merge(un.m, centres, by.x = "States.ICESI", by.y = "id")
ggplot(bubble, aes(long, lat)) +
       geom_polygon(aes(group = group), fill = NA,
                    color = I("black"), data = mx.map) +
       theme_bw() +
       geom_point(aes(color = I(value/100)), size = 4) +
       scale_colour_gradient("Unemployment", formatter="percent", 
                             low="yellow", high="red", limits=c(.01,.1)) +
       facet_wrap(~ variable) +
       coord_map(project="gilbert") +
       opts(title = "Unemployment rate in Mexico (2007–2009), by State") +
       scale_y_continuous(breaks = NA) +
       scale_x_continuous(breaks = NA) + xlab("") + ylab("") 
dev.print(png,"Unemployment Rate Bubbles.png", width=900, height=600)
#mmmh


