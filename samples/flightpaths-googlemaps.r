#' Fortify method for classes from the sp package.
#' 
#' To figure out the correct variable name for region, inspect 
#' \code{as.data.frame(model)}.
#' 
#' @param model \code{SpatialPolygonsDataFrame} to convert into a dataframe.
#' @param data not used by this method
#' @param region name of variable used to split up regions
#' @param ... not used by this method
#' @name fortify.sp
#' @examples
#' if (require("maptools")) {
#'  sids <- system.file("shapes/sids.shp", package="maptools")
#'  nc1 <- readShapePoly(sids, 
#'    proj4string = CRS("+proj=longlat +datum=NAD27"))
#'  nc1_df <- fortify(nc1) 
#' }
NULL

#' @rdname fortify.sp
#' @export
#' @method fortify SpatialPolygonsDataFrame
fortify.SpatialPolygonsDataFrame <- function(model, data, region = NULL, ...) {
  attr <- as.data.frame(model)
  # If not specified, split into regions based on polygons
  if (is.null(region)) {
    coords <- ldply(model@polygons,fortify)
    message("Regions defined for each Polygons")
  } else {
    cp <- polygons(model)
    try_require("maptools")

    # Union together all polygons that make up a region
    unioned <- unionSpatialPolygons(cp, attr[, region])
    coords <- fortify(unioned)
    coords$order <- 1:nrow(coords)
  }
  coords
}

#' @rdname fortify.sp
#' @export
#' @method fortify SpatialPolygons
fortify.SpatialPolygons <- function(model, data, ...) {
  ldply(model@polygons, fortify)
}

#' @rdname fortify.sp
#' @export
#' @method fortify Polygons
fortify.Polygons <- function(model, data, ...) {
  subpolys <- model@Polygons
  pieces <- ldply(seq_along(subpolys), function(i) {
    df <- fortify(subpolys[[model@plotOrder[i]]])
    df$piece <- i
    df
  })
  
  within(pieces,{
    order <- 1:nrow(pieces)
    id <- model@ID
    piece <- factor(piece)
    group <- interaction(id, piece)
  })
}

#' @rdname fortify.sp
#' @export
#' @method fortify Polygon
fortify.Polygon <- function(model, data, ...) {
  df <- as.data.frame(model@coords)
  names(df) <- c("long", "lat")
  df$order <- 1:nrow(df)
  df$hole <- model@hole
  df
}

#' @rdname fortify.sp
#' @export
#' @method fortify SpatialLinesDataFrame
fortify.SpatialLinesDataFrame <- function(model, data, ...) {
  ldply(model@lines, fortify)
}

#' @rdname fortify.sp
#' @export
#' @method fortify Lines
fortify.Lines <- function(model, data, ...) {
  lines <- model@Lines
  pieces <- ldply(seq_along(lines), function(i) {
    df <- fortify(lines[[i]])
    df$piece <- i
    df
  })
  
  within(pieces,{
    order <- 1:nrow(pieces)
    id <- model@ID
    piece <- factor(piece)
    group <- interaction(id, piece)
  })
}

#' @rdname fortify.sp
#' @export
#' @method fortify Line
fortify.Line <- function(model, data, ...) {
  df <- as.data.frame(model@coords)
  names(df) <- c("long", "lat")
  df$order <- 1:nrow(df)
  df  
}








library(maps)
library(geosphere)
library(plyr) 
library(ggplot2)
library(sp)
 
airports <- read.csv("data/airports.csv", as.is=TRUE, header=TRUE)
flights <- read.csv("http://www.stanford.edu/~cengel/cgi-bin/anthrospace/wp-content/uploads/2012/03/PEK-openflights-export-2012-03-19.csv", as.is=TRUE, header=TRUE)
 
# aggregate nunber of flights
flights.ag <- ddply(flights, c("From","To"), function(x) count(x$To))
 
# add latlons
flights.ll <- merge(flights.ag, airports, all.x=T, by.x="To", by.y="IATA")
beijing.ll <- c(airports$longitude[airports["IATA"]=="PEK"],airports$latitude[airports["IATA"]=="PEK"])
 
# calculate routes -- Dateline Break FALSE, otherwise we get a bump in the shifted ggplots
rts <- gcIntermediate(beijing.ll, flights.ll[,c('longitude', 'latitude')], 100, breakAtDateLine=FALSE, addStartEnd=TRUE, sp=TRUE)
rts.ff <- fortify.SpatialLinesDataFrame(rts) # convert into something ggplot can plot
 
flights.ll$id <-as.character(c(1:nrow(flights.ll))) # that rts.ff$id is a char
gcircles <- merge(rts.ff, flights.ll, all.x=T, by="id") # join attributes, we keep them all, just in case
 
 
### Recenter ####
 
center <- 115 # positive values only - US centered view is 260
 
# shift coordinates to recenter great circles
gcircles$long.recenter <-  ifelse(gcircles$long  < center - 180 , gcircles$long + 360, gcircles$long) 
 
# shift coordinates to recenter worldmap
worldmap <- map_data ("world")
worldmap$long.recenter <-  ifelse(worldmap$long  < center - 180 , worldmap$long + 360, worldmap$long)
 
### Function to regroup split lines and polygons
# takes dataframe, column with long and unique group variable, returns df with added column named group.regroup
RegroupElements <- function(df, longcol, idcol){  
  g <- rep(1, length(df[,longcol]))
  if (diff(range(df[,longcol])) > 300) {          # check if longitude within group differs more than 300 deg, ie if element was split
    d <- df[,longcol] > mean(range(df[,longcol])) # we use the mean to help us separate the extreme values
    g[!d] <- 1     # some marker for parts that stay in place (we cheat here a little, as we do not take into account concave polygons)
    g[d] <- 2      # parts that are moved
  }
  g <-  paste(df[, idcol], g, sep=".") # attach to id to create unique group variable for the dataset
  df$group.regroup <- g
  df
}
 
### Function to close regrouped polygons
# takes dataframe, checks if 1st and last longitude value are the same, if not, inserts first as last and reassigns order variable
ClosePolygons <- function(df, longcol, ordercol){
  if (df[1,longcol] != df[nrow(df),longcol]) {
    tmp <- df[1,]
    df <- rbind(df,tmp)
  }
  o <- c(1: nrow(df))  # rassign the order variable
  df[,ordercol] <- o
  df
}
 
# now regroup
gcircles.rg <- ddply(gcircles, .(id), RegroupElements, "long.recenter", "id")
worldmap.rg <- ddply(worldmap, .(group), RegroupElements, "long.recenter", "group")
 
# close polys
worldmap.cp <- ddply(worldmap.rg, .(group.regroup), ClosePolygons, "long.recenter", "order")  # use the new grouping var
 
# plot
 
ggplot() +
  geom_polygon(aes(long.recenter,lat,group=group.regroup), size = 0.2, fill="#f9f9f9", colour = "grey65", data=worldmap.cp) +
  geom_line(aes(long.recenter,lat,group=group.regroup, color=freq, alpha=freq), size=0.4, data= gcircles.rg) +        # set transparency here
  scale_colour_gradient(low="#fafafa", high="#EE0000") +                                                              # set color gradient here
  theme(panel.background = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(),  axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position = "none") +
  ylim(-60, 90) +
  coord_equal()
