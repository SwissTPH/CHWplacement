#' @title Gets the footprint around a pixel
#' @description This function returns the coordinates of all the points in the surrounding of a
#' given list of spatial points. The surrounding is based on walking distance within
#' radius, using the transition matrix T.CG associated to the map pop.map
#' @param pos coordinates of the given points
#' @param radius dimension of the radius around the point (same dimension as T.GC, e.g minutes)
#' @param pop.map raster map containing population densities
#' @param T.GC transition matrix T.CG associated to the map pop.map
#'
#' @return a raster
#' @export
#'
GetFootprintWalkingTime= function(pos, radius, pop.map, T.GC){

  point.locations=as.data.frame(pos)
  point.locations$name=row.names(point.locations)
  names(point.locations) <- c("X_COORD", "Y_COORD", "name")

  # Keep only point coordinates within the shapefile bounds
  sp::coordinates(point.locations) <- ~ X_COORD + Y_COORD
  sp::proj4string(point.locations) <- sp::proj4string(pop.map)
  raster::crs(point.locations)<-raster::crs(pop.map)


  points <- as.matrix(point.locations@coords)
  this.access.raster <- gdistance::accCost(T.GC, points)
  footprint=which(raster::values(this.access.raster)<radius)
  return(footprint)
}
