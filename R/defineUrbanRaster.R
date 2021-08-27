#' @title Defines rural and urban areas based on population density
#' @description This function defines urban areas as those with a population density
#' higher than rururb_cutoff per pixel. Urban areas are also contiguous and have a minimal
#' total population equal to min_urbsize. The methodology is derived from \url{http://documents.worldbank.org/curated/en/709121516634280180/pdf/122880-V1-WP-P156561-OUO-9-FINAL-ENGLISH.pdf}
#' @param population.raster raster map containing population densities
#' @param rururb_cutoff minimal population density to define an urban area
#' @param min_urbsize minimal total population size to define an urban area
#'
#' @return a raster similar to population.raster in which all non-urban pixels are masked (i.e. NA)
#' @export
DefineUrban=function(population.raster, rururb_cutoff=300, min_urbsize=2000){
  names(population.raster)="pop"
  # create a new raster indicating pixels above the cutoff
  popurb=population.raster
  popurb[popurb<=rururb_cutoff ]=as.integer(0)
  popurb[popurb>rururb_cutoff ]=as.integer(1)

  # identify contiguous clusters of pixels above the cutoff
  pop.urb.cl=raster::clump(popurb, directions=8)
  pop.urb.cl.shp=raster::disaggregate(raster::rasterToPolygons(pop.urb.cl,dissolve=TRUE,fun=function(x){x>=1}))

  # retrieve the population in each cluster and select if above min_urbsize
  pop.sp <- raster::extract(population.raster, pop.urb.cl.shp, fun=sum, na.rm=TRUE, sp=TRUE)
  urb.raster=raster::mask(population.raster, pop.sp[pop.sp$pop>min_urbsize,])

  print(paste0("Proportion of the population in urban areas is ",round(sum(raster::values(urb.raster) , na.rm = TRUE)*100/sum(raster::values(population.raster) , na.rm = TRUE), digits = 2), "%"))

  return(urb.raster)
}

