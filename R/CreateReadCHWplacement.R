#' @title Prepare raster files for CHW placement algorithm
#' @description This function combined the different rasters and shapefiles needed to create the optimisation program
#' @param population.raster raster map containing population densities
#' @param friction.raster raster map containing friction surface
#' @param access.raster raster map containing the distance to HF layer
#' @param popurb.raster raster map containing the urban population layer (urban pixels show the population in the pixel and the rural pixels are NA). Can be created with \link[CHWplacement]{DefineUrban}.
#' @param shp shapefile of the area of interest
#' @param buffer buffer around current HF
#' @param is.inside whether CHW are placed inside the buffer (if FALSE, they are placed outside)
#'
#' @return a list containing the pre-processed rasters and transition matrix
#' @export
#'
PrepareRasterFiles=function(population.raster, friction.raster,shp,
                             buffer, access.raster,popurb.raster, is.inside){

  pop.map=raster::mask(raster::crop(population.raster, shp), shp)

  friction.map=raster::crop(friction.raster,shp) # crop before to reduce map size
  friction.map=raster::mask(friction.map,shp)
  friction.map=raster::projectRaster(friction.map,pop.map)  # have the same extend and cell number as population raster
  pop.map=raster::mask(pop.map,friction.map)  # keep only points for which we have a friction value

  GreaterThanBuffer<-raster::mask(raster::crop(access.raster, shp), shp)
  if(is.inside==TRUE) {
    GreaterThanBuffer[GreaterThanBuffer>=buffer]<-NA
  } else {
    GreaterThanBuffer[GreaterThanBuffer<buffer]<-NA
  }

  pop.map=raster::mask(pop.map,GreaterThanBuffer)

  urb.map=raster::mask(raster::crop(popurb.raster, shp),shp)

  access.map=raster::mask(raster::crop(access.raster, shp), shp)
  access.map=raster::projectRaster(access.map,pop.map)  # have the same extend and cell number as population raster

  # compute the transition matrix
  TT <- gdistance::transition(friction.map, function(x) 1/mean(x), 8)
  T.GC <- gdistance::geoCorrection(TT)

  return(list("pop.map"=pop.map,"access.map"=access.map,
              "T.GC"=T.GC,"urb.map"=urb.map))
}



######################################################################


#' @title Creates the integer program associated with the CHW placement problem
#' @description This function combined the different rasters and shapefiles needed to create the optimisation program
#' @param population.raster raster map containing population densities
#' @param friction.raster raster map containing friction surface
#' @param access.raster raster map containing the distance to HF layer
#' @param popurb.raster raster map containing the urban population layer (urban pixels show the population in the pixel and the rural pixels are NA). Can be created with \link[CHWplacement]{DefineUrban}.
#' @param shp shapefile of the area of interest
#' @param buffer buffer around current HF
#' @param is.inside whether CHW are placed inside the buffer (if FALSE, they are placed outside)
#' @param radius dimension of the radius around the point (same dimension as T.GC, e.g minutes)
#' @param max.treat.per.CHW.urban max number of people per CHW in urban areas
#' @param max.treat.per.CHW.rural max number of people per CHW in rural areas
#' @param max.CHW.per.pixel maximum number of CHW that can be placed on the same pixel
#' @param filepath directory where to store the MPS file
#' @param name name (character) of the MPS file
#' @param capacity.name name (character) given to the capacity definition (for folder naming purposes)
#'
#' @return a MPS file containing the optimisation program
#' @export
#'
CreateCHWplacement=function(population.raster, friction.raster,shp,
                            name, buffer, radius, capacity.name="MSPP",
                            access.raster,popurb.raster,
                            max.treat.per.CHW.urban=2500, max.treat.per.CHW.rural=1000,
                            max.CHW.per.pixel, is.inside=FALSE, filepath){


  all.raster=PrepareRasterFiles(population.raster=population.raster,
                                friction.raster=friction.raster,shp=shp,
                              buffer=buffer, access.raster=access.raster,
                              popurb.raster=popurb.raster, is.inside=is.inside)

  extendedpath=file.path(filepath,paste0("clscp_",radius,"_buffer",buffer,"_capa",capacity.name))
  if(!dir.exists(extendedpath)){
    print(paste0("create dir ", paste0("clscp_",radius,"_buffer",buffer,"_capa",capacity.name)))
    dir.create(extendedpath)
  }

  CHW.opti=OptiCLSCPWalkingTimeMPS(pop.map=all.raster$pop.map,
                                   access.raster=all.raster$access.map,
                                   popurb=all.raster$urb.map,
                                   radius=radius,T.GC=all.raster$T.GC, name=name,
                                   directory=extendedpath,
                                   max.treat.per.CHW.urban=max.treat.per.CHW.urban,
                                   max.treat.per.CHW.rural=max.treat.per.CHW.rural,
                                   max.CHW.per.pixel=max.CHW.per.pixel)

}



######################################################################

#' @title Read Gurobi output
#' @description This function tranforms the output of a Gurobi opitimisation into map coordinates
#' @param population.raster raster map containing population densities
#' @param friction.raster raster map containing friction surface
#' @param access.raster raster map containing the distance to HF layer
#' @param popurb.raster raster map containing the urban population layer (urban pixels show the population in the pixel and the rural pixels are NA). Can be created with \link[CHWplacement]{DefineUrban}.
#' @param shp shapefile of the area of interest
#' @param buffer buffer around current HF
#' @param radius dimension of the radius around the point (same dimension as T.GC, e.g minutes)
#' @param is.inside whether CHW are placed inside the buffer (if FALSE, they are placed outside)
#' @param filepath directory where to store the MPS file
#' @param write if TRUE, the output is written as a csv file in filepath
#' @param name  name (character) of the .sol file
#' @param capacity.name name (character) given to the capacity definition (for folder naming purposes)
#' @param check Run sanity checks?
#'
#' @return a MPS file containing the optimisation program
#' @export
#'
read_CHWplacement_sol=function(population.raster, friction.raster,shp,
                       name, buffer, radius, capacity.name,
                       access.raster,popurb.raster,
                       is.inside, filepath, write=F, check = TRUE){

  # import file
  fileName=file.path(filepath,paste0("clscp_",radius,"_buffer",buffer,"_capa",capacity.name,"/", name,".sol"))
  solution.sol=readLines(fileName)

  # find objective value
  obj.idx=grep("Objective value", solution.sol)
  total.CHW=as.numeric(unlist(strsplit(solution.sol[obj.idx],"="))[2])
  print(total.CHW)

  #  prepare map
  all.raster=PrepareRasterFiles(population.raster=population.raster,
                                friction.raster=friction.raster,shp=shp,
                                buffer=buffer, access.raster=access.raster,
                                popurb.raster=popurb.raster, is.inside=is.inside)

  population.map=all.raster$pop.map
  population.map[is.na(population.map)==T]=0  # replace NA by zeroes

  # get the vector of populations
  inmap=raster::Which(population.map>0, cells=TRUE) # cell numbers of cells with pop

  ################################
  # get solution
  my.solution=solution.sol[(obj.idx+1):(length(inmap)+1)]
  my.solution2=data.frame(t(data.frame(lapply(1:length(my.solution), function(e) strsplit(my.solution[e]," ")))))
  row.names(my.solution2)=NULL
  names(my.solution2)=c("index", "is.CHW")
  my.solution2$is.CHW=as.numeric(as.character(my.solution2$is.CHW))

  # sanity check: does the sum of CHW corresponds to output of objective
  if(check == TRUE) {
    if(abs(sum(my.solution2$is.CHW)-total.CHW) >0.001) stop("error reading file") # test coherence
  }


  my.solution2$index2=as.numeric(sub("C","",my.solution2$index))
  if (all(my.solution2$is.CHW<2) ){
    list.CHW=my.solution2[my.solution2$is.CHW==1,]$index2
  } else {
    list.CHW=unlist(sapply(1:max(my.solution2$is.CHW), function(e) rep(my.solution2[abs(my.solution2$is.CHW-e) <0.001,]$index2, e)))
  }

  # get coordinates and write output file
  positions.CHW=as.data.frame(raster::xyFromCell(population.map,inmap[list.CHW])) # get coordinates of CHWs
  positions.CHW$is.rural=ifelse(is.na(all.raster$urb.map[inmap[list.CHW]]), 1,0)

  ###################################
  # get capacity per CHW
  my.capa=solution.sol[(length(inmap)+2):length(solution.sol)]
  objective=raster::values(population.map)
  my.obj=objective[inmap]
  my.list.capa.here=c()
  for (j in list.CHW){
    my.capa.j=my.capa[j+((1:length(inmap))-1)*length(inmap)]
    my.capa.j.df=data.frame(t(data.frame(lapply(1:length(my.capa.j), function(e) strsplit(my.capa.j[e]," ")))))
    row.names(my.capa.j.df)=NULL
    names(my.capa.j.df)=c("index", "is.cov")
    my.capa.j.df$is.cov=as.numeric(as.character(my.capa.j.df$is.cov))
    my.capa.j.df$cov.pop=my.capa.j.df$is.cov*my.obj
    my.list.capa.here=c(my.list.capa.here,sum(as.numeric(my.capa.j.df$cov.pop)))
  }

  #write file
  positions.CHW.popcov=cbind(positions.CHW, my.list.capa.here)
  names(positions.CHW.popcov)=c("x", "y", "is.rural", "capacity")
  if(write) utils::write.csv(positions.CHW.popcov, file=file.path(filepath,paste0("clscp_",radius,"_buffer",buffer,"_capa",capacity.name,"/positions_popcov_",name,".csv")), row.names = F)

  return(positions.CHW.popcov)
}

