######################################################################

#' @title Solve optimisation problem with R (lpSolve API)
#' @description This function tranforms the output of a Gurobi opitimisation into map coordinates
#' @param population.raster raster map containing population densities
#' @param friction.raster raster map containing friction surface
#' @param access.raster raster map containing the distance to HF layer
#' @param popurb.raster raster map containing the urban population layer
#' @param shp shapefile of the area of interest
#' @param buffer buffer around current HF
#' @param radius dimension of the radius around the point (same dimension as T.GC, e.g minutes)
#' @param is.inside whether CHW are placed inside the buffer (if FALSE, they are placed outside)
#' @param filepath directory where to store the MPS file
#' @param write f TRUE, the output is written as a csv file in filepath
#' @param name name of the MPS file
#' @param capacity.name ??
#'
#' @return a MPS file containing the optimisation program
#' @export
#'
solve_CHWplacement = function(population.raster,friction.raster,
                              shp, buffer,access.raster, radius,capacity.name ,
                              popurb.raster, name,
                              is.inside=F, write=F, filepath){

  all.raster=PrepareRasterFiles(population.raster=population.raster,
                                friction.raster=friction.raster,
                                shp=shp, buffer=buffer, access.raster=access.raster,
                                popurb.raster=popurb.raster,
                                is.inside=is.inside)
  solution=lpSolveAPI::read.lp(paste0("./clscp_",radius,"_buffer",buffer,"_capa",capacity.name,"/",name,".mps"), type="mps")

  if(lpSolveAPI::dim.lpExtPtr(solution)[2]>200000){stop("the program is too large to be solve with R")}
  lpSolveAPI::solve.lpExtPtr(solution)

  total.CHW=lpSolveAPI::get.objective(solution)
  print(total.CHW)

  all.variables=lpSolveAPI::get.variables(solution)

  population.map=all.raster$pop.map
  population.map[is.na(population.map)==T]=0  # replace NA by zeroes
  inmap=raster::Which(population.map>0, cells=TRUE) # cell numbers of cells with pop
  length(inmap)

  # get CHW positions
  my.solution2=data.frame(is.CHW=all.variables[1:length(inmap)],  index2=1:length(inmap))

  list.CHW=my.solution2$index2[my.solution2$is.CHW==1]
  positions.CHW=as.data.frame(raster::xyFromCell(population.map,inmap[list.CHW])) # get coordinates of CHWs
  positions.CHW$is.rural=ifelse(is.na(all.raster$urb.map[inmap[list.CHW]]), 1,0)

  # get the vector of populations
  my.capa=all.variables[(length(inmap)+1):length(all.variables)]

  objective=raster::values(population.map)
  my.obj=objective[inmap]
  my.list.capa.here=c()
  for (j in list.CHW){
    my.capa.j.df=data.frame(index=1:length(inmap), is.cov=my.capa[j+((1:length(inmap))-1)*length(inmap)])
    my.capa.j.df$cov.pop=my.capa.j.df$is.cov*my.obj
    my.list.capa.here=c(my.list.capa.here,sum(as.numeric(my.capa.j.df$cov.pop)))
  }

  #write file
  positions.CHW.popcov=cbind(positions.CHW, my.list.capa.here)
  names(positions.CHW.popcov)=c("x", "y", "is.rural", "capacity")
  if(write) utils::write.csv(positions.CHW.popcov, file=file.path(filepath,paste0("clscp_",radius,"_buffer",buffer,"_capa",capacity.name,"/positions_popcov_",name,".csv")), row.names = F)

  return(positions.CHW.popcov)
}




######################################################################

#' @title Solve optimisation problem with R (lpSolve API)
#' @description This function tranforms the output of a Gurobi opitimisation into map coordinates
#' @param population.raster raster map containing population densities
#' @param friction.raster raster map containing friction surface
#' @param access.raster raster map containing the distance to HF layer
#' @param popurb.raster raster map containing the urban population layer
#' @param shp shapefile of the area of interest
#' @param buffer buffer around current HF
#' @param radius dimension of the radius around the point (same dimension as T.GC, e.g minutes)
#' @param is.inside whether CHW are placed inside the buffer (if FALSE, they are placed outside)
#' @param filepath directory where to store the MPS file
#' @param write f TRUE, the output is written as a csv file in filepath
#' @param name name of the MPS file
#' @param capacity.name ??
#' @param mytimeout maximal time (in seconds) that the function is allowed to search for the maximum
#'
#' @return a MPS file containing the optimisation program
#' @export
#'
#'
solve_CHWplacement_wrapper = function(population.raster,friction.raster,
                              shp, buffer,access.raster, radius,capacity.name ,
                              popurb.raster, name,
                              is.inside=F, write=F, filepath, mytimeout=10){
  return(R.utils::withTimeout(solve_CHWplacement(population.raster=population.raster, friction.raster=friction.raster,shp=shp,
                                        name=name, buffer=buffer, radius=radius, capacity.name=capacity.name,
                                        access.raster=access.raster,popurb.raster=popurb.raster,
                                        is.inside = is.inside, filepath=filepath), timeout=mytimeout, elapsed=mytimeout, onTimeout = "warning" ))
}
