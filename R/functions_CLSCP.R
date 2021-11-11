#' @title Creates the integer program associated with the CLSCP problem
#' @description This function executes the CLSCP problem to find optimal locations (Current & Storbeck)
#'  using the lpsolveAPI package for integer programming.
#' @param pop.map raster map containing population densities
#' @param access.raster raster containing access to HF variable
#' @param radius dimension of the radius around the point (same dimension as T.GC, e.g minutes)
#' @param T.GC transition matrix T.CG associated to the map pop.map
#' @param max.treat.per.CHW.urban max number of people per CHW in urban areas
#' @param max.treat.per.CHW.rural max number of people per CHW in rural areas
#' @param max.CHW.per.pixel maximum number of CHW that can be placed on the same pixel
#' @param popurb raster map containing the urban population layer
#' @param directory directory where to store the MPS file
#' @param name name of the MPS file
#'
#' @return a MPS file containing the optimisation program
#' @export
#'
OptiCLSCPWalkingTimeMPS=function(pop.map, access.raster,
                                 radius, T.GC,
                                 popurb="none",
                                 max.treat.per.CHW.urban,max.treat.per.CHW.rural,
                                 max.CHW.per.pixel=1,
                                 directory, name){

  #####################################################################################
  # This function executes the CLSCP problem to find optimal locations (Current & Storbeck)
  # using the lpsolveAPI package for integer programming
  #####################################################################################

  # 1. prepare data
  pop.gt1h.zeros=pop.map
  pop.gt1h.zeros[is.na(pop.gt1h.zeros)==TRUE]=0  # replace NA by zeroes

  # 2. get the vector of populations and the sizes
  objective=raster::values(pop.gt1h.zeros)
  inmap=raster::Which(pop.gt1h.zeros>0, cells=TRUE) # cell numbers of cells with pop

  I=length(inmap)  # number of demand points
  J=length(inmap)  # number of candidate points

  ####
  # 3. set LP program

  # Lp creation of MPS has a limitations on the format of its comlumne
  # Thus the number of decision variables can't be more than 9 999 999
  if (J+I*J> 9999999){ stop(" Too many decision var: Lp limitation ")}

  lprec <- lpSolveAPI::make.lp(0, J+I*J)
  lpSolveAPI::set.objfn(lprec,c(rep(1,I),rep(0,I*J)))  # objective is only based on demand points covered
  lpSolveAPI::set.bounds(lprec, lower = rep(0,J+I*J))  # lower bound is zero for all decision variables

  if(max.CHW.per.pixel== Inf){
    lpSolveAPI::set.bounds(lprec,  upper = c(rep(1,I*J)), columns = c((J+1):(J+I*J))) # upper bound is 1 for inmap decision variables

  }else{
    lpSolveAPI::set.bounds(lprec,  upper = c(rep(max.CHW.per.pixel,J),rep(1,I*J))) # upper bound is 1 for inmap decision variables
  }
  lpSolveAPI::set.type(lprec, columns = 1:J, type = "integer") # all decision variables are integers
  lpSolveAPI::lp.control(lprec,sense="min")  # maximization program


  footprint=t(sapply(inmap,
                     function(e) GetFootprintWalkingTime(raster::xyFromCell(pop.gt1h.zeros,e),
                                                         radius=radius,pop.map=pop.gt1h.zeros, T.GC=T.GC ) ))

  for (i in 1:I){
    my.vect.x=rep(0,J)
    my.vect.x[which(inmap %in% footprint[[i]])]=1
    lpSolveAPI::add.constraint(lprec, my.vect.x, ">=", 1, indices=((J+J*(i-1)+1):(J+J*i)) ) # constraint that all demand is covered
  }


  my.obj=objective[inmap]
  my.rururb=raster::values(popurb)[inmap]
  capacity=ifelse(is.na(my.rururb),max.treat.per.CHW.rural,max.treat.per.CHW.urban)

  for (j in 1:J){
    my.vect.capa=rep(0,J)
    my.vect.capa[j]= -capacity[j]

    my.vect.az=rep(0,I*J)
    my.vect.az[j+((1:I)-1)*J]=my.obj
    lpSolveAPI::add.constraint(lprec, c(my.vect.capa,my.vect.az), "<=", 0)
  }
  # print the program
  print(lprec)
  lpSolveAPI::write.lp(lprec,file.path(directory,paste0( name,".mps")), type="mps")

}

