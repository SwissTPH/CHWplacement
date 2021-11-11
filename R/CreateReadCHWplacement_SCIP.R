
############ Creation of a Script in order to use SCIP on an MPS file ########################################
#' @title Creation of a Script in order to use SCIP on an MPS file
#' @description This function use Paths informations to produce a txt file which containes two Script
#' A manual one which can be use in every OS and version, and an Automated one in Ps1 (Windows script)
#' If the Windows script don't work, verify the Script version and maybe change the "SCIP_path" in the function.
#' @param mps_directory directory of the mps file
#' @param name          name of the mps file
#' @param dualitygap    condition of suboptimality acceptance for the solver:
#'                      give an upper bounds of the difference with an hypothetical optimal solution
#' @param relativegap   if true -> |primal-dual|/MIN(|dual|,|primal|)
#'                      else       |primal-dual|    (more interpretable but less accurate)
#' @param SCIP_version usefull for creating SCIP_path so only for windows user
#' @export

CreateScriptSCIP <- function(mps_directory, name, dualitygap = 0, relativegap = T , SCIP_version = "7.0.3"){

  # Paths definition
  CLSCP_path     <- file.path( mps_directory, paste0( name,".mps")                 )
  Solutions_path <- file.path( mps_directory, paste0( name,"_solutions.sol")       )
  script_path    <- file.path( mps_directory, paste0( "SCIP_script_", name,".txt") )
  SCIP_path      <- file.path(paste0( "C:/Program Files/SCIPOptSuite ",  SCIP_version ,"/bin/scip.exe"))# for windows user

  # for duality gap setting
  gaptype <- ifelse( relativegap, "gap ", "absgap ")

  # creation of the txt file
  fileConn<-file(script_path)
  writeLines( c(
    paste0( "*****  Manual use of SCIP version ", SCIP_version, "*****" ),
    "** Please enter the lines below one by one on the SCIP shell **",
    " ",
    paste0("set limits ", gaptype, dualitygap ),
    paste0("read ", CLSCP_path ),
    "optimize",
    paste0("write solution ", Solutions_path),
    "quit ",
    " ",
    " ",
    paste0( "*****  Automated use of SCIP version ", SCIP_version, "( for Windows users ) *****" ),
    "** Please enter the script below in windows powershell **",
    " ",
    paste0("Start-Process -FilePath ","'", SCIP_path,"'" ),
    "$wshell = New-Object -ComObject wscript.shell",
    "$wshell.SendKeys('%{TAB}')",
    "Sleep 2",
    paste0("$wshell.SendKeys( '", "set limits ", gaptype, dualitygap, "' )"),
    "$wshell.SendKeys('~')",
    "Sleep .3",
    paste0("$wshell.SendKeys( '", "read ", CLSCP_path, "' )"),
    "$wshell.SendKeys('~')",
    "Sleep 1",
    "$wshell.SendKeys('optimize' )",
    "$wshell.SendKeys('~')",
    "Wait-Process -Id $wshell.id",
    paste0("$wshell.SendKeys( '","write solution ",  Solutions_path, "' )"),
    "$wshell.SendKeys('~')",
    "Sleep .5",
    "$wshell.SendKeys('quit')",
    "$wshell.SendKeys('~')"

  ), fileConn)

  close(fileConn)
}


############ Creation of MPS file and txt.Script  ########################################
#' @title Creation of MPS file and txt.Script
#' @description This function executes the CLSCP problem to find optimal locations (Current & Storbeck)
#'  using the lpsolveAPI package for integer programming. Relies on the functions CHW_CreatProgScript_SCIP and  PrepareRasterFiles (see functions_CLSCP)
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
#' @param capacity.name name (character) given to the capacity definition (for folder naming purposes)
#' @param name (character) name of the MPS file
#' @param SCIP_version version of the SCIP shell downloaded
#' @param dualitygap    condition of suboptimality acceptance for the solver:
#'                      give an upper bounds of the difference with an hypothetical optimal solution
#' @param relativegap   if true -> |primal-dual|/MIN(|dual|,|primal|)
#'                      else       |primal-dual|    (more interpretable but less accurate)
#' @return a MPS file containing the optimisation program and txt script to run it in SCIP shell
#' @export

CreateCHWplacement_SCIP = function(   population.raster, friction.raster, access.raster, popurb.raster, shp,
                                   filepath, name,
                                   buffer, radius, capacity.name = "MSPP",
                                   max.treat.per.CHW.urban = 2500, max.treat.per.CHW.rural = 1000,
                                   max.CHW.per.pixel, is.inside = FALSE,
                                   dualitygap = 1,
                                   relativegap = F,
                                   SCIP_version = "7.0.3" ){


  all.rasters = PrepareRasterFiles( population.raster = population.raster,
                                    friction.raster = friction.raster,
                                    access.raster = access.raster,
                                    popurb.raster = popurb.raster,
                                    shp = shp,
                                    buffer = buffer,
                                    is.inside = is.inside)

  extendedpath = file.path( filepath, paste0( "clscp_", radius, "_buffer", buffer, "_capa", capacity.name ))
  if(!dir.exists(extendedpath)){
    print( paste0("create dir ", paste0("clscp_",radius,"_buffer",buffer,"_capa",capacity.name) ) )
    dir.create(extendedpath)
  }
  OptiCLSCPWalkingTimeMPS( pop.map = all.rasters$pop.map,
                            radius  = radius,
                            T.GC = all.rasters$T.GC,
                            popurb  = all.rasters$urb.map,
                            max.treat.per.CHW.urban = max.treat.per.CHW.urban,
                            max.treat.per.CHW.rural = max.treat.per.CHW.rural,
                            max.CHW.per.pixel = max.CHW.per.pixel,
                            directory = extendedpath,
                            name = name )

  CreateScriptSCIP(mps_directory = extendedpath, name = name,
                   dualitygap = dualitygap, relativegap = relativegap ,
                   SCIP_version = SCIP_version )

}



################## Read SCIP results ####################################################

#' @title Read SCIP results
#' @description This function tranforms the output of a SCIP opitimisation into map coordinates
#' @param population.raster raster map containing population densities
#' @param friction.raster raster map containing friction surface
#' @param access.raster raster map containing the distance to HF layer
#' @param popurb.raster raster map containing the urban population layer (urban pixels show the population in the pixel and the rural pixels are NA). Can be created with \link[CHWplacement]{DefineUrban}.
#' @param buffer buffer around current HF
#' @param radius dimension of the radius around the point (same dimension as T.GC, e.g minutes)
#' @param shp shapefile of the area of interest
#' @param is.inside whether CHW are placed inside the buffer (if FALSE, they are placed outside)
#' @param filepath directory where to store the MPS file
#' @param write if TRUE, the output is written as a csv file in filepath
#' @param name name (character) of the MPS file
#' @param capacity.name name (character) given to the capacity definition (for folder naming purposes)
#'
#' @return a csv file containing the optimisation program
#' @export
#'

read_CHWplacement_sol_SCIP=function( population.raster, friction.raster, access.raster, popurb.raster, shp,
                          name, filepath,radius, capacity.name,
                          buffer,is.inside , write = F ){

  # import file
  fileName=file.path(filepath,paste0("clscp_",radius,"_buffer",buffer,"_capa",capacity.name,"/", name,"_solutions.sol"))
  solution=readLines(fileName)
  # SCIP raw format is close to .sol and give the no null variables names (C...) and their values
  # and their coefficients in objective function (obj:...)
  # thus (obj:1) is in our case linked to Y_j values (how many CHW in the location j) and (obj:0) to X_ij (coverage)

  # How many CHW?
  total.CHW = solution[2]           %>%
              stringr::str_extract( "\\d+" ) %>%
              as.numeric()

  sprintf("Total number of CHW = %.f", total.CHW)


  # SCIP raw format reformatting
  df_solution <- solution %>%
    data.frame() %>%
    dplyr::slice( 3:dplyr::n() ) %>%
    dplyr::rename( "col" = ".") %>%
    tidyr::separate( col, c("id", "value", "CHW"), sep = "[[:blank:]]+")

  df_solution$id = as.numeric( stringr::str_extract( df_solution$id, "\\d+") )
  df_solution$value = as.numeric( df_solution$value )
  df_solution$CHW = as.numeric( stringr::str_extract( df_solution$CHW, "\\d+") )



  df_CHW <- df_solution %>% dplyr::filter( CHW == 1) %>% dplyr::select(- CHW)
  rownames( df_CHW ) <- df_CHW$id
  df_coverage <- df_solution %>% dplyr::filter( CHW == 0) %>% dplyr::select(- CHW)

  #  prepare map
  all.rasters = PrepareRasterFiles( population.raster = population.raster,
                                    friction.raster   = friction.raster,
                                    access.raster     = access.raster,
                                    popurb.raster     = popurb.raster, shp = shp,
                                    buffer = buffer,  is.inside = is.inside       )

  pop_map = all.rasters$pop.map
  pop_map[is.na(pop_map)==T]=0  # replace NA by zeroes

  # get the vector of populations
  i_populated = raster::Which( pop_map>0, cells=TRUE ) # cell numbers of cells with pop
  # return( i_populated)

  I = length( i_populated )




  # sanitycheck: does the sum of CHW corresponds to output of objective
  if ( abs( sum( df_CHW$value )-total.CHW ) >0.001 ) stop("error reading file")


  ## get coordinates and write output file

  list.CHW = df_CHW$id

  # get coordinates of CHWs
  positions.CHW = as.data.frame(raster::xyFromCell(pop_map,i_populated[list.CHW]))

  positions.CHW$is.rural=ifelse(is.na(all.rasters$urb.map[i_populated[list.CHW]]), 1,0)

   # get pop to calculate the capacity
  pop = raster::values(pop_map)
  populated = pop[i_populated]

  df_CHW$capacity <- rep(0, nrow(df_CHW)) # initialisation

  # The index of the coverage X_ij is equal by definition to J+j+(i-1)*I and because I=J, id = j+i*I for i in 1:I
  # So i = (id-j)/I

  for (j in list.CHW){

     # construction of the df associated to the coverage by a location j
     cov.j = df_coverage %>%
       dplyr::filter( id %in% ( j + (1:I)*I)) %>% # we select the X_ij for j fixed
       dplyr::mutate( id = ( id - j )/I ) %>% # calculate the i index
       dplyr::mutate( nb_cov = value * populated[ id ] ) # calculate the number of people covered

     # The capacity is the total number of people covered by CHW
     df_CHW[ as.character(j), "capacity" ] = sum( cov.j$nb_cov )/df_CHW[ as.character(j), "value" ]


  }

  #write file

  CHW_res = cbind( positions.CHW, df_CHW$capacity, df_CHW$value )
  names(CHW_res) = c("x", "y", "is.rural", "capacity", "value")

  CHW_res <- as.data.frame(lapply(CHW_res, rep, df_CHW$value)) %>%
    dplyr::select( - value)



  if(write) utils::write.csv(CHW_res, file=file.path(filepath,paste0("clscp_",radius,"_buffer",buffer,"_capa",capacity.name,"/positions_popcov_",name,"_SCIP.csv")), row.names = F)
  return(CHW_res)
}

