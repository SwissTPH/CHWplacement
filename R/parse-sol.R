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
#' @return a data.frame containing details on CHW optimization results
#' @export
#'
parse_gurobi_sol = function(population.raster, friction.raster,shp,
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

  # Parse results
  in_map <- raster::Which(all.raster$pop.map>0, cells=TRUE) # cell numbers of cells with pop
  n_inmap <- length(in_map) # Number of populated pixels

  # Separation of sol file
  sol_summary <- solution.sol[1:2]
  sol_chw = as.numeric(sapply(
    strsplit(solution.sol[3:(n_inmap+2)], " "), function(x) x[2]))
  sol_capacity_vec <- as.numeric(sapply(
    strsplit(solution.sol[(n_inmap+3):length(solution.sol)], " "), function(x) x[2]))

  # Checks
  if(check == TRUE) {
    if(length(sol_chw) != n_inmap){break}
    if(length(sol_capacity_vec) != n_inmap^2){break}
  }

  # Get CHW locations
  chw_list <- which(sol_chw > 0)
  chw_locations <- as.data.frame(raster::xyFromCell(all.raster$pop.map,in_map[chw_list]))

  # Get capacity totals
  pop_values <- raster::values(all.raster$pop.map)[in_map]
  urb_values <- raster::values(all.raster$urb.map)[in_map]

  # Organize capacity values into matrix
  capacity_mat <- matrix(data = sol_capacity_vec, ncol = n_inmap, byrow = T)

  # Get capacity values
  capacity_values <- t(capacity_mat) %*% pop_values

  # Get classification for footprint values
  urban_class <- apply(capacity_mat, 2,
                       function(x) ifelse(is.na(urb_values[which(x != 0)]),
                                          "rural", "urban"))
  catchment_type <- sapply(urban_class,
                           function(x) ifelse(length(unique(x)) > 1,
                                              "mixed", unique(x)))

  # Classify if CHW is in a rural pixel
  chw_rural <- ifelse(is.na(all.raster$urb.map[in_map[chw_list]]), 1,0)

  # Output as dataframe
  out <- data.frame(
    chw_locations,
    n_chw = sol_chw[chw_list],
    coverage_pop = capacity_values[chw_list],
    in_rural_pix = chw_rural,
    catchment_type = catchment_type[chw_list])

  # Export
  if(write) utils::write.csv(out,
                             file=file.path(filepath,
                                            paste0("clscp_",radius,"_buffer",buffer,"_capa",
                                                   capacity.name,"/positions_popcov_",name,".csv")),
                             row.names = F)

  return(out)
}
