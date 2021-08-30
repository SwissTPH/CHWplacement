#=============================================================================
#=============================================================================
# 			main_CLSCP_clust.R
#
# R-code will suggest optimal placement of CHWs based on CLSCP covering problem (Current and Storbeck)
# The radius of impact of CHWs considers walking time
#		- Step 0: Read shapefiles, incidence, population, walking time available data
#   - Step 1: Function to write the problem in MPS format
#   - Step 2: Function to read the output from Gurobi
#
#  created by :  Clara Champagne  (clara.champagne@swisstph.ch)
#  originally created : 2019
#
#
##=============================================================================
# outstanding TODO
#=============================================================================
rm(list=ls())
print("launch R")

cmd_args       <- commandArgs()
ll             <- length(cmd_args)
workingDir     <-   cmd_args[(ll-10)]
write_read     <-   cmd_args[(ll-9)]
id1     <-   cmd_args[(ll-8)]
capacity_name     <-   cmd_args[(ll-7)]
buffer     <-   cmd_args[(ll-6)]
isinside     <-   cmd_args[(ll-5)]
capaurb     <-   cmd_args[(ll-4)]
caparur     <-   cmd_args[(ll-3)]
rururb_cutoff   <-   cmd_args[(ll-2)]
minurbsize   <-   cmd_args[(ll-1)]
radius     <-   cmd_args[ll]


############################################################################
# LOAD LIBRARIES
############################################################################

library(raster)
library(sp)
library(ggplot2)
library(rgeos)
library(rgdal)
library(lpSolveAPI)
library(plyr)


############################################################################
# SET UP DIRECTORY PATHS
############################################################################
workingDir=workingDir
dirInputs=file.path(workingDir,"inputs")
dirOutputs=file.path(workingDir,"outputs")


if(!dir.exists(file.path(dirOutputs,paste0("clscp_",radius,"_buffer",buffer,"_capa",capacity_name)))){
  print("create dir in")
  dir.create(file.path(dirOutputs,paste0("clscp_",radius,"_buffer",buffer,"_capa",capacity_name)))
}


# path_package=file.path(workingDir,"CHWplacement")
# install.packages(path_package, repos=NULL, type="source")
library(CHWplacement)
# ############################################################################
# # DECLARE GLOBAL VARIABLES
# ############################################################################
#
# ## Standard projection
newproj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0"


############################################################################
# STEP 0: IMPORT RELEVANT DATASETS
############################################################################

### Names of input layers
#population
pop.FileName=file.path(dirInputs,"/popadj.tif") # to be updated with the corresponding path

#section communale shapefile
seccom.NameFile=file.path(dirInputs,"Section Communale/HTIadm4poly.shp") # to be updated with the corresponding path

# dowloading the friction surface. shp arguments speeds up by selecting only the pixels in the shapefile
friction.FileName=file.path(dirInputs,"friction_surface_v47_wo.tif") # to be updated with the corresponding path
friction=raster(friction.FileName,proj4string = CRS(newproj))

# list of health facilities in SPA survey
point.locations.spa.FileName=file.path(dirInputs,"Haiti_SPA_GPSPTS_HTGE7BFLSR/HTGE7BFLSR.shp") # to be updated with the corresponding path


### Charging the data
# Charge shapefile
seccom.shp=shapefile(seccom.NameFile)
seccom.shp=spTransform(seccom.shp,newproj)


# charge population
population=raster(pop.FileName,proj4string = newproj)
population.haiti=mask(crop(population, seccom.shp), seccom.shp)
sum(values(population.haiti), na.rm = T)

popurb.haiti=DefineUrban(population.raster=population.haiti, rururb_cutoff=as.numeric(rururb_cutoff), min_urbsize=as.numeric(minurbsize))

# charge friction layer
friction=raster(friction.FileName,proj4string = newproj)
friction.haiti=raster::crop(friction,seccom.shp) # crop before to reduce map size
friction.haiti=raster::mask(friction.haiti,seccom.shp) # crop before to reduce map size
friction.haiti=projectRaster(friction.haiti,population.haiti) # have the same extend and cell number as population raster
population.haiti=mask(population.haiti,friction.haiti) # keep only points for which we have a friction value

# compute the transition matrix
T <- gdistance::transition(friction.haiti, function(x) 1/mean(x), 8)
Haiti.T.GC <- gdistance::geoCorrection(T)

# compute walking time to health facilites
SPA.shp=shapefile(point.locations.spa.FileName)
SPA.shp=spTransform(SPA.shp,newproj)

#to dispensaries (centres communautaires de sant?)
points.spa.CCS <- as.matrix(subset(SPA.shp,SPATYPEN=="dispensaire/centre communautaire de sante")@coords)
access.raster.spa.CCS  <- gdistance::accCost(Haiti.T.GC, points.spa.CCS )
access.raster.spa.CCS =mask(access.raster.spa.CCS ,seccom.shp)


seccom.shp@data$NAME_1b=seccom.shp@data$NAME_1
seccom.shp@data$NAME_1b[seccom.shp@data$NAME_2=="Dessalines" | seccom.shp@data$NAME_2=="Saint-Marc"]="Artibonite Sud"
seccom.shp@data$NAME_1b[seccom.shp@data$NAME_2=="Marmelade" | seccom.shp@data$NAME_2=="Gonaives" | seccom.shp@data$NAME_2=="Gros Morne"]="Artibonite Nord"

seccom.shp@data$NAME_1b[seccom.shp@data$NAME_2=="Arcahaie" | seccom.shp@data$NAME_2=="Croix-Des-Bouquets"]="Ouest E"
seccom.shp@data$NAME_1b[ seccom.shp@data$NAME_2=="Leogane" | seccom.shp@data$NAME_2=="Gonave"]="Ouest O"

seccom.shp@data$NAME_1b[seccom.shp@data$NAME_2=="Port-Au-Prince" & seccom.shp@data$ID_4< 73072]="Aire metropolitaine"
seccom.shp@data$NAME_1b[seccom.shp@data$NAME_2=="Port-Au-Prince" & seccom.shp@data$ID_4>= 73072 & seccom.shp@data$ID_4<= 73076]="Ouest E"
seccom.shp@data$NAME_1b[seccom.shp@data$NAME_2=="Port-Au-Prince" & seccom.shp@data$ID_4>= 73077]="Ouest O"

seccom.shp@data$NAME_1b[seccom.shp@data$NAME_2=="Ceca La Source" | seccom.shp@data$NAME_2=="Hinche"]="Centre Nord"
seccom.shp@data$NAME_1b[seccom.shp@data$NAME_2=="Lascahobas" | seccom.shp@data$NAME_2=="Mirebalais"]="Centre Sud"


dpt.list2=c("GrandeAnse"="Grande Anse", "Nippes"="Nippes",
            "NordEst"= "Nord Est","NordOuest"= "Nord Ouest",
            "SudEst"="Sud Est","Nord"="Nord","Sud"="Sud",
            "ArtiboniteSud"="Artibonite Sud","ArtiboniteNord"="Artibonite Nord",
            "CentreNord"="Centre Nord","CentreSud"="Centre Sud",
            "OuestO"="Ouest O","OuestE"="Ouest E","AireMetro"="Aire metropolitaine")


############
# SCENARIO

if(write_read=="write"){

  CreateCHWplacement(population.raster=population, friction.raster=friction,
                     shp=subset(seccom.shp, NAME_1b==dpt.list2[names(dpt.list2)==id1]),
                     name=id1, buffer=as.numeric(buffer), radius=as.numeric(radius), capacity.name=capacity_name,
                     access.raster=access.raster.spa.CCS,popurb.raster=popurb.haiti,
                     max.treat.per.CHW.urban=as.numeric(capaurb), max.treat.per.CHW.rural=as.numeric(caparur),
                     max.CHW.per.pixel=Inf, is.inside = as.logical(as.numeric(isinside)), filepath=dirOutputs)

  print("launch gurobi")
}

if(write_read=="read"){
  read_new=read_CHWplacement_sol(population.raster=population, friction.raster=friction,  shp=subset(seccom.shp, NAME_1b==dpt.list2[names(dpt.list2)==id1]),
                                 name=id1, buffer=as.numeric(buffer), radius=as.numeric(radius), capacity.name=capacity_name,
                                 access.raster=access.raster.spa.CCS,popurb.raster=popurb.haiti,
                                 is.inside=as.logical(as.numeric(isinside)), filepath=dirOutputs, write=TRUE)
}
