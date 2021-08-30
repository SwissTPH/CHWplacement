#=============================================================================
# 			ASCP PLACEMENT IN HAITI : POSTPROCESSING AND PLOTTING THE RESULTS
#
#  Plotting and postprocessing scenarios for CHW manuscript (April 2021)
#
#  created by : Clara Champagne  (clara.champagne@swisstph.ch), Emilie Pothin
#  2020-2021
#
#   0. Import datasets
#   1. Plot the data on maps
#   2. Plot the scenarios and calculate the capacities
#   3. Compare with current placement (GAP analysis)
#   4. Plot results of robustness check on scenario C
#
#=============================================================================
#=============================================================================
rm(list=ls())
############################################################################
# LOAD LIBRARIES
############################################################################

library(raster)
library(sp)
library(reshape)
library(ggplot2)
library(rgeos)
library(gdistance)
library(RColorBrewer)
library(foreign)
library(broom)
library(maptools)
library(readxl)
library(dplyr)
library(spatialEco)
library(stringr)
library(cowplot)
library(tidyverse)
library(CHWplacement)
############################################################################
# SET UP DIRECTORY PATHS
############################################################################

### Define input layers directory
dirMain="."  # to be updated with the corresponding path
dirPlots=file.path(dirMain,"figures") # to be updated with the desired path
dirInputs=file.path(dirMain,"inputs")
dirOutputs=file.path(dirMain, "outputs")
dirSPA=file.path(dirInputs,"SPA") # to be updated with the corresponding path

dirScripts="." # to be updated with the corresponding path
source(file.path(dirScripts,"functions_Post_Processing_figures.R"))

############################################################################
# LOAD FUNCTIONS
############################################################################
newproj=NULL
newproj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"


############################################################################
# STEP 0: IMPORT RELEVANT DATASETS
############################################################################

#### Names of input layers
#population
pop.FileName=file.path(dirInputs, "popadj20_1km.tif")

#section communale shapefile
seccom.NameFile=file.path(dirInputs,"Section Communale/HTIadm4poly.shp")

# dowloading the friction surface. shp arguments speeds up by selecting only the pixels in the shapefile
friction.FileName=file.path(dirInputs,"friction_surface_v47_wo.tif")

# list of health facilities in the SPA survey
point.locations.spa.FileName=file.path(dirSPA,"Haiti_SPA_GPSPTS_HTGE7BFLSR/HTGE7BFLSR.shp")

######################
# IMPORT CHW FILES

dpt.list2=c("GrandeAnse"="Grande Anse", "Nippes"="Nippes",
            "NordEst"= "Nord Est","NordOuest"= "Nord Ouest",
            "SudEst"="Sud Est","Nord"="Nord",
            "ArtiboniteSud"="Artibonite Sud","ArtiboniteNord"="Artibonite Nord",
            "CentreNord"="Centre Nord","CentreSud"="Centre Sud",
            "OuestO"="Ouest O","OuestE"="Ouest E",
            "Sud"="Sud", "AireMetro"="Aire metropolitaine")

switch_dpt=function(this_dept){
  return(ifelse(this_dept %in% c("ArtiboniteSud","ArtiboniteNord"),"Artibonite",
                ifelse(this_dept %in% c("CentreSud","CentreNord"),"Centre",
                       ifelse(this_dept %in% c("OuestE","OuestO"),"Ouest", this_dept))))
}


# list of scenarios
CHW.positions.scenarioA.CSLCP=data.frame()
CHW.positions.scenarioB.CSLCP=data.frame()
CHW.positions.scenarioCin.CSLCP=data.frame()
CHW.positions.scenarioCout.CSLCP=data.frame()
CHW.positions.scenarioCin2.CSLCP=data.frame()
for (i in names(dpt.list2)){
  print(i)
  this.data=read.csv(file.path(dirOutputs,paste0("clscp_60_SPACCS30_capa25001000clump300in0/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  this.data$out=1
  CHW.positions.scenarioB.CSLCP=rbind(CHW.positions.scenarioB.CSLCP,this.data)

  this.data=read.csv(file.path(dirOutputs,paste0("clscp_60_SPACCS60_capa40004000clump300in1/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  this.data$out=0
  CHW.positions.scenarioCin.CSLCP=rbind(CHW.positions.scenarioCin.CSLCP,this.data)

  this.data=read.csv(file.path(dirOutputs,paste0("clscp_60_SPACCS60_capa25001000clump300in0/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  this.data$out=1
  CHW.positions.scenarioCout.CSLCP=rbind(CHW.positions.scenarioCout.CSLCP,this.data)

  this.data=read.csv(file.path(dirOutputs,paste0("clscp_60_SPACCS60_capa40001000clump300in1/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  this.data$out=0
  CHW.positions.scenarioCin2.CSLCP=rbind(CHW.positions.scenarioCin2.CSLCP,this.data)
}

for (i in names(dpt.list2)[1:13]){
  print(i)
  this.data=read.csv(file.path(dirOutputs,paste0("clscp_60_SPACCS0_capa25001000clump300in0/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  this.data$out=1
  CHW.positions.scenarioA.CSLCP=rbind(CHW.positions.scenarioA.CSLCP,this.data)
}
this.data=read.csv(file.path(dirOutputs,paste0("clscp_60_SPACCS0_capa40004000clump300in0/positions_popcov_","AireMetro",".csv")))
this.data$metro=1
this.data$out=1
this.data$dpt="AireMetro"
CHW.positions.scenarioA.CSLCP=rbind(CHW.positions.scenarioA.CSLCP,this.data)


######################
# IMPORT shapefile

# charge shapefile
seccom.shp=raster::shapefile(seccom.NameFile)
seccom.shp=spTransform(seccom.shp,newproj)
plot(seccom.shp)

# define aire metropolitaine
seccom.shp@data$departement=seccom.shp@data$NAME_1
seccom.shp@data$departement[seccom.shp@data$NAME_2=="Port-Au-Prince" & seccom.shp@data$ID_4< 73072]="Aire metropolitaine"
seccom.shp@data$departement[seccom.shp@data$departement=="Ouest"]="Reste Ouest"

# shapefile at department level
seccom.shp.union <- unionSpatialPolygons(seccom.shp, seccom.shp@data$departement)

# some corrections on the shapefile
seccom.shp@data$NAME_4[seccom.shp@data$ID_4==73112]="1ère Varreux"
seccom.shp@data$NAME_4[seccom.shp@data$ID_4==73113]

seccom.shp@data$ID_4[seccom.shp@data$ID_4==73532 & seccom.shp@data$NAME_4=="6ème Les Cayemites"]=73532.5


######################
# IMPORT POPULATION

### charge population raster
# charge file
population=raster(pop.FileName,proj4string = CRS(newproj))
names(population)= 'popadj'
population.haiti=mask(crop(population, seccom.shp), seccom.shp)
sum(values(population.haiti), na.rm = T)

# define urban and rural areas
population.urb.hai=DefineUrban(population.raster=population.haiti,
                         rururb_cutoff=300, min_urbsize=2000)
population.urb.hai.dummy=mask((!is.na(population.urb.hai)), seccom.shp)

#calculate basic ratios and summary statistics on rural/urban
pop.haiti=sum(values(population.haiti), na.rm = TRUE)
pop.haiti.urb=sum(values(population.urb.hai), na.rm = TRUE)
pop.haiti.rur=sum(values(mask(population.haiti,population.urb.hai, inverse=T)), na.rm = TRUE)

pop.haiti.urb/pop.haiti
pop.haiti.urb/2500+pop.haiti.rur/1000

pop.metrop=sum(values(mask(population.haiti, subset(seccom.shp,departement=="Aire metropolitaine" ))), na.rm = TRUE)
pop.nometrop=sum(values(mask(population.haiti, subset(seccom.shp,departement!="Aire metropolitaine" ))), na.rm = TRUE)
pop.haiti.urb.nometrop=sum(values(mask(population.urb.hai, subset(seccom.shp,departement!="Aire metropolitaine" ))), na.rm = TRUE)
pop.haiti.rur.nometrop=sum(values(mask(mask(population.haiti,population.urb.hai, inverse=T), subset(seccom.shp,departement!="Aire metropolitaine" ))), na.rm = TRUE)

pop.metrop/pop.haiti
pop.haiti.urb.nometrop/pop.haiti
pop.haiti.rur.nometrop/pop.haiti

pop.haiti.urb.nometrop/2500+pop.haiti.rur.nometrop/1000+pop.metrop/4000

# calculate surface for urban and rural
surface=population-population
values(surface)=1
surface2=mask(surface, seccom.shp)
area=sum(!is.na(surface2[]))  # total shapefile surface
area.pop=sum(!is.na(population.haiti[])) # total populated surface
area.pop.urb=sum(!is.na(population.urb.hai[])) # total populated urban surface
area.pop/area
area.pop.urb/area
area.pop.urb/area.pop


###########################################################
# IMPORT TRAVEL TIME AND COMPUTE TIME TO HEALTH FACILITIES
### Friction and travel time
# charge frition raster
friction=raster(friction.FileName,proj4string = CRS(newproj))
friction.haiti=raster::crop(friction,seccom.shp) # crop before to reduce map size
friction.haiti=raster::mask(friction.haiti,seccom.shp)
friction.haiti=projectRaster(friction.haiti,population.haiti) # have the same extend and cell number as population raster
population.haiti=mask(population.haiti,friction.haiti) # keep only points for which we have a friction value

# compute the transition matrix
T <- gdistance::transition(friction.haiti, function(x) 1/mean(x), 8)
Haiti.T.GC <- gdistance::geoCorrection(T)

# CREATE SUB CATEGORIES OF HEALTH FACILITIES
# compute walking time to health facilites
SPA.shp=shapefile(point.locations.spa.FileName)
SPA.shp=spTransform(SPA.shp,newproj)
CCS.shp=subset(SPA.shp,SPATYPEN=="dispensaire/centre communautaire de sante")

# CALCULATE TRAVEL TIMES AND CREATE MASKED LAYERS
#to all health facilities
points.spa <- as.matrix(SPA.shp@coords)
access.raster.spa <- gdistance::accCost(Haiti.T.GC, points.spa)
access.raster.spa=mask(access.raster.spa,seccom.shp)

#to dispensaries (centres communautaires de sant?)
points.spa.CCS <- as.matrix(CCS.shp@coords)
access.raster.spa.CCS  <- gdistance::accCost(Haiti.T.GC, points.spa.CCS )
access.raster.spa.CCS =mask(access.raster.spa.CCS ,seccom.shp)

# descriptive statistics on population totals
GreaterThanBuffer=access.raster.spa.CCS
GreaterThanBuffer[GreaterThanBuffer<=60]<-NA
population.haiti.buffer=mask(population.haiti,GreaterThanBuffer)
population.urb.hai.buffer=mask(population.urb.hai,GreaterThanBuffer)
pop.outside.buffer=sum(values(population.haiti.buffer), na.rm = TRUE)
pop.urb.outside.buffer=sum(values(population.urb.hai.buffer), na.rm = TRUE)


pop.outside.buffer/pop.haiti
pop.urb.outside.buffer/pop.haiti
(pop.haiti-pop.outside.buffer)/4000
pop.urb.outside.buffer/2500 + (pop.outside.buffer-pop.urb.outside.buffer)/1000
(pop.haiti-pop.outside.buffer)/4000+ pop.urb.outside.buffer/2500 + (pop.outside.buffer-pop.urb.outside.buffer)/1000

GreaterThanBuffer30=access.raster.spa.CCS
GreaterThanBuffer30[GreaterThanBuffer30<=30]<-NA
population.haiti.buffer30=mask(population.haiti,GreaterThanBuffer30)
population.urb.hai.buffer30=mask(population.urb.hai,GreaterThanBuffer30)
pop.outside.buffer30=sum(values(population.haiti.buffer30), na.rm = TRUE)
pop.urb.outside.buffer30=sum(values(population.urb.hai.buffer30), na.rm = TRUE)
pop.outside.buffer30/pop.haiti
pop.urb.outside.buffer30/2500 + (pop.outside.buffer30-pop.urb.outside.buffer30)/1000

##########################################
# CUSTOMIZING LEGENDS

colormapDT0=data.frame(color=c('#ffffb2','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026'))
colormapDT0$value=c(1,100,500,1000,5000,10000,1000000)
colormapDT0$label=c(colormapDT0$value[-7],">10000")

colormapDTrurub=data.frame(color=c('#ffffb2','#b10026'))
colormapDTrurub$value=c(-0.1,1)
colormapDTrurub$label=c("rural","urban")

colormapDTb=data.frame(color=rev(c('#d73027','#fdae61','#fee090','#e0f3f8','#abd9e9','lightblue3')))
colormapDTb$value=c(30,45,60,90,120,Inf)
colormapDTb$label=c(colormapDTb$value[-6],">120")

colormapDTb2=data.frame(color=rev(c('#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1')))
colormapDTb2$value=c(0.01,0.025,0.05,0.075,0.1,0.5)*1000
colormapDTb2$label=colormapDTb2$value

#=======================================================
# 1. PLOT THE MAPS OF DATA
#======================================================

map_rururb<-PlotMap(population.urb.hai.dummy,Title.legend=" ",shpFile=seccom.shp,colormapDT=colormapDTrurub)
map_rururb<-map_rururb+theme(legend.key.size = unit(0.8, "cm"),legend.position = c(0.12, 0.7),legend.text=element_text(size=10),legend.title=element_text(size=10))
map_rururb
ggsave(map_rururb, file=file.path(dirPlots,"rururb_national.jpg"), width=12,height=10, units = "cm")
ggsave(map_rururb, file=file.path(dirPlots,"rururb_national.pdf"), width=12,height=10, units = "cm")


seccom.shp.union2=SpatialPolygonsDataFrame(seccom.shp.union,data.frame(dpt=unique(seccom.shp$departement)), match.ID = F)
seccom.shp.union2@data=data.frame(dpt=unique(seccom.shp$NAME_1))
all_coords=as.data.frame(coordinates(seccom.shp.union2))
all_coords$dpt=row.names(all_coords)

#=======================================================
# 2. EXTRACT SCENARIOS
#=======================================================

# Scenarios
CHW.positions.scenarioA.CSLCP=plot_scenario_all_cslscp(CHW.positions.scenarioA.CSLCP, "scenarioA")
CHW.positions.scenarioB.CSLCP=plot_scenario_all_cslscp(CHW.positions.scenarioB.CSLCP, "scenarioB")
CHW.positions.scenarioC.CSLCP=plot_scenario_all_cslscp(rbind(CHW.positions.scenarioCin.CSLCP,CHW.positions.scenarioCout.CSLCP), "scenarioC")
CHW.positions.scenarioC2.CSLCP=plot_scenario_all_cslscp(rbind(CHW.positions.scenarioCin2.CSLCP,CHW.positions.scenarioCout.CSLCP), "scenarioC2")



###################################
# a. PLOT SCENARIOS PER DEPARTMENT
###################################


plot_dpt_ouest=plot_dpt_summary("Ouest")
ggsave(plot_dpt_ouest, file=file.path(dirPlots,"figure_all_Ouest.jpg"), width=12, height=7)

plot_dpt_GA=plot_dpt_summary("GrandeAnse")
ggsave(plot_dpt_GA, file=file.path(dirPlots,"figure_all_GrandeAnse.jpg"), width=12, height=7)

plot_dpt_Sud=plot_dpt_summary("Sud")
ggsave(plot_dpt_Sud, file=file.path(dirPlots,"figure_all_Sud.jpg"), width=16, height=7)

plot_dpt_SudEst=plot_dpt_summary("SudEst")
ggsave(plot_dpt_SudEst, file=file.path(dirPlots,"figure_all_SudEst.jpg"), width=16, height=7)

plot_dpt_Nord=plot_dpt_summary("Nord")
ggsave(plot_dpt_Nord, file=file.path(dirPlots,"figure_all_Nord.jpg"), width=9, height=7)

plot_dpt_NordEst=plot_dpt_summary("NordEst")
ggsave(plot_dpt_NordEst, file=file.path(dirPlots,"figure_all_NordEst.jpg"), width=9, height=7)

plot_dpt_NordOuest=plot_dpt_summary("NordOuest")
ggsave(plot_dpt_NordOuest, file=file.path(dirPlots,"figure_all_NordOuest.jpg"), width=12, height=7)

plot_dpt_Centre=plot_dpt_summary("Centre")
ggsave(plot_dpt_Centre, file=file.path(dirPlots,"figure_all_Centre.jpg"), width=10, height=7)

plot_dpt_Nippes=plot_dpt_summary("Nippes")
ggsave(plot_dpt_Nippes, file=file.path(dirPlots,"figure_all_Nippes.jpg"), width=12, height=7)

plot_dpt_Artibonite=plot_dpt_summary("Artibonite")
ggsave(plot_dpt_Artibonite, file=file.path(dirPlots,"figure_all_Artibonite.jpg"), width=9, height=9)


##########################################################
# b. CALCULATE CAPACITIES AND SUMMARY STATISTICS ON SCENARIOS
##########################################################
# combine all files
capacities.all=rbind(CHW.positions.scenarioA.CSLCP %>% mutate(scenario="A"),
                     CHW.positions.scenarioB.CSLCP %>% mutate(scenario="B"),
                     CHW.positions.scenarioC.CSLCP  %>% mutate(scenario="C"),
                     CHW.positions.scenarioC2.CSLCP  %>% mutate(scenario="C2"))%>%
  mutate(categories=factor(ifelse(metro==1, "Metropolitan", ifelse(is.rural==1, "Rural", "Urban"))),
         urbrur=factor(ifelse(is.rural==0, "Urban", "Rural")),
         close2CCS=factor(ifelse(out==0, "(close to\nCCS)", "(far from\nCCS)")))
capacities.all$categories=factor(capacities.all$categories, levels=c("Metropolitan","Urban","Rural"))



#total per scenarios
total_per_scen=data.frame(table(capacities.all$scenario))
names(total_per_scen)=c("scenario", "totalCHW")
print(total_per_scen)


# total and proportion per rural/urban/metropolitan
total_per_category=data.frame(table(capacities.all$categories,capacities.all$scenario)) %>%
  rename(category=Var1, scenario=Var2,count=Freq) %>%
  left_join(total_per_scen, by="scenario") %>%
  mutate(prop=round(100*count/totalCHW))
print(total_per_category)

# plot total CHW numbers
total_per_category %>%
  mutate(totalCHW2=ifelse(category=="Metropolitan", total_per_category$totalCHW, NA),
         category2=factor(category, levels=c("Metropolitan","Urban","Rural"))) %>%
  ggplot(aes(x = scenario, y = count, fill =scenario, alpha= category2)) +  # Create stacked bar chart
  geom_bar(stat = "identity")+
  scale_fill_manual(name="" ,values=c("darkorange", "darkgreen", "dodgerblue", "dodgerblue4"), guide=F)+
  scale_alpha_manual(name="" ,values=c(1,0.7,0.4), labels=c("Metropolitan","Urban","Rural"), drop=F)+
  labs(x="", y="Number of CHWs")+theme_bw()+
  geom_text(aes(label = totalCHW2 , y=totalCHW2), vjust=-0.25, size=5, show.legend = FALSE)+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=14), legend.text = element_text(size=14)) -> ggp2
ggp2


# plot average inh. per CHW
test_df=capacities.all %>%
  mutate(categories_merge=factor(ifelse(scenario=="A", as.character(categories),as.character(urbrur)),levels=c("Metropolitan","Urban","Rural")) ,
         scenario_far=ifelse(scenario %in% c("A"), scenario,paste0(scenario,"\n",close2CCS ))) %>%
  group_by(scenario,close2CCS,categories_merge,scenario_far) %>%
  summarise(mean=mean(capacities), q75=quantile(capacities, 0.9), q25=quantile(capacities, 0.05) )

test_df_all=capacities.all %>%
  group_by(scenario) %>%
  summarise(mean=mean(capacities), q75=quantile(capacities, 0.9), q25=quantile(capacities, 0.05) )%>%
  mutate(scenario_far=paste0(scenario, "_Total"), close2CCS="Total", categories_merge="Total")

test_df_plot=rbind(test_df, test_df_all)
test_df_plot$scenario_far=factor(test_df_plot$scenario_far, levels=c("A_Total","A","B_Total", "B\n(far from\nCCS)","C_Total","C\n(close to\nCCS)","C\n(far from\nCCS)","C2_Total","C2\n(close to\nCCS)","C2\n(far from\nCCS)" ))
test_df_plot$categories_merge=factor(test_df_plot$categories_merge, levels=c("Total", "Metropolitan", "Urban", "Rural" ))

test_df_plot=arrange(test_df_plot, scenario_far)
test_df_plot$x.seq=c(1.2,2,2.5,3,4.2,5,5.5,6.7,7.5,8,8.7,9.2,10.4,11.2,11.7,12.4,12.9)

ggplot(test_df_plot, aes(x=x.seq, y=mean, fill=scenario_far, color=scenario_far, alpha=categories_merge, ymin=q25, ymax=q75, group=(categories_merge)))+
  scale_alpha_manual(name="" ,values=c(0,1,0.7,0.4))+
  geom_bar(stat="identity", position = position_dodge() )+
  geom_errorbar(position = position_dodge(.9), alpha=1, width=0.2)+
  ylim(-200,4100)+
  geom_text(aes(x=1.2, y=0,  label="Total \n"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  geom_text(aes(x=4.2, y=0,  label="Total \n"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  geom_text(aes(x=6.7, y=0,  label="Total \n"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  geom_text(aes(x=10.4, y=0,  label="Total \n"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  geom_text(aes(x=5.2, y=0,  label="far from\nCCS"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  geom_text(aes(x=7.7, y=0,  label="close to\nCCS"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  geom_text(aes(x=8.9, y=0,  label="far from\nCCS"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  geom_text(aes(x=11.4, y=0,  label="close to\nCCS"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  geom_text(aes(x=12.6, y=0,  label="far from\nCCS"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  scale_x_continuous(breaks = c(1.5, 4.85, 7.95, 11.65), labels=c("A", "B", "C", "C2"))+
  scale_fill_manual(values = c("darkorange","darkorange", "darkgreen", "darkgreen","dodgerblue", "dodgerblue", "dodgerblue","dodgerblue4","dodgerblue4", "dodgerblue4"), guide=F)+
  scale_color_manual(values = c("darkorange","darkorange", "darkgreen", "darkgreen","dodgerblue", "dodgerblue", "dodgerblue","dodgerblue4","dodgerblue4", "dodgerblue4"), guide=F)+
  labs(y="Inhabitants per CHW", x="",
       caption="Error bars indicate the 5% and 95% quantiles\nof the distribution over all CHWs.")+
  theme_bw()+
  guides(alpha = guide_legend(override.aes = list(colour = "black", size = 0.5)))+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        plot.caption = element_text(size=14), legend.text = element_text(size=14))-> plot_average
plot_average

# compbine both plots
plot2=plot_grid(ggp2+theme(legend.position = "none"),plot_average+theme(legend.position = "none"),
                labels=c("1.","2."),ncol=2,scale=0.9, rel_widths = c(0.6,1),label_size = 20 )
legend <- get_legend(
  # create some space to the left of the legend
  plot_average + theme(legend.box.margin = margin(0, 0, 0, 0), legend.position = "bottom")
)
all_2legend=plot_grid(plot2,legend,  ncol=1, rel_heights = c(1,0.07))
ggsave(all_2legend, file=file.path(dirPlots, "barplot_CHWtotals_main.png"), width=30,height=16, units = "cm")


# supplementary plot on the number of CHW with few inh.
# get totals for C and C2
test_df_small_all=capacities.all %>% filter(scenario %in% c("C", "C2")) %>%
  mutate(is.small=(capacities<100)) %>%
  group_by(scenario) %>%
  summarise(prop_100=sum(is.small))%>%
  mutate(scenario_far=paste0(scenario, "_Total"), close2CCS="Total", categories_merge="Metropolitan")

# get totals per categories
test_df_small=capacities.all %>%
  mutate(categories_merge=factor(ifelse(scenario=="A", as.character(categories),as.character(urbrur)),levels=c("Metropolitan","Urban","Rural")),
         scenario_far=ifelse(scenario %in% c("A"), scenario,paste0(scenario,"\n",close2CCS )),
         is.small=(capacities<100)) %>%
  group_by(scenario,close2CCS,categories_merge,scenario_far, out) %>%
  summarise(prop_100=sum(is.small))

test_df_plot_small=rbind(test_df_small, test_df_small_all)%>%
  left_join(total_per_scen) %>% mutate(percentage=100*prop_100 /totalCHW )
test_df_plot_small$scenario_far=factor(test_df_plot_small$scenario_far, levels=c("A","B\n(far from\nCCS)","C_Total","C\n(close to\nCCS)","C\n(far from\nCCS)","C2_Total","C2\n(close to\nCCS)","C2\n(far from\nCCS)" ))
test_df_plot_small=arrange(test_df_plot_small, scenario_far)
test_df_plot_small$x.seq=c(1,1,1,3,3,5,6,6,7,7,8,9,9,10,10)

test_df_plot_small%>%
  ggplot(aes(x=x.seq, y=percentage, fill=scenario_far, alpha=categories_merge, color=scenario))+
  scale_alpha_manual(name="" ,values=c(1,0.7,0.4))+
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("darkorange", "darkgreen", "white","dodgerblue","dodgerblue", "white", "dodgerblue4", "dodgerblue4"), guide=F)+
  scale_color_manual(values = c("darkorange", "darkgreen", "dodgerblue", "dodgerblue4"), guide=F)+
  labs(y="% of all CHWs in scenario\nwith <100 assigned inh.", x="")+
  geom_text(aes(x=3, y=0,  label="far from\nCCS"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  geom_text(aes(x=5, y=0,  label="Total\n"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  geom_text(aes(x=6, y=0,  label="close to\nCCS"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  geom_text(aes(x=7, y=0,  label="far from\nCCS"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  geom_text(aes(x=8, y=0,  label="Total\n"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  geom_text(aes(x=9, y=0,  label="close to\nCCS"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  geom_text(aes(x=10, y=0,  label="far from\nCCS"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  scale_x_continuous(breaks = c(1, 3, 6, 9), labels=c("A", "B", "C", "C2"))+
  scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 2.5),labels = c("0%", "0.5%", "1%", "1.5%", "2%", "2.5%"), limits = c(-0.1,2.5))+
  theme_bw()+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=14), legend.text = element_text(size=14)) -> plot_small
plot_small



# supplementary plot on the number of CHW reaching the threshold
# get totals for C and C2
test_df_threshold_ungroup= capacities.all %>%
  mutate(categories_merge=factor(ifelse(scenario=="A", as.character(categories),as.character(urbrur)),levels=c("Metropolitan","Urban","Rural")),
         scenario_far=ifelse(scenario %in% c("A"), scenario,paste0(scenario,"\n",close2CCS )),
         is.threshold4000=(capacities>=3995),
         is.threshold2500=(capacities>=2495 & capacities<=2500 ),
         is.threshold1000=(capacities>=995 & capacities<=1000 ),
         threshold4000=((categories_merge =="Metropolitan") | (scenario=="C" & close2CCS == "(close to\nCCS)") | (scenario=="C2" & close2CCS == "(close to\nCCS)" & categories_merge =="Urban")),
         threshold2500=(categories_merge =="Urban" & close2CCS == "(far from\nCCS)"),
         threshold1000=(categories_merge =="Rural" & close2CCS == "(far from\nCCS)") | (scenario=="C2" & close2CCS == "close to CCS" & categories_merge =="Rural"),
         test=as.numeric(threshold2500)+as.numeric(threshold4000)+as.numeric(threshold1000),
         is.threshold=((threshold4000 &is.threshold4000)|(is.threshold2500&threshold2500)|(is.threshold1000&threshold1000)),
         threshold=ifelse(threshold4000, 4000, ifelse(threshold2500, 2500, 1000)))

# get totals per categories
test_df_threshold= test_df_threshold_ungroup%>%
  group_by(scenario,close2CCS,categories_merge,threshold,scenario_far) %>%
  summarise(prop_4000=sum(is.threshold4000),
            prop_2500=sum(is.threshold2500),
            prop_1000=sum(is.threshold1000),
            prop_threshold=sum(is.threshold),
            test1=min(test), test2=max(test))

test_df_threshold_all= test_df_threshold_ungroup%>% filter(scenario %in% c("C", "C2")) %>%
  group_by(scenario) %>%
  summarise(prop_4000=sum(is.threshold4000),
            prop_2500=sum(is.threshold2500),
            prop_1000=sum(is.threshold1000),
            prop_threshold=sum(is.threshold),
            test1=min(test), test2=max(test))%>%
  mutate(scenario_far=paste0(scenario, "_Total"), close2CCS="Total", categories_merge="Metropolitan")

test_df_plot_threshold=rbind(test_df_threshold, test_df_threshold_all)%>%
  left_join(total_per_scen) %>% mutate(percentage=100*prop_threshold /totalCHW )
test_df_plot_threshold$scenario_far=factor(test_df_plot_threshold$scenario_far, levels=c("A","B\n(far from\nCCS)","C_Total","C\n(close to\nCCS)","C\n(far from\nCCS)","C2_Total","C2\n(close to\nCCS)","C2\n(far from\nCCS)" ))
test_df_plot_threshold=arrange(test_df_plot_threshold, scenario_far)

test_df_plot_threshold$x.seq=c(1,1,1,3,3,5,6,6,7,7,8,9,9,10,10)


test_df_plot_threshold%>%
  ggplot(aes(x=x.seq, y=percentage, fill=scenario_far, color=scenario, alpha=categories_merge))+
  scale_alpha_manual(name="" ,values=c(1,0.7,0.4))+
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("darkorange", "darkgreen", "white","dodgerblue","dodgerblue", "white", "dodgerblue4", "dodgerblue4"), guide=F)+
  scale_color_manual(values = c("darkorange", "darkgreen", "dodgerblue", "dodgerblue4"), guide=F)+
  geom_text(aes(x=3, y=0,  label="far from\nCCS"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  geom_text(aes(x=5, y=0,  label="Total\n"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  geom_text(aes(x=6, y=0,  label="close to\nCCS"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  geom_text(aes(x=7, y=0,  label="far from\nCCS"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  geom_text(aes(x=8, y=0,  label="Total\n"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  geom_text(aes(x=9, y=0,  label="close to\nCCS"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  geom_text(aes(x=10, y=0,  label="far from\nCCS"), vjust=1.2, size=3, color="black", show.legend = FALSE)+
  scale_x_continuous(breaks = c(1, 3, 6, 9), labels=c("A", "B", "C", "C2"))+
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         alpha = guide_legend(override.aes = list(pattern = "none"))) +
  labs(y="% of all CHWs in scenario\nreaching the threshold population", x="")+
  theme_bw()+
  scale_y_continuous(breaks = c(0,10, 20, 30, 40,50, 60),labels = c("0%","10%", "20%", "30%", "40%","50%", "60%"), limits = c(-3,NA))+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=14), legend.text = element_text(size=14)) -> plot_threshold
plot_threshold

# combine both plots
plot3=plot_grid(plot_threshold+theme(legend.position = "none"),plot_small+theme(legend.position = "none"),
                labels=c("1.","2."),ncol=2,scale=0.9,label_size = 20 )
legend2 <- get_legend(  plot_threshold + theme(legend.box.margin = margin(0, 0, 0, 0), legend.position = "bottom"))
all_3legend=plot_grid(plot3,legend,  ncol=1, rel_heights = c(1,0.07))
ggsave(all_3legend, file=file.path(dirPlots, "barplot_CHWprop_appendix.png"), width=35,height=16, units = "cm")



########################
# Descriptive stats
# mean capacity per scenario
capacities.all %>%
  group_by(scenario) %>%
  dplyr::summarize( mean = mean(capacities),median = median(capacities), max=max(capacities), min=min(capacities),
                    q1=quantile(capacities, probs=0.25), q3=quantile(capacities, probs=0.75),
                    q025=quantile(capacities, probs=0.025), q975=quantile(capacities, probs=0.975),
                    q5=quantile(capacities, probs=0.05), q95=quantile(capacities, probs=0.95)) %>%
  as.data.frame() %>% mutate(categories="Total")

# mean capacity per scenario per rural/urban/metropolitan
capacities.all %>%
  group_by(scenario, categories) %>%
  dplyr::summarize( mean = round(mean(capacities)),median = median(capacities),
                    max=max(capacities), min=min(capacities),
                    q1=quantile(capacities, probs=0.25), q3=quantile(capacities, probs=0.75),
                    q025=quantile(capacities, probs=0.025), q975=quantile(capacities, probs=0.975),
                    q5=quantile(capacities, probs=0.05), q95=quantile(capacities, probs=0.95)) %>%
  as.data.frame()



mytable50=table(capacities.all$capacities<50, capacities.all$scenario)
mytable100=table(capacities.all$capacities<100, capacities.all$scenario)
prop.table(mytable50,2)
prop.table(mytable100,2)
table(capacities.all$capacities<100, capacities.all$scenario)


#=======================================================
# 3. GAP ANALYSIS
#=======================================================

##############
# SPA DATA

# Manipulate SPA data
# merge with the shapefile for GPS coordinates
data.spa=read.dta(file.path(dirSPA,"/HTFC7ADTSP/HTFC7AFLSP.DTA"))
data.spa$SPAFACID=data.spa$facil
data.spa=merge(data.spa, SPA.shp@data, all=TRUE)


#keep only facilities with CHWs and rename departments for merging
data.spa.withCHW=data.spa[data.spa$q3201b>0 & is.na(data.spa$q3201b)==FALSE,]
data.spa.withCHW$dept=data.spa.withCHW$departf
data.spa.withCHW$dept=gsub("-"," ",data.spa.withCHW$dept)
data.spa.withCHW$dept=gsub("grand","grande",data.spa.withCHW$dept)
data.spa.withCHW$dept=gsub("...aire métropolitaine","aire metropolitaine",data.spa.withCHW$dept)
data.spa.withCHW$dept=gsub("...reste ouest","reste ouest",data.spa.withCHW$dept)


# ASCP/ASC et superviseurs
data.spa.asc.list=read.dta(file.path(dirSPA,"/HTCS7ADTSP/HTCS7AFLSP.DTA"))
data.spa.asc.list=data.spa.asc.list[,c(1,300:398)]
data.spa.asc.list.melt=na.omit(melt(data.spa.asc.list, id.vars = "facil"))
data.spa.ascp=as.data.frame(table(data.spa.asc.list.melt$facil,data.spa.asc.list.melt$value))
data.spa.ascp=cast(data.spa.ascp, Var1~Var2)
names(data.spa.ascp)=c("facil","asc","ascp","superviseur")
data.spa.ascp$chw=data.spa.ascp$asc+data.spa.ascp$ascp  #chw correspond to asc+ascp
data.spa.withCHW=merge(data.spa.ascp,data.spa.withCHW, all.x=TRUE)

# total numbers per department
spa.departements=aggregate((data.spa.withCHW$chw),by= list(dept=data.spa.withCHW$dept), FUN=sum)
names(spa.departements)=c("dept","nbCHW")
#merge with section communale shapefile
dept=unique(seccom.shp@data[,c(2,12)])
dept$dept=tolower(dept$departement)
spa.departements=merge(spa.departements,dept, all=TRUE)
spa.departements$nbCHW_cut=cut(spa.departements$nbCHW, breaks=c(0,100,200,300,400,500,600,10000),include.lowest=TRUE, right=FALSE)
levels(spa.departements$nbCHW_cut)=c("<100","100-200","200-300","300-400","400-500","500-600", ">=600")


# have information on ASC and supervisors per department
ascp.spa.departements=aggregate((data.spa.withCHW$ascp),by= list(dept=data.spa.withCHW$dept), FUN=sum); names(ascp.spa.departements)=c("dept","ascp")
asc.spa.departements=aggregate((data.spa.withCHW$asc),by= list(dept=data.spa.withCHW$dept), FUN=sum); names(asc.spa.departements)=c("dept","asc")
superviseurs.spa.departements=aggregate((data.spa.withCHW$superviseur),by= list(dept=data.spa.withCHW$dept), FUN=sum); names(superviseurs.spa.departements)=c("dept","superviseurs")
CHW.spa.dpt=merge(spa.departements,ascp.spa.departements)
CHW.spa.dpt=merge(CHW.spa.dpt,asc.spa.departements)
CHW.spa.dpt=merge(CHW.spa.dpt,superviseurs.spa.departements)
CHW.spa.dpt$ascp.prop=CHW.spa.dpt$ascp/CHW.spa.dpt$nbCHW
CHW.spa.dpt$asc.prop=CHW.spa.dpt$asc/CHW.spa.dpt$nbCHW

spa.departements$case="SPA"

######################
# CARTOGRAPHIE

# values extracted from the Cartographie report.
# Because the Ouest department was not splitted in the report, I recalculated the numbers based on the various tables in the report.
# in RESTE OUEST : I found 285 ASCPs intégrés et 35 ASC intégrés (+ 2 ASCs are benevoles, so there are 37 ASCs in total, but only 35 intégrés) amounting to 320
# in METROPOLITAN AREA (Carrefour, Cité Soleil, Petion Ville, Delmas, Petion Ville, Tabarre): I found 1004 ASCPs intégrés and 107 ASC intégrés (+ 12 ASCs are benevoles, so there are 119 ASCs in total, but only 107 intégrés) amounting to 1111
cartographie_dpt=data.frame(departement=c("Aire metropolitaine", "Artibonite","Centre","Grande Anse","Nippes", "Nord","Nord Est","Nord Ouest","Reste Ouest", "Sud","Sud Est"),
                            nbCHW=c(1111,689,442,383,99,323,280,211,320,315,164))

#merge with section communale shapefile
dept=unique(seccom.shp@data[,c(2,12)])
cartographie_dpt=merge(cartographie_dpt,dept, all=TRUE)
cartographie_dpt$nbCHW_cut=cut(cartographie_dpt$nbCHW, breaks=c(0,100,200,300,400,500,600,10000),include.lowest=TRUE, right=FALSE)
levels(cartographie_dpt$nbCHW_cut)=c("<100","100-200","200-300","300-400","400-500","500-600", ">=600")

cartographie_dpt$case="Cartography"


#############################
# GAP PER DEPARTMENT

compare=capacities.all %>% group_by(dpt, scenario) %>% summarise(nbCHW=n())

compare$departement=as.character(compare$dpt)
compare$departement[compare$departement=="AireMetro"]="Metr. Area"
compare$departement[compare$departement=="GrandeAnse"]="Grande-Anse"
compare$departement[compare$departement=="NordEst"]="Nord-Est"
compare$departement[compare$departement=="NordOuest"]="Nord-Ouest"
compare$departement[compare$departement=="SudEst"]="Sud-Est"
compare$departement[compare$departement=="Ouest"]="Rest Ouest"

compare_data=spa.departements[c("nbCHW","departement","case")]
compare_data=merge(compare_data,cartographie_dpt, all=TRUE)

compare_data$departement[compare_data$departement=="Aire metropolitaine"]="Metr. Area"
compare_data$departement[compare_data$departement=="Grande Anse"]="Grande-Anse"
compare_data$departement[compare_data$departement=="Nord Est"]="Nord-Est"
compare_data$departement[compare_data$departement=="Nord Ouest"]="Nord-Ouest"
compare_data$departement[compare_data$departement=="Sud Est"]="Sud-Est"
compare_data$departement[compare_data$departement=="Reste Ouest"]="Rest Ouest"


p=ggplot( ) +
  geom_bar(
    data = compare_data
    , aes(y = fct_reorder(departement, nbCHW), x = nbCHW, fill = case )
    , stat = "identity"
    , alpha = 0.5
    , position = position_dodge(0.5) ) +
  geom_point(
    data = compare
    , aes(y = departement, x = nbCHW, pch = scenario, col = scenario ), cex = 5)+
  labs( y = ""
        , x = "Number of agents"
        , fill = "Current placement"
        , pch ="Suggested scenarios"
        , color = "Suggested scenarios"
        , title = "")+
  scale_fill_manual( values = c("grey55","grey21"))+
  scale_color_manual( values = c("darkorange","darkgreen","dodgerblue","darkblue"))+
  scale_shape_manual( values = c(19, 17, 15, 8))+
  theme_minimal()+
  theme(axis.text = element_text(size=16),
        #axis.text.x = element_text(angle = 45),
        axis.title = element_text(size=16),
        legend.text = element_text(size=16),
        legend.title  = element_text(size=16)) + guides(shape = guide_legend(override.aes = list(size = 5)))
p
ggsave(p, file=file.path(dirPlots,"barplot_compare_scenarios.jpg"), width=11, height=8)


#############################
# GAP PER SECTION COMMUNALE

#total numbers per section communale
SPA.shp.seccom <- point.in.poly(SPA.shp, seccom.shp)
# convert to data frame, keeping your data
SPA.shp.seccom<- as.data.frame(SPA.shp.seccom) %>%
  select(NAME_1, NAME_2, ID_4, NAME_4, departement, SPAFACID)

myvars=c("SPAFACID","chw","asc","ascp","superviseur", "q3201b")
SPA.shp.seccom=merge(SPA.shp.seccom,data.spa.withCHW[myvars], all=TRUE)
SPA.shp.seccom=SPA.shp.seccom[SPA.shp.seccom$chw>0 & is.na(SPA.shp.seccom$chw)==FALSE,]
spa.seccom=SPA.shp.seccom %>%
  group_by(ID_4)%>%
  summarise(nbCHWSPA=sum(chw, na.rm = TRUE),asc=sum(asc, na.rm = TRUE),ascp=sum(ascp, na.rm = TRUE),superviseur=sum(superviseur, na.rm = TRUE))%>%
  filter(is.na(ID_4)==F)
spa.seccom.sp=sp::merge(seccom.shp,spa.seccom)

#plot the gap per section communale

plot.GapscenarioA.seccom=PlotGapScenarioSeccom(CHW.positions.scenarioA.CSLCP[,1:2])
plot.GapscenarioB.seccom=PlotGapScenarioSeccom(CHW.positions.scenarioB.CSLCP[,1:2])
plot.GapscenarioC.seccom=PlotGapScenarioSeccom(CHW.positions.scenarioC.CSLCP[,1:2])
plot.GapscenarioC2.seccom=PlotGapScenarioSeccom(CHW.positions.scenarioC2.CSLCP[,1:2])

# combined plot for scenario C
# create the plots for all scenarios
plot.CHW.positions.scenarioC.CSLCP=plot_scenario_all_cslscp_plots(rbind(CHW.positions.scenarioCin.CSLCP,CHW.positions.scenarioCout.CSLCP), "scenarioC")

figure5=plot_grid(plot.CHW.positions.scenarioC.CSLCP$map_seccom,plot.GapscenarioC.seccom$plot, labels=c("1.", "2.") )
ggsave(figure5, file=file.path(dirPlots,"compare_scenario_seccom_C.pdf"), width=20,height=10, units = "cm")
ggsave(figure5, file=file.path(dirPlots,"compare_scenario_seccom_C.png"), width=20,height=10, units = "cm")



plot.CHW.positions.scenarioA.CSLCP=plot_scenario_all_cslscp_plots(rbind(CHW.positions.scenarioA.CSLCP), "scenarioA")
plot.CHW.positions.scenarioB.CSLCP=plot_scenario_all_cslscp_plots(rbind(CHW.positions.scenarioB.CSLCP), "scenarioB")
plot.CHW.positions.scenarioC2.CSLCP=plot_scenario_all_cslscp_plots(rbind(CHW.positions.scenarioCin2.CSLCP,CHW.positions.scenarioCout.CSLCP), "scenarioC2")

figure5_A=plot_grid(plot.CHW.positions.scenarioA.CSLCP$map_seccom,plot.GapscenarioA.seccom$plot, labels=c("1.", "2.") )
figure5_B=plot_grid(plot.CHW.positions.scenarioB.CSLCP$map_seccom,plot.GapscenarioB.seccom$plot, labels=c("1.", "2.") )
figure5_C2=plot_grid(plot.CHW.positions.scenarioC2.CSLCP$map_seccom,plot.GapscenarioC2.seccom$plot, labels=c("1.", "2.") )
ggsave(figure5_A, file=file.path(dirPlots,"compare_scenario_seccom_A.pdf"), width=20,height=10, units = "cm")
ggsave(figure5_A, file=file.path(dirPlots,"compare_scenario_seccom_A.png"), width=20,height=10, units = "cm")
ggsave(figure5_B, file=file.path(dirPlots,"compare_scenario_seccom_B.pdf"), width=20,height=10, units = "cm")
ggsave(figure5_B, file=file.path(dirPlots,"compare_scenario_seccom_B.png"), width=20,height=10, units = "cm")
ggsave(figure5_C2, file=file.path(dirPlots,"compare_scenario_seccom_C2.pdf"), width=20,height=10, units = "cm")
ggsave(figure5_C2, file=file.path(dirPlots,"compare_scenario_seccom_C2.png"), width=20,height=10, units = "cm")



#=======================================================
# 3. ROBUSTNESS CHECK ON SCENARIO C
#=======================================================

################################################
# Charge the scenario files
###############################################

# change radius
Cin_rad54=data.frame()
Cout_rad54=data.frame()
Cin_rad66=data.frame()
Cout_rad66=data.frame()
for (i in names(dpt.list2)){
  print(i)
  this.data=read.csv(file.path(dirOutputs,paste0("SA/clscp_54_buffer60_capa40004000300in1urbs2000/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  Cin_rad54=rbind(Cin_rad54,this.data)

  this.data=read.csv(file.path(dirOutputs,paste0("SA/clscp_54_buffer60_capa25001000300in0urbs2000/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  Cout_rad54=rbind(Cout_rad54,this.data)

  this.data=read.csv(file.path(dirOutputs,paste0("SA/clscp_66_buffer60_capa40004000300in1urbs2000/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  Cin_rad66=rbind(Cin_rad66,this.data)

  this.data=read.csv(file.path(dirOutputs,paste0("SA/clscp_66_buffer60_capa25001000300in0urbs2000/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  Cout_rad66=rbind(Cout_rad66,this.data)
}

# change buffer to CCS
Cin_buff54=data.frame()
Cout_buff54=data.frame()
Cin_buff66=data.frame()
Cout_buff66=data.frame()
for (i in names(dpt.list2)){
  print(i)
  this.data=read.csv(file.path(dirOutputs,paste0("SA/clscp_60_buffer54_capa40004000300in1urbs2000/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  Cin_buff54=rbind(Cin_buff54,this.data)

  this.data=read.csv(file.path(dirOutputs,paste0("SA/clscp_60_buffer54_capa25001000300in0urbs2000/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  Cout_buff54=rbind(Cout_buff54,this.data)

  this.data=read.csv(file.path(dirOutputs,paste0("SA/clscp_60_buffer66_capa40004000300in1urbs2000/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  Cin_buff66=rbind(Cin_buff66,this.data)

  this.data=read.csv(file.path(dirOutputs,paste0("SA/clscp_60_buffer66_capa25001000300in0urbs2000/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  Cout_buff66=rbind(Cout_buff66,this.data)
}

# change capacity
Cin_capaP=data.frame()
Cout_capaP=data.frame()
Cin_capaM=data.frame()
Cout_capaM=data.frame()
for (i in names(dpt.list2)){
  print(i)
  this.data=read.csv(file.path(dirOutputs,paste0("SA/clscp_60_buffer60_capa44004400300in1urbs2000/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  Cin_capaP=rbind(Cin_capaP,this.data)

  this.data=read.csv(file.path(dirOutputs,paste0("SA/clscp_60_buffer60_capa27501100300in0urbs2000/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  Cout_capaP=rbind(Cout_capaP,this.data)

  this.data=read.csv(file.path(dirOutputs,paste0("SA/clscp_60_buffer60_capa36003600300in1urbs2000/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  Cin_capaM=rbind(Cin_capaM,this.data)

  this.data=read.csv(file.path(dirOutputs,paste0("SA/clscp_60_buffer60_capa2250900300in0urbs2000/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  Cout_capaM=rbind(Cout_capaM,this.data)
}

# change urban definition
Cin_clump270=data.frame()
Cout_clump270=data.frame()
Cin_clump330=data.frame()
Cout_clump330=data.frame()
for (i in names(dpt.list2)){
  this.data=read.csv(file.path(dirOutputs,paste0("SA/clscp_60_buffer60_capa40004000270in1urbs2000/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  Cin_clump270=rbind(Cin_clump270,this.data)

  this.data=read.csv(file.path(dirOutputs,paste0("SA/clscp_60_buffer60_capa25001000270in0urbs2000/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  Cout_clump270=rbind(Cout_clump270,this.data)

  this.data=read.csv(file.path(dirOutputs,paste0("SA/clscp_60_buffer60_capa40004000330in1urbs2000/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  Cin_clump330=rbind(Cin_clump330,this.data)

  this.data=read.csv(file.path(dirOutputs,paste0("SA/clscp_60_buffer60_capa25001000330in0urbs2000/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  Cout_clump330=rbind(Cout_clump330,this.data)
}

# change urban definition
Cin_urb2200=data.frame()
Cout_urb2200=data.frame()
Cin_urb1800=data.frame()
Cout_urb1800=data.frame()
for (i in names(dpt.list2)){
  this.data=read.csv(file.path(dirOutputs,paste0("SA/clscp_60_buffer60_capa40004000300in1urbs2200/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  Cin_urb2200=rbind(Cin_urb2200,this.data)

  this.data=read.csv(file.path(dirOutputs,paste0("SA/clscp_60_buffer60_capa25001000300in0urbs2200/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  Cout_urb2200=rbind(Cout_urb2200,this.data)

  this.data=read.csv(file.path(dirOutputs,paste0("SA/clscp_60_buffer60_capa40004000300in1urbs1800/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  Cin_urb1800=rbind(Cin_urb1800,this.data)

  this.data=read.csv(file.path(dirOutputs,paste0("SA/clscp_60_buffer60_capa25001000300in0urbs1800/positions_popcov_",i,".csv")))
  this.data$metro=ifelse(i=="AireMetro" , 1, 0)
  this.data$dpt=switch_dpt(i)
  Cout_urb1800=rbind(Cout_urb1800,this.data)
}


scenarioC=plot_scenario_all_cslscp(rbind(CHW.positions.scenarioCin.CSLCP,CHW.positions.scenarioCout.CSLCP) %>%
                                     mutate(capacity=my.list.capa.here), "scenarioC_baseline") %>% mutate(updown="baseline")
scenarioCrad54=plot_scenario_all_cslscp_new_names(rbind(Cin_rad54 %>% mutate(out=0),Cout_rad54 %>% mutate(out=1)), "scenarioC_rad54")%>% mutate(updown="-10%", sensi1="rad")
scenarioCrad66=plot_scenario_all_cslscp_new_names(rbind(Cin_rad66 %>% mutate(out=0),Cout_rad66 %>% mutate(out=1)), "scenarioC_rad66") %>% mutate(updown="+10%", sensi1="rad")
scenarioCcapaM=plot_scenario_all_cslscp_new_names(rbind(Cin_capaM %>% mutate(out=0),Cout_capaM %>% mutate(out=1)), "scenarioC_capaM")%>% mutate(updown="-10%", sensi1="cap")
scenarioCcapaP=plot_scenario_all_cslscp_new_names(rbind(Cin_capaP %>% mutate(out=0),Cout_capaP %>% mutate(out=1)), "scenarioC_capaP")%>% mutate(updown="+10%", sensi1="cap")
scenarioCbuff54=plot_scenario_all_cslscp_new_names(rbind(Cin_buff54 %>% mutate(out=0),Cout_buff54 %>% mutate(out=1)), "scenarioC_buff55")%>% mutate(updown="-10%", sensi1="buf")
scenarioCbuff66=plot_scenario_all_cslscp_new_names(rbind(Cin_buff66 %>% mutate(out=0),Cout_buff66 %>% mutate(out=1)), "scenarioC_buff65")%>% mutate(updown="+10%", sensi1="buf")
scenarioCclump270=plot_scenario_all_cslscp_new_names(rbind(Cin_clump270 %>% mutate(out=0),Cout_clump270 %>% mutate(out=1)), "scenarioC_clump200")%>% mutate(updown="-10%", sensi1="clu")
scenarioCclump330=plot_scenario_all_cslscp_new_names(rbind(Cin_clump330 %>% mutate(out=0),Cout_clump330 %>% mutate(out=1)), "scenarioC_clump400")%>% mutate(updown="+10%", sensi1="clu")
scenarioCurb1800=plot_scenario_all_cslscp_new_names(rbind(Cin_urb1800 %>% mutate(out=0),Cout_urb1800 %>% mutate(out=1)), "scenarioC_urb1950")%>% mutate(updown="-10%", sensi1="urb")
scenarioCurb2200=plot_scenario_all_cslscp_new_names(rbind(Cin_urb2200 %>% mutate(out=0),Cout_urb2200 %>% mutate(out=1)), "scenarioC_urb2050")%>% mutate(updown="+10%", sensi1="urb")


################################################
# Combine the files to create national level summary plots
###############################################

capacities.all.sensi=rbind(scenarioC %>% mutate(sensi1="rad", my.list.capa.here=NULL),
                           scenarioC %>% mutate(sensi1="buf", my.list.capa.here=NULL),
                           scenarioC %>% mutate(sensi1="cap", my.list.capa.here=NULL),
                           scenarioC %>% mutate(sensi1="clu", my.list.capa.here=NULL),
                           scenarioC %>% mutate(sensi1="urb", my.list.capa.here=NULL),
                           scenarioCrad54 ,scenarioCrad66,
                           scenarioCbuff54 ,scenarioCbuff66 ,
                           scenarioCcapaM ,scenarioCcapaP ,
                           scenarioCclump270 ,scenarioCclump330,
                           scenarioCurb1800 ,scenarioCurb2200 ) %>%
  mutate(categories=factor(ifelse(metro==1, "Metropolitan", ifelse(is.rural==1, "Rural", "Urban"))),
         urbrur=factor(ifelse(is.rural==0, "Urban", "Rural")),
         close2CCS=factor(ifelse(out==0, "Close to\nCCS", "Far from\nCCS")),
         updown=factor(updown, levels=c("-10%", "baseline", "+10%")),
         sensi=factor(sensi1, levels=c("bas","cap", "rad","buf","clu","urb"),
                      labels=c("Baseline","Max. population","Max. walking time","Distance to CCS","Min. density for\nurban areas", "Min. population\nfor urban areas") ),
         case=paste0(sensi1, "_",updown )

  )


###################
#total per scenarios
total_per_scen_sensi=data.frame(table(capacities.all.sensi$case))
names(total_per_scen_sensi)=c("case", "totalCHW")
print(total_per_scen_sensi)

total_per_category_sensi=capacities.all.sensi %>%
  group_by(case, sensi,sensi1, updown,categories)%>%
  summarise(count=n())%>%
  left_join(total_per_scen_sensi, by="case") %>%
  mutate(prop=round(100*count/totalCHW),
         totalCHW2=ifelse(categories=="Metropolitan",totalCHW, NA))
print(total_per_category_sensi)

total_per_category_sensi %>%
  ggplot(aes(x = updown, y = count, alpha= categories)) +  # Create stacked bar chart
  geom_bar(stat = "identity", fill="dodgerblue")+
  scale_alpha_manual(name="" ,values=c(1,0.7,0.4))+
  labs(x="", y="Number of CHWs")+theme_bw()+
  geom_text(aes(label = totalCHW2 , y=totalCHW2), vjust=-0.25, size=6, show.legend = FALSE)+
  facet_wrap(.~sensi, scales="free_x", ncol=3)+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20, face = "bold"),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.position = "bottom",
        strip.text = element_text(size = 20, face = "bold"),
        strip.background = element_rect(fill="white") ) -> ggp2_sensi
ggp2_sensi

base.value=unique(total_per_category_sensi$totalCHW[total_per_category_sensi$updown=="baseline"])

width <- 0.95
tornado_plot=total_per_category_sensi %>%
  filter(updown != "baseline", categories=="Metropolitan") %>%
  # create the columns for geom_rect
  mutate(Parameter=factor(sensi, levels=c( "Min. population\nfor urban areas","Min. density for\nurban areas","Distance to CCS", "Max. walking time", "Max. population")) ,
         ymin=pmin(100*(totalCHW-base.value)/base.value,0),
         ymax=pmax(100*(totalCHW-base.value)/base.value,0),
         xmin=as.numeric(Parameter)-width/2,
         xmax=as.numeric(Parameter)+width/2) %>%
  ggplot() +
  geom_rect(aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=updown)) +
  theme_bw() +
  theme(axis.title.y=element_blank(), legend.position = 'bottom',
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20, face = "bold"),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks=c(1,2,3,4,5),
                     labels = c( "Min. population\nfor urban areas","Min. density for\nurban areas","Distance to CCS", "Max. walking time", "Max. population")) +
  scale_fill_manual(values=c("lightblue","darkblue"))+labs(y="% change in the number of CHWs", fill= "Parameter value")+
  coord_flip()+ theme(plot.margin = unit(c(0,0,20,0), "lines"))

sensi_tornado=plot_grid(ggp2_sensi, tornado_plot, rel_widths = c(1, 0.6), rel_heights = c(1, 0.6), scale = 0.9, labels = c("1.", "2."), label_size = 25)
ggsave(sensi_tornado, file=file.path(dirPlots,"barplot_tornado.jpg"), width=19, height=15)

######################################################
# mean capacity per scenario per rural/urban/metropolitan
capacities.all.sensi %>% #filter(out==1)%>%
  mutate(scenario_far=interaction(updown,close2CCS)) %>%
  group_by(scenario,close2CCS,case, updown, sensi,scenario_far,urbrur) %>%
  summarise(mean=mean(capacities), q75=quantile(capacities, 0.9), q25=quantile(capacities, 0.05) )%>%
  ggplot(aes(x=close2CCS, y=mean, fill=updown, color=updown, ymin=q25, ymax=q75, group=interaction(updown,urbrur), alpha=(urbrur)))+
  scale_alpha_manual(name="" ,values=c(0.4,0.7))+
  geom_bar(stat="identity", position ="dodge" ,color=NA)+
  geom_errorbar(position = position_dodge(.9), alpha=1, width=0.2)+
  ylim(0,4500)+theme_bw()+
  facet_wrap(.~sensi, scales="free_x", ncol=3)+
  scale_fill_manual(values = c("lightblue", "dodgerblue","darkblue"), name="")+
  scale_color_manual(values = c("lightblue3", "dodgerblue","darkblue"), guide=F)+
  labs(y="Inhabitants per CHW", x="", caption="Error bars indicate the 5% and 95% quantiles of the distribution over all CHWs.")+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20, face = "bold"),
        axis.title = element_text(size=20),
        plot.caption = element_text(size=20),
        legend.text = element_text(size=20),
        legend.position = "bottom",
        strip.text = element_text(size = 18, face = "bold"),
        strip.background = element_rect(fill="white") ) -> plot_average_sensi
plot_average_sensi
ggsave(plot_average_sensi, file=file.path(dirPlots,"barplot_nb_inhab_per_CHW_sensiC_10pct.jpg"), width=19, height=15)


################################################
# Compare with SPA and Cartographie per department
###############################################
compare_sensi=capacities.all.sensi %>% group_by(dpt, updown, sensi) %>% summarise(nbCHW=n())

compare_sensi$departement=as.character(compare_sensi$dpt)
compare_sensi$departement[compare_sensi$departement=="AireMetro"]="Metr. Area"
compare_sensi$departement[compare_sensi$departement=="GrandeAnse"]="Grande-Anse"
compare_sensi$departement[compare_sensi$departement=="NordEst"]="Nord-Est"
compare_sensi$departement[compare_sensi$departement=="NordOuest"]="Nord-Ouest"
compare_sensi$departement[compare_sensi$departement=="SudEst"]="Sud-Est"
compare_sensi$departement[compare_sensi$departement=="Ouest"]="Rest Ouest"

p=ggplot(data=compare_sensi, aes(x=departement, y=nbCHW, color=updown)) +
  facet_wrap(.~sensi, ncol=3)+
  geom_bar(
    data = compare_data
    , aes(x = fct_reorder(departement, nbCHW), y = nbCHW, fill = case )
    , stat = "identity"
    , alpha = 0.5, color=NA
    , position = position_dodge(0.5) ) +
  geom_point(stat="identity",  shape=15, size=4)+
  xlab("")+theme_bw()+
  ylab("Number of CHWs")+coord_flip()+
  scale_color_manual(values = c("lightblue", "dodgerblue","darkblue"), name="Scenario C:")+
  scale_fill_manual( values = c("grey55","grey21"), name="Current placement:")+
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16, face = "bold"),
        #axis.text.x = element_text(angle = 45),
        axis.title = element_text(size=16),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom",
        strip.text = element_text(size = 16, face = "bold"),
        strip.background = element_rect(fill="white") )
p
ggsave(p, file=file.path(dirPlots,"barplot_sensitivity_compare_C_10pct.jpg"), width=16, height=14)



