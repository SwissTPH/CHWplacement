#################################
# plotting function from Emilie Pothin
PlotMap=function(img,NAMEfile="",shpFile,colormapDT,Title.legend="", myalpha=1, width_polygon=0.2){
  
  shapefile_df <- fortify(shpFile)
  
  r=as.data.frame(rasterToPoints(img))
  names(r)[3]="z"
  r$name=NAMEfile
  r$z=cut(r$z,unique(c(0,as.numeric(as.character(colormapDT$value)))))
  
  gg<-ggplot(r)+geom_tile(aes(x=x,y=y,fill=factor(as.numeric(z),levels=1:nrow(colormapDT))), alpha=myalpha)+
    geom_path(data = shapefile_df, 
              aes(x = long, y = lat, group = group),
              color = 'black',size=width_polygon)+
    theme_void()+
    coord_fixed()+
    theme(panel.border = element_blank())+
    scale_fill_manual(Title.legend,breaks=1:nrow(colormapDT),labels=as.character(colormapDT$label), values = as.character(colormapDT$color),drop = FALSE)
  
  return(gg)
}

#################################
# formating scenario files
plot_scenario_all_cslscp=function(CHW.positions, name_scenario){
  
  my.CHW.positions=CHW.positions
  my.CHW.positions$count=1
  duplicatedCHW=aggregate((my.CHW.positions$count),
                          by= list(posx=my.CHW.positions$x,posy=my.CHW.positions$y), FUN=(sum))
  names(duplicatedCHW)=c("x","y","count")
  my.CHW.positions$count=NULL
  my.CHW.positions=merge(my.CHW.positions,duplicatedCHW, all.x=TRUE )
  my.CHW.positions$capacities=my.CHW.positions$my.list.capa.here/my.CHW.positions$count
  my.CHW.positions$scenario=name_scenario
  return(my.CHW.positions)
}

plot_scenario_all_cslscp_new_names=function(CHW.positions, name_scenario){
  
  my.CHW.positions=CHW.positions
  my.CHW.positions$count=1
  duplicatedCHW=aggregate((my.CHW.positions$count),
                          by= list(posx=my.CHW.positions$x,posy=my.CHW.positions$y), FUN=(sum))
  names(duplicatedCHW)=c("x","y","count")
  my.CHW.positions$count=NULL
  my.CHW.positions=merge(my.CHW.positions,duplicatedCHW, all.x=TRUE )
  my.CHW.positions$capacities=my.CHW.positions$capacity/my.CHW.positions$count
  my.CHW.positions$scenario=name_scenario
  return(my.CHW.positions)
}

#################################
# Plot summary file per department
plot_dpt_summary=function(mydpt){
  
  list_dpt3=seccom.shp@data %>% select(NAME_1, ID_1, departement) %>% 
    unique %>%
    mutate(ADM1NAME =gsub("grande", "grand",tolower(gsub(" ", "-", NAME_1))),
           dpt =gsub(" ", "", NAME_1),
           departement=gsub("Reste", "",gsub(" ", "", 
                                             ifelse(departement=="Aire metropolitaine", "AireMetro", departement))))
  
  mydpt_id1=unique(list_dpt3$ID_1[list_dpt3$dpt == mydpt])
  mydpt_ADM1NAME=unique(list_dpt3$ADM1NAME[list_dpt3$dpt %in% mydpt])
  mydepart=unique(list_dpt3$departement[list_dpt3$dpt %in% mydpt])
  
  my.shp.dpt=subset(seccom.shp, ID_1 == mydpt_id1)
  population.dpt=mask(crop(population.haiti, my.shp.dpt), my.shp.dpt)
  friction.dpt=mask(crop(friction, my.shp.dpt), my.shp.dpt)
  urb.dpt=mask(crop(population.urb.hai.dummy, my.shp.dpt), my.shp.dpt)
  access.dpt=mask(crop(access.raster.spa.CCS, my.shp.dpt), my.shp.dpt)
  
  CHW.positions.scenario.dpt.scenarioA=CHW.positions.scenarioA.CSLCP %>% filter(dpt %in% mydepart)
  CHW.positions.scenario.dpt.scenarioB=CHW.positions.scenarioB.CSLCP %>% filter(dpt %in% mydepart)
  CHW.positions.scenario.dpt.scenarioC=CHW.positions.scenarioC.CSLCP %>% filter(dpt %in% mydepart)
  CHW.positions.scenario.dpt.scenarioC2=CHW.positions.scenarioC2.CSLCP %>% filter(dpt %in% mydepart)
  
  CCS.shp.dpt=subset(CCS.shp,ADM1NAME == mydpt_ADM1NAME & SOURCE !="MIS")
  CCS.shp.dpt=spTransform(CCS.shp.dpt,newproj)
  CCS.dpt=data.frame(CCS.shp.dpt@coords)
  
  map.population.dpt<-PlotMap(population.dpt,Title.legend="Population \ndensity",shpFile=my.shp.dpt,colormapDT=colormapDT0)+theme(legend.key.size = unit(0.16, "in"))
  #map.population.dpt
  map.friction.dpt<-PlotMap(friction.dpt*1000,Title.legend="Minutes to \ncross 1km ",shpFile=my.shp.dpt,colormapDT=colormapDTb2)+theme(legend.key.size = unit(0.16, "in"))
  map.friction.dpt
  
  map.population.dpt_frame=plot_grid(map.population.dpt,labels = c("2.")) +theme(plot.background = element_rect(color = "black"))
  #plot_pop_fric
  map.friction.dpt_frame=plot_grid(map.friction.dpt,labels = c("3.")) +theme(plot.background = element_rect(color = "black"))
  plot_pop_fric=plot_grid(map.population.dpt_frame,
                          map.friction.dpt_frame,labels = c("2.", "3."), ncol=2)
  
  
  travel.time.dpt<-PlotMap(access.dpt,Title.legend="Walking time\nto CCS\n(minutes)\n ",shpFile=my.shp.dpt,colormapDT=colormapDTb, myalpha = 0.7)+
    geom_point(data=CCS.dpt,aes(x=coords.x1,y=coords.x2, shape="CCS"),col="red")+scale_shape_manual(values=17, name="")
  
  
  map.A=travel.time.dpt+
    geom_point(data=CHW.positions.scenario.dpt.scenarioA,aes(x=x,y=y, group=seq_along(rank), size=count),shape=20,col="dodgerblue")+
    labs(size="Number\nof CHWs")
  
  map.B=travel.time.dpt+
    geom_point(data=CHW.positions.scenario.dpt.scenarioB,aes(x=x,y=y, group=seq_along(rank), size=count),shape=20,col="dodgerblue")+
    labs(size="Number\nof CHWs")
  
  map.C=travel.time.dpt+
    geom_point(data=CHW.positions.scenario.dpt.scenarioC,aes(x=x,y=y, group=seq_along(rank), size=count),shape=20,col="dodgerblue")+
    labs(size="Number\nof CHWs")
  
  map.C2=travel.time.dpt+
    geom_point(data=CHW.positions.scenario.dpt.scenarioC2,aes(x=x,y=y, group=seq_along(rank), size=count),shape=20,col="dodgerblue")+
    labs(size="Number\nof CHWs")
  
  legend <- get_legend(map.A + theme(legend.box.margin = margin(0, 0, 0, 0), legend.position = "bottom")+
                         guides(size=guide_legend(order=2), fill=guide_legend(order=1)))
  title <- ggdraw() +  draw_label( " 1.", fontface = 'bold', x = 0, hjust = 0 ) + theme(plot.margin = margin(0, 0, 0, 0))
  
  plot_scenarios=plot_grid(map.A+theme(legend.position = "none"), 
                           map.B+theme(legend.position = "none"), 
                           map.C+theme(legend.position = "none"), 
                           map.C2+theme(legend.position = "none") , 
                           ncol=2, labels = c("A", "B", "C", "C2"))
  
  plot_scenarios_title=plot_grid(title, plot_scenarios, ncol=1,rel_heights = c(0.05,1))
  plot_scenarios_legend=plot_grid(plot_scenarios_title, legend, rel_heights = c(1, .1), ncol=1) +
    theme(plot.background = element_rect(color = "black"))
  #plot_scenarios_legend
  
  all.dpt=plot_grid(plot_scenarios_legend,plot_pop_fric  , ncol=1, scale=1,rel_heights = c(2.7,1))
  #all.dpt
  return(all.dpt)
}



#################################
# Plot gap per section communale
PlotGapScenarioSeccom=function(CHW.positions){
  scenario.sp=SpatialPointsDataFrame(CHW.positions,
                                     CHW.positions,    #the R object to convert
                                     proj4string = crs(newproj))
  
  scenario.sp <- over(scenario.sp, seccom.shp)
  CHW.per.seccom=data.frame(table(scenario.sp$ID_4)) ;names(CHW.per.seccom)=c("ID_4","nbCHW")
  
  CHW.per.seccom=sp::merge(seccom.shp,CHW.per.seccom)
  CHW.per.seccom=merge(CHW.per.seccom,spa.seccom,all=TRUE)
  CHW.per.seccom@data$nbCHW[is.na(CHW.per.seccom@data$nbCHW)]=0
  CHW.per.seccom@data$nbCHWSPA[is.na(CHW.per.seccom@data$nbCHWSPA)]=0
  CHW.per.seccom@data$diffSPA=CHW.per.seccom@data$nbCHWSPA-CHW.per.seccom@data$nbCHW
  CHW.per.seccom.df <- tidy(CHW.per.seccom)
  CHW.per.seccom$polyID <- sapply(slot(CHW.per.seccom, "polygons"), function(x) slot(x, "ID"))
  CHW.per.seccom.df <- merge(CHW.per.seccom.df, CHW.per.seccom, by.x = "id", by.y="polyID")
  CHW.per.seccom.df$diffSPA.cut=cut(CHW.per.seccom.df$diffSPA, breaks=c(-350,-50,-10,0,10,50,300),
                                    label=c("< -50","-50 to -10","-10 to 0","0 to 10","10 to 50", ">50"),include.lowest=TRUE, right=FALSE)
  plot.seccom=ggplot() +                                               # initialize ggplot object
    geom_polygon(                                          # make a polygon
      data = CHW.per.seccom.df,                                    # data frame
      aes(x = long, y = lat, group = group,                # coordinates, and group them by polygons
          fill = diffSPA.cut)) +    # variable to use for filling
    #scale_fill_gradient2( name="difference",low="red", high="lightblue",mid="white") +
    scale_fill_brewer( name="Difference\nin nb. of CHWs", type="seq", palette="RdBu", drop=FALSE)+
    geom_path(data = seccom.shp.union,
              aes(x = long, y = lat, group = group),
              color = 'black',size=0.2)+
    theme(line = element_blank(),                          # remove axis lines ..
          axis.text=element_blank(),                       # .. tickmarks..
          axis.title=element_blank(),                      # .. axis labels..
          panel.background = element_blank(),
          legend.key.size = unit(0.5, "cm"),
          legend.position = c(0.2, 0.7),
          legend.text=element_text(size=8),legend.title=element_text(size=10))+            # .. background gridlines
    coord_equal()
  return(list("plot"=plot.seccom, "file"=CHW.per.seccom@data))
}



#################################
# Plot total per section communale
PlotScenarioSeccom=function(CHW.positions){
  scenario.sp=SpatialPointsDataFrame(CHW.positions[c("x","y")],
                                     CHW.positions,    #the R object to convert
                                     proj4string = crs(newproj))
  scenario.sp <- over(scenario.sp, seccom.shp)
  CHW.per.seccom=data.frame(table(scenario.sp$ID_4)) ;names(CHW.per.seccom)=c("ID_4","nbCHW")
  CHW.per.seccom=sp::merge(seccom.shp,CHW.per.seccom)
  CHW.per.seccom.df <- tidy(CHW.per.seccom)
  CHW.per.seccom$polyID <- sapply(slot(CHW.per.seccom, "polygons"), function(x) slot(x, "ID"))
  CHW.per.seccom.df <- merge(CHW.per.seccom.df, CHW.per.seccom, by.x = "id", by.y="polyID")
  CHW.per.seccom.df$nbCHW[is.na(CHW.per.seccom.df$nbCHW)]=0
  CHW.per.seccom.df$nbCHW.cut=cut(CHW.per.seccom.df$nbCHW, breaks=c(0,10,20,30,40,50,500),
                                  label=c("<10","10 to 20","20 to 30","30 to 40","40 to 50", ">50"), include.lowest=TRUE, right=TRUE)
  print(max(CHW.per.seccom.df$nbCHW))
  plot.seccom=ggplot() +                                               # initialize ggplot object
    geom_polygon(                                          # make a polygon
      data = CHW.per.seccom.df,                                    # data frame
      aes(x = long, y = lat, group = group,                # coordinates, and group them by polygons
          fill = nbCHW.cut)) +    # variable to use for filling
    scale_fill_brewer( name="Nb. of CHWs", type="seq", palette="Reds", drop=FALSE)+
    geom_path(data = seccom.shp.union,
              aes(x = long, y = lat, group = group),
              color = 'black',size=0.2)+
    theme(line = element_blank(),                          # remove axis lines ..
          axis.text=element_blank(),                       # .. tickmarks..
          axis.title=element_blank(),                      # .. axis labels..
          panel.background = element_blank()) +            # .. background gridlines
    coord_equal()  +
    theme(legend.position = c(0.2, 0.7),legend.text=element_text(size=8))
  
  return(plot.seccom)
}




#################################
# wrapper around plot per section communale
plot_scenario_all_cslscp_plots=function(CHW.positions, name_scenario){
  
  my.CHW.positions=CHW.positions
  my.CHW.positions$count=1
  duplicatedCHW=aggregate((my.CHW.positions$count),
                          by= list(posx=my.CHW.positions$x,posy=my.CHW.positions$y), FUN=(sum))
  names(duplicatedCHW)=c("x","y","count")
  my.CHW.positions$count=NULL
  my.CHW.positions=merge(my.CHW.positions,duplicatedCHW, all.x=TRUE )
  my.CHW.positions$capacities=my.CHW.positions$my.list.capa.here/my.CHW.positions$count
  
  plot.scenario.seccom=PlotScenarioSeccom(my.CHW.positions)
  
  return(list("map_seccom"=plot.scenario.seccom))
}
