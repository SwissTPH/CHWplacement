test_that("test mps file creation", {

  newproj= "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
  pop.dummy=raster::raster( matrix(rep(100,225), ncol=15))
  fric.dummy=raster::raster(matrix(rep(0.02,225), ncol=15))
  acc.dummy=raster::raster(matrix(c(rep(30,125), rep(90,100)), ncol=15))
  urb.dummy=raster::raster(matrix(c(rep(30,125), rep(90,100)), ncol=15, byrow = TRUE))
  shp.dummy <- as(raster::extent(pop.dummy), 'SpatialPolygons')
  raster::crs(pop.dummy)=newproj
  raster::crs(fric.dummy)=newproj
  raster::crs(urb.dummy)=newproj
  raster::crs(acc.dummy)=newproj
  names(acc.dummy)=""
  names(urb.dummy)=""


  CreateCHWplacement(population.raster=pop.dummy, friction.raster=fric.dummy,shp=shp.dummy,
                              name="temp", buffer=60, radius=60, capacity.name="",
                              access.raster=acc.dummy,popurb.raster=urb.dummy,
                              max.treat.per.CHW.urban=2500, max.treat.per.CHW.rural=1000,
                              max.CHW.per.pixel=1, is.inside = FALSE, filepath=".")

  CreateCHWplacement(population.raster=pop.dummy, friction.raster=fric.dummy,shp=shp.dummy,
                     name="temp", buffer=60, radius=600, capacity.name="",
                     access.raster=acc.dummy,popurb.raster=urb.dummy,
                     max.treat.per.CHW.urban=200, max.treat.per.CHW.rural=100,
                     max.CHW.per.pixel=1, is.inside = FALSE, filepath=".")


  file_new=readLines(file.path("./",paste0("clscp_",60,"_buffer",60,"_capa","","/","temp",".mps")))
  file_new2=readLines(file.path("./",paste0("clscp_",600,"_buffer",60,"_capa","","/","temp",".mps")))

  expect_equal(digest::sha1(file_new, algo="sha256"), "3d57a8f1079a31d0f51db8fae1b587a0325182b890144e895ff3c76cd7b0a699", label = "test mps 1")
  expect_equal(digest::sha1(file_new2, algo="sha256"), "5ac9a61cda49fb99aff42eb928a1a9460dda5c0caa1c9980ab92c148978f11b4", label = "test mps 2")

})

