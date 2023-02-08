test_that("sanity of raster importation", {

  newproj= "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
  pop.dummy=raster::raster( matrix(rep(100,900), ncol=30))
  fric.dummy=raster::raster(matrix(rep(0.02,900), ncol=30))
  acc.dummy=raster::raster(matrix(c(rep(30,450), rep(90,450)), ncol=30))
  urb.dummy=raster::raster(matrix(c(rep(30,450), rep(90,450)), ncol=30, byrow = TRUE))
  shp.dummy <- as(raster::extent(pop.dummy), 'SpatialPolygons')
  raster::crs(pop.dummy)=newproj
  raster::crs(fric.dummy)=newproj
  raster::crs(urb.dummy)=newproj
  raster::crs(acc.dummy)=newproj
  names(acc.dummy)=""
  names(urb.dummy)=""

  all.raster.dummy=PrepareRasterFiles(population.raster=pop.dummy,
                                      friction.raster=fric.dummy,
                                      shp=shp.dummy,
                                      buffer=60, access.raster=acc.dummy,
                                      popurb.raster=urb.dummy,
                                      is.inside=F)

  pop.expected=raster::raster(matrix(c(rep(NA,450), rep(100,450)), ncol=30))
  raster::crs(pop.expected)=newproj
  names(pop.expected)=""

  # add srs field in raster
  pop.expected@srs=all.raster.dummy$pop.map@srs
  acc.dummy@srs=all.raster.dummy$pop.map@srs
  urb.dummy@srs=all.raster.dummy$pop.map@srs

  expect_equal(all.raster.dummy$pop.map , pop.expected, label = "pop")
  expect_equal(all.raster.dummy$access.map , acc.dummy, label = "access")
  expect_equal(all.raster.dummy$urb.map , urb.dummy, label = "urb")

})




test_that("testing GetFootprintWalkingTime", {

  newproj= "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
  pop.dummy=raster::raster( matrix(rep(100,900), ncol=30))
  fric.dummy=raster::raster(matrix(rep(0.02,900), ncol=30))
  fric.dummy_b=raster::raster(matrix(rep(0.01,900), ncol=30))
  acc.dummy=raster::raster(matrix(c(rep(30,450), rep(90,450)), ncol=30))
  urb.dummy=raster::raster(matrix(c(rep(30,450), rep(90,450)), ncol=30, byrow = TRUE))
  shp.dummy <- as(raster::extent(pop.dummy), 'SpatialPolygons')
  raster::crs(pop.dummy)=newproj
  raster::crs(fric.dummy)=newproj
  raster::crs(fric.dummy_b)=newproj
  raster::crs(urb.dummy)=newproj
  raster::crs(acc.dummy)=newproj

  all.raster.dummy=PrepareRasterFiles(population.raster=pop.dummy,
                                      friction.raster=fric.dummy,
                                      shp=shp.dummy, buffer=60, access.raster=acc.dummy,
                                      popurb.raster=urb.dummy,is.inside=F)


  all.raster.dummy_b=PrepareRasterFiles(population.raster=pop.dummy,
                                      friction.raster=fric.dummy_b,
                                      shp=shp.dummy,buffer=60, access.raster=acc.dummy,
                                      popurb.raster=urb.dummy, is.inside=F)


  pos.dummy1=t(c(0.9833333, 0.9166667))
  footprint1=GetFootprintWalkingTime(pos=pos.dummy1, radius=60, pop.map=all.raster.dummy$pop.map, T.GC=all.raster.dummy$T.GC)
  expect_equal(footprint1 , raster::cellFromXY(all.raster.dummy$pop.map,pos.dummy1), label = "footprint1")

  footprint1_90=GetFootprintWalkingTime(pos=pos.dummy1, radius=90, pop.map=all.raster.dummy$pop.map, T.GC=all.raster.dummy$T.GC)
  expect_equal(footprint1_90 , c(60,89,90,120), label = "footprint1 90 min")

  footprint1_b=GetFootprintWalkingTime(pos=pos.dummy1, radius=60, pop.map=all.raster.dummy_b$pop.map, T.GC=all.raster.dummy_b$T.GC)
  expect_equal(footprint1_b ,c(59, 60,89,90,119,120), label = "footprint1 other friction")


  pos.dummy2=t(c(0.3833333, 0.9833333))
  footprint2=GetFootprintWalkingTime(pos=pos.dummy2, radius=60, pop.map=all.raster.dummy$pop.map, T.GC=all.raster.dummy$T.GC)
  expect_equal(footprint2 , raster::cellFromXY(all.raster.dummy$pop.map,pos.dummy2), label = "footprint2")

  footprint2_90=GetFootprintWalkingTime(pos=pos.dummy2, radius=90, pop.map=all.raster.dummy$pop.map, T.GC=all.raster.dummy$T.GC)
  expect_equal(footprint2_90 , c(11,12,13,42), label = "footprint2 90 min")

  footprint2_b=GetFootprintWalkingTime(pos=pos.dummy2, radius=90, pop.map=all.raster.dummy_b$pop.map, T.GC=all.raster.dummy_b$T.GC)
  expect_equal(footprint2_b , c(10,11,12,13,14, 40,41,42,43,44,71,72,73), label = "footprint2 other friction")


})
