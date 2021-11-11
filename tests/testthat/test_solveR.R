test_that("solve with R", {

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
                     name="temp", buffer=60, radius=600, capacity.name="",
                     access.raster=acc.dummy,popurb.raster=urb.dummy,
                     max.treat.per.CHW.urban=1000, max.treat.per.CHW.rural=2500,
                     max.CHW.per.pixel=1, is.inside = FALSE, filepath=".")


  CMWanswer=solve_CHWplacement_wrapper(population.raster=pop.dummy, friction.raster=fric.dummy,shp=shp.dummy,
                                       name="temp", buffer=60, radius=600, capacity.name="",
                                       access.raster=acc.dummy,popurb.raster=urb.dummy,
                                       is.inside = FALSE, filepath=".", mytimeout=10)

  CHWanswer.expected=data.frame(x=c(0.7666667,0.5666667,0.9000000,0.9666667,0.9000000,0.9666667,0.5666667,0.6333333,0.9000000,0.7666667),
                        y=c(0.9666667, 0.6333333, 0.6333333, 0.6333333, 0.5000000, 0.5000000, 0.4333333, 0.4333333, 0.3000000, 0.1666667),
                        is.rural=rep(0,10), capacity=rep(1000,10))

  expect_equal(CMWanswer, CHWanswer.expected, label = "test solve R", tolerance=1e-05)

})
#
# ## other test, but takes longer to run
# test_that("solve with R", {
#
#   newproj= "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
#   pop.dummy=raster::raster( matrix(rep(100,225), ncol=15))
#   fric.dummy=raster::raster(matrix(rep(0.02,225), ncol=15))
#   acc.dummy=raster::raster(matrix(c(rep(30,125), rep(90,100)), ncol=15))
#   urb.dummy=raster::raster(matrix(c(rep(30,125), rep(90,100)), ncol=15, byrow = TRUE))
#   shp.dummy <- as(raster::extent(pop.dummy), 'SpatialPolygons')
#   raster::crs(pop.dummy)=newproj
#   raster::crs(fric.dummy)=newproj
#   raster::crs(urb.dummy)=newproj
#   raster::crs(acc.dummy)=newproj
#   names(acc.dummy)=""
#   names(urb.dummy)=""
#
#
#   CreateCHWplacement(population.raster=pop.dummy, friction.raster=fric.dummy,shp=shp.dummy,
#                      name="temp", buffer=60, radius=600, capacity.name="",
#                      access.raster=acc.dummy,popurb.raster=urb.dummy,
#                      max.treat.per.CHW.urban=2500, max.treat.per.CHW.rural=2500,
#                      max.CHW.per.pixel=1, is.inside = FALSE, filepath=".")
#
#
#   CMWanswer=solve_CHWplacement_wrapper(population.raster=pop.dummy, friction.raster=fric.dummy,shp=shp.dummy,
#                                        name="temp", buffer=60, radius=600, capacity.name="",
#                                        access.raster=acc.dummy,popurb.raster=urb.dummy,
#                                        is.inside = FALSE, filepath=".", mytimeout=20)
#
#   CHWanswer.expected=data.frame(x=c(0.7666667,0.9666667,0.7000000,0.7666667),
#                                 y=c(0.9000000,0.5000000,0.4333333,0.1666667 ),
#                                 is.rural=rep(0,4), my.list.capa.here=rep(2500,4))
#
#   expect_equal(CMWanswer, CHWanswer.expected, label = "test solve R", tolerance=1e-05)
#
# })
