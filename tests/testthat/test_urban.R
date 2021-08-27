test_that("test urban area definition", {

  # create a dummy population
  pop.dummy.low=raster::raster(matrix(c(rep(100,225), rep(5,225)), ncol=30))
  pop.dummy.up=raster::raster(matrix(c(rep(50,225), rep(200,225)), ncol=30))
  pop.dummy.up@extent@ymin=1
  pop.dummy.up@extent@ymax=2

  pop.dummy=raster::merge(pop.dummy.low,pop.dummy.up)
  names(pop.dummy)="pop"
  # visual check of the dummy population
  #rasterVis::gplot(pop.dummy)+ geom_tile(aes(fill=factor(value)), alpha=0.6) + labs(fill="Population")


  urb0=DefineUrban(pop.dummy, rururb_cutoff = 3)  # all is urban
  expect_equal(urb0, pop.dummy, label = "test when all is urban")

  urb1=DefineUrban(pop.dummy, rururb_cutoff = 40)  # half if urban, because of the cutoff
  pop.dummy.low1=raster::raster(matrix(c(rep(100,225), rep(NA,225)), ncol=30))
  pop.dummy.1=raster::merge(pop.dummy.low1,pop.dummy.up)
  names(pop.dummy.1)="pop"
  expect_equal(urb1, pop.dummy.1, label = "test when only 3 panels are urban, due to cutoff")

  urb2=DefineUrban(pop.dummy, rururb_cutoff = 40, min_urbsize = 30000)
  expect_equal(urb2, pop.dummy.1, label = "test when only 3 panels are urban, due to cutoff and urbsize")

  urb3=DefineUrban(pop.dummy, rururb_cutoff = 60, min_urbsize = 30000)
  pop.dummy.low3=raster::raster(matrix(rep(NA,450), ncol=30))
  pop.dummy.up3=raster::raster(matrix(c(rep(NA,225), rep(200,225)), ncol=30))
  pop.dummy.up3@extent@ymin=1
  pop.dummy.up3@extent@ymax=2
  pop.dummy.3=raster::merge(pop.dummy.low3,pop.dummy.up3)
  names(pop.dummy.3)="pop"
  expect_equal(urb3, pop.dummy.3, label = "test when only 1 panel is urban, due to cutoff and urbsize")
})



