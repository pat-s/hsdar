
pacman::p_load(hsdar, raster, here, rasterFunctions, testthat)

testthat::test_that("vegindex works with object of class HyperSpecRaster", {
  
  hyperspecs <- readRDS(here("inst/hyperspecraster.rda"))
  
  out <- vegindex(hyperspecs, "NDVI", method = "finApprox", nl = 1,
                  filename = here("inst/test.tif"))
  
  expect_s4_class(out, "RasterLayer")
  expect_type(out@data@max, "double")
  
})

testthat::test_that("vegindex works with object of class HyperSpecRaster when not writing to disk", {
  
  hyperspecs <- readRDS(here("inst/hyperspecraster.rda"))
  
  out <- vegindex(hyperspecs, "NDVI", method = "finApprox")
  
  expect_s4_class(out, "RasterLayer")
  expect_type(out@data@max, "double")
  
})
