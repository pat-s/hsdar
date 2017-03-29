context("vegindex.R")

pacman::p_load(hsdar, raster, here, rasterFunctions, testthat)

testthat::test_that("vegindex works with object of class HyperSpecRaster", {
  
  data("hyperspecs")
  
  out <- vegindex(hyperspecs, c("NDVI", "Boochs"),
                  filename = here("inst/test.tif"))
  
  expect_s4_class(out, "RasterBrick")
  expect_type(out@data@max, "double")
  
})

testthat::test_that("vegindex works with object of class HyperSpecRaster when not writing to disk", {
  
  data("hyperspecs")
  
  out <- vegindex(hyperspecs, "NDVI")
  
  expect_s4_class(out, "RasterBrick")
  expect_type(out@data@max, "double")
  
})

testthat::test_that("vegindex works with object of class Speclib", {
  
  data("spectral_data")
  
  out <- vegindex(spectral_data, c("NDVI", "Boochs"), method = "finApprox")
  
  
})


testthat::test_that("nbi calculation works with object of class HyperSpecRaster when writing to disk", {
  
  data("hyperspecs")
  
  out <- nbi_raster(hyperspecs, filename = here("inst/test.grd"))
})

testthat::test_that("nbi calculation works with object of class HyperSpecRaster when doing calculation in memory", {
  
  data("hyperspecs")
  
  out <- nbi_raster(hyperspecs)
})
