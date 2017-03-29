context("vegindex.R")

pacman::p_load(hsdar, raster, here, rasterFunctions, testthat)

testthat::test_that("vegindex works with object of class HyperSpecRaster", {
  
  hyperspecs <- readRDS(here("inst/hyperspecraster.rda"))
  
  out <- vegindex(hyperspecs, c("NDVI", "Boochs"), method = "finApprox", nl = 2,
                  filename = here("inst/test.tif"))
  
  expect_s4_class(out, "RasterBrick")
  expect_type(out@data@max, "double")
  
})

testthat::test_that("vegindex works with object of class HyperSpecRaster when not writing to disk", {
  
  hyperspecs <- readRDS(here("inst/hyperspecraster.rda"))
  
  out <- vegindex(hyperspecs, "NDVI", method = "finApprox")
  
  expect_s4_class(out, "RasterBrick")
  expect_type(out@data@max, "double")
  
})

testthat::test_that("vegindex works with object of class Speclib", {
  
  data("spectral_data")
  
  out <- vegindex(spectral_data, c("NDVI", "Boochs"), method = "finApprox")
  
  
})


testthat::test_that("nbi calculation works with object of class HyperSpecRaster when writing to disk", {
  
  hyperspecs <- readRDS(here("inst/hyperspecraster.rda"))
  
  out <- nbi_raster(hyperspecs, "NBI", nl = 7875,
             filename = here("inst/test"), bnames = "NBI")
})

testthat::test_that("nbi calculation works with object of class HyperSpecRaster when doing calculation in memory", {
  
  hyperspecs <- readRDS(here("inst/hyperspecraster.rda"))
  
  out <- nbi_raster(hyperspecs, nl = 7875, bnames = "NBI")
})
