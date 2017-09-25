
library(testthat)
library(magrittr)
library(dplyr)
library(tidyr)
library(graphics)
library(maps)
library(readr)

#Tests for fars_summarize_years() function:
test_that("Testing fars_summarize_years()", {

  years = c("2014","2015")
  summariseDF <- AccidentR::fars_summarize_years(years)

  expect_that(summariseDF, is_a("data.frame"))
  expect_that(ncol(summariseDF), equals((length(years)+1)))
  expect_that(nrow(summariseDF), equals(12))
})

#Tests for fars_map_state() function:
test_that("Testing fars_map_state()", {

  library(magrittr)

  expect_that(AccidentR::fars_map_state("120","2015"), throws_error())

})




