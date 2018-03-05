context("test-sp-anglr.R")
library(rgl)

data(simpleworld)
library(raster)


test_that("sp works", {
  for (i in sample(seq_len(nrow(simpleworld)), 10)) {
    expect_warning(cmesh <- anglr(simpleworld[i, ]))
    
  }
  
})
