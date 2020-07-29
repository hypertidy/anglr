library(anglr)
## https://github.com/hypertidy/anglr/issues/7#issuecomment-628362518
cst10 <- readRDS(system.file("extdata/cst10_polygon.rds", package = "anglr", mustWork = TRUE))

usethis::use_data(cst10, version = 2)

#library(silicate)
#p <- PATH0(cst10)
# DEL0(p)  ## fails
## fails at 14 and crashes R in DEL0() in R version 4.0.0 RC (2020-04-17 r78247) on
## windows
# p$vertex$x_ <- signif(p$vertex$x_, 13)
# p$vertex$y_ <- signif(p$vertex$y_, 13)
# DEL0(p)


