context("test-silicate-sanity")
sc0 <- SC0(minimal_mesh)
sc <- SC(minimal_mesh)
tri <- TRI(minimal_mesh)

test_that("SC0 round trip suite works", {
  expect_silent({
    ## these test had to be removed from silicate, because dep on anglr
    anglr::DEL(SC(sc0))
    plot(anglr::DEL(SC(sc0)))

    anglr::DEL(sc)
    plot(anglr::DEL(sc))
    
    
    anglr::DEL(SC(sc))
    plot(anglr::DEL(SC(sc)))
    
    
    anglr::DEL(SC(tri))
    plot(anglr::DEL(SC(tri)))
    
  })
})


test_that("errors when SC0 round trip unsupported", {
  ## these test had to be removed from silicate, because dep on anglr
  expect_error(DEL(sc0))

  expect_error(DEL(tri))
  
})
