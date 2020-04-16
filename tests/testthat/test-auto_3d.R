
test_that("auto_3d does something", {


    rgl::open3d()
    suppressWarnings(topo <- copy_down(silicate::SC(simpleworld), gebco))
    plot3d(topo)

    #expect_true(all(rgl::par3d("scale") == c(1, 1, 1)))
    scl <- auto_3d()
    expect_equivalent(rgl::par3d("scale") > scl, c(TRUE, TRUE, FALSE))
    rgl::rgl.close()
})
