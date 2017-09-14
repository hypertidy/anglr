.onLoad <- function(libname, pkgname) {
  use_rgl_null <- isTRUE(!capabilities()[["X11"]])
  # op <- options()
  # op.anglr <- list(
  #   rgl.useNULL = use_rgl_null
  # )
   if (use_rgl_null) Sys.setenv(RGL_USE_NULL=TRUE)
  # print("usenull!")
  # print(use_rgl_null)
  # toset <- !(names(op.anglr) %in% names(op))
  # if(any(toset)) options(op.anglr[toset])
  # 
  invisible()
}

