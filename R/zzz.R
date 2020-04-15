.onAttach <- function(libname, pkgname) {
 #  msg <- sprintf("This is an early developmental version of anglr (%s),\n still in an experimental state with changes pending.",
 #                 utils::packageVersion("anglr"))
 #  packageStartupMessage(msg)
 invisible()
}
.onLoad <- function(libname, pkgname) {
  ## there's no point doing this, because rgl is loaded first
  # use_rgl_null <- !screen_device()
  # if (use_rgl_null) Sys.setenv(RGL_USE_NULL=TRUE)

  op <- getOption("anglr.max.triangles")
  if (is.null(op)) {
    options(anglr.max.triangles = 1e7)
  }

  invisible()
}

