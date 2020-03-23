.onLoad <- function(libname, pkgname) {
  ## there's no point doing this, because rgl is loaded first
  use_rgl_null <- !screen_device()
  if (use_rgl_null) Sys.setenv(RGL_USE_NULL=TRUE)
  invisible()
}

