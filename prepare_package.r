# Generate a template for a README.Rmd
#devtools::use_readme_rmd()
# Build README (better to use rmarkdown than knitr!)
rmarkdown::render("README.Rmd", "all")
# Build vignette (better to use rmarkdown than knitr!)
#vigs <- list.files("vignettes", pattern = "rmd$", full.names = TRUE, ignore.case = TRUE)
#for (i in seq_along(vigs)) rmarkdown::render(vigs[i], "all")

# Generate a template for a Code of Conduct
#devtools::use_code_of_conduct()

# Create the travis config file for continuous integration on Linux-OSX
#devtools::use_travis()
# move the newly created .travis.yml to the root directory and modify it

# Create the Appveyor config file for continuous integration on Windows
#devtools::use_appveyor()
# move the newly created appveyor.yml to the root directory and modify it

# Check spelling mistakes
#devtools::spell_check()

pkgdown::build_site()

# Run R CMD check
devtools::check()
# The above will also run the unit tests using testthat
# devtools::test()

# Good practice (@MangoTheCat)
#goodpractice::gp()
