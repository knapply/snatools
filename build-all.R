if("snatools" %in% installed.packages()[, "Package"]) {
  remove.packages("snatools")
}

if(dir.exists("docs")) {
  pkgdown::clean_site()
  unlink("docs", recursive = TRUE, force = TRUE)
}

if(dir.exists("man")) {
  unlink("man", recursive = TRUE, force = TRUE)
}

devtools::document(".")

devtools::install(".", dependencies = FALSE, upgrade_dependencies = FALSE)

# source("inst/logo.R")

rmarkdown::render("README.Rmd", output_format = "github_document")

pkgdown::build_site()

