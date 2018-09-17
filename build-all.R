if ("snatools" %in% installed.packages()[, "Package"]) {
  remove.packages("snatools")
}

if (dir.exists("docs")) {
  pkgdown::clean_site()
  unlink("docs", recursive = TRUE, force = TRUE)
}

if (dir.exists("man")) {
  unlink("man", recursive = TRUE, force = TRUE)
}

devtools::document(".")

devtools::install(".", dependencies = FALSE, upgrade_dependencies = FALSE)

system("Rscript inst/logo.R", ignore.stdout = TRUE, wait = TRUE, 
       show.output.on.console = FALSE)

rmarkdown::render("README.Rmd", output_format = "github_document")

pkgdown::build_site(lazy = FALSE, seed = 831)

