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

devtools::install(".", dependencies = TRUE, upgrade_dependencies = TRUE)

# system("Rscript inst/logo.R", ignore.stdout = TRUE, wait = TRUE, 
#        show.output.on.console = FALSE) # rlang update killed it

rmarkdown::render("README.Rmd", output_format = "github_document",
                  envir = new.env())

covrpage::covrpage(preview = FALSE, vignette = TRUE)

# library(tibble);library(pillar);library(crayon)

pkgdown::build_site(lazy = FALSE, seed = 831)
