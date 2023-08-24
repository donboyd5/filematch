# package building basics ----
# https://r-pkgs.org/
# https://github.com/hadley/r-pkgs

# https://rstudio.github.io/r-manuals/r-exts/Function-and-variable-index.html

# https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html


# navigation notes ----
# alt-o, shift-alt-o
# alt-l, shift-alt-l

# alt-r

# shortcuts ----
# Install Package: 'Ctrl + Shift + B'
# Document ctrl-shift-d
# Check Package: 'Ctrl + Shift + E'
# Test Package: 'Ctrl + Shift + T'


# examples ----
# https://github.com/jennybc/regexcite


# setup ----

#..++ set up .Rprofile ----
if (interactive()) {
  suppressMessages(require(devtools))
  suppressMessages(require(usethis))
}
options(
  usethis.full_name = "Don Boyd",
  usethis.description = list(
    `Authors@R` = 'person("Don", "Boyd", email = "donboyd5@gmail.com", role = c("aut", "cre"),
    comment = c(ORCID = "YOUR-ORCID-ID"))',
    License = "GPL-2",
    Version = "0.0.0.9000"
  ),
  # usethis.protocol  = "ssh"
)

# toy package ----
library(devtools)
create_package("~/regexcite") # C:/Users/donbo/regexcite/

# filematch ----
library(devtools)
library(fs)
pdir <- r"(E:\R_projects\packages)"
ppath <- path(pdir, "filematch")
create_package(ppath)

# go to the RStudio session that was opened up
use_git()





#..++ Create package if not already done ----
# The main and only required argument to create_package() is the path where your new package will live:
# pkg <- r"(C:\Users\donbo\Documents\R_projects\packages\bggplot)"
# create_package(pkg)
# then from within the new package:
# use_gpl3_license()
# use_git()
# do some work, commit, then
# use_github()
# use_pipe()

#..++ set up .Rbuildignore in project directory ----
# here is an example for tidyverse, do not include the comments after each line
# ^.*\.Rproj$         # Designates the directory as an RStudio Project
# ^\.Rproj\.user$     # Used by RStudio for temporary files
# ^README\.Rmd$       # An Rmd file used to generate README.md
# ^LICENSE\.md$       # Full text of the license
# ^cran-comments\.md$ # Comments for CRAN submission
# ^data-raw$          # Code used to create data included in the package
# ^pkgdown$           # Resources used for the package website
# ^_pkgdown\.yml$     # Configuration info for the package website
# ^\.github$          # Contributing guidelines, CoC, issue templates, etc.

#..++ set up DESCRIPTION ----
# use_gpl3_license()

# note that tidyverse includes:
# ggplot2, purrr
# tibble, dplyr, tidyr
# stringr
# readr, forcats

# edit DESCRIPTION
# here's an example:
# Package: bggplot
# Title: Tools, etc. for ggplot2
# Version: 0.0.0.9000
# Authors@R:
#   person("Don", "Boyd", , "donboyd5@gmail.com", role = c("aut", "cre"))
# Description: What the package does (one paragraph).
# License: GPL (>= 3)
# LazyData: true
# Imports:
#   dplyr,
#   lubridate,
#   magrittr,
#   scales,
#   stats,
#   tidyverse,
#   zoo
# Encoding: UTF-8
# Roxygen: list(markdown = TRUE)
# RoxygenNote: 7.1.2


# workflow ----
#.. potentially useful ----
devtools::session_info()
formatR::tidy_dir("R")
lintr::lint_package()
use_r("filename") # create/open script in R/ folder

#.. simulate installing and loading ----
library(devtools)
load_all()


#.. documentation ----
# revise roxygen2 comments
# ctrl-shift-d
# ?<function name> to render



# edit the DESCRIPTION file using these commands library(usethis) This block
# will update the DESCRIPTION file when devtools::document() is run (or via
# shift-ctrl-d) #### usethis::use_package("plyr") usethis::use_package("dplyr")

# usethis::use_package("ggplot")
# usethis::use_package("lubridate")
# usethis::use_package("magrittr")
# usethis::use_package("precis")
# usethis::use_package("scales")
# usethis::use_package("stats")
# usethis::use_package("stringr")
# usethis::use_package("tibble")
# usethis::use_package("zoo")

# usethis::use_package("bdata")
# usethis::use_package("usmap")

# how to build package source ----
devtools::build(
  pkg = here::here(),  # location of source code
  path = NULL,  # where to put the tar file
  binary = FALSE,
  vignettes = FALSE,
  manual = FALSE,
  args = NULL,
  quiet = FALSE)


# This block (if uncommented) will update the NAMESPACE file when devtools::document() is run (or via shift-ctrl-d) ####

#' #' Pipe operator
#' #'
#' #' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#' #'
#' #' @name %>%
#' #' @rdname pipe
#' #' @keywords internal
#' #' @export
#' #' @importFrom magrittr %>%
#' #' @usage lhs \%>\% rhs
#' NULL






