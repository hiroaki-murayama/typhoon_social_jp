required_packages <- c(
  "dplyr",
  "magrittr",
  "tidyr",
  "tibble",
  "purrr",
  "ggplot2",
  "ggpubr",
  "readr",
  "splines2",
  "spdep",
  "geodata",
  "sf",
  "Matrix",
  "scales",
  "INLA",
  "IRkernel"
)

options(
  repos = c(
    CRAN = "https://cloud.r-project.org",
    INLA = "https://inla.r-inla-download.org/R/stable"
  )
)

cran_packages <- c("geodata")
missing_cran <- cran_packages[!vapply(cran_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing_cran) > 0) {
  install.packages(missing_cran, dependencies = NA)
}

if (!requireNamespace("INLA", quietly = TRUE)) {
  install.packages("INLA", dependencies = NA)
}

IRkernel::installspec(user = FALSE)

missing_required <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]
if (length(missing_required) > 0) {
  stop("Missing R packages after image build: ", paste(missing_required, collapse = ", "))
}
