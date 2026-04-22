required_packages <- c("shiny", "bslib", "dplR", "dplyr", "tidyr", "readr", "treeclim", "plotly")

new_packages <- required_packages[
  !required_packages %in% installed.packages()[, "Package"]
]

if (length(new_packages) > 0) {
  message("Installiere fehlende Pakete: ", paste(new_packages, collapse = ", "))
  install.packages(new_packages, repos = "https://cloud.r-project.org")
}

shiny::runGitHub("buche_vs_eiche", "cszang")