required_packages <- c("shiny", "bslib", "dplR", "dplyr", "tidyr", "readr", "treeclim", "plotly")

dir.create("~/R/library", recursive = TRUE, showWarnings = FALSE)
.libPaths(c("~/R/library", .libPaths()))

new_packages <- required_packages[
  !required_packages %in% installed.packages()[, "Package"]
]

if (length(new_packages) > 0) {
  message("Installiere fehlende Pakete: ", paste(new_packages, collapse = ", "))
  install.packages(new_packages, repos = "https://cloud.r-project.org", 
                   lib = "~/R/library", dependencies = TRUE)  # + dependencies
}

# Pakete explizit laden und Fehler abfangen
invisible(lapply(required_packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("Paket konnte nicht geladen werden: ", pkg)
}))


shiny::runGitHub("buche_vs_eiche", "cszang")