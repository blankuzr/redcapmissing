# Render the tracked vignette from its authored source using the current checkout.
#
# Run from the package root:
#   Rscript tools/render-vignette.R

if (!file.exists("DESCRIPTION") || !dir.exists("vignettes")) {
  stop("Run this renderer from the redcapmissing package root.", call. = FALSE)
}

devtools::load_all(quiet = TRUE)
rmarkdown::render(
  "vignettes/redcapmissing.Rmd",
  output_file = "redcapmissing.html",
  output_dir = "vignettes",
  quiet = FALSE,
  envir = new.env(parent = globalenv())
)

output_path <- "vignettes/redcapmissing.html"
rendered_lines <- readLines(output_path, warn = FALSE, encoding = "UTF-8")
normalized_lines <- sub("[\t ]+$", "", rendered_lines, perl = TRUE)
writeLines(normalized_lines, output_path, useBytes = TRUE)
