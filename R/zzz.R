# Package startup hooks.

.onAttach <- function(libname, pkgname) {
  if (!isTRUE(getOption("redcapmissing.startup_message", TRUE))) {
    return(invisible())
  }

  packageStartupMessage(.redcapmissing_startup_build_message(pkgname = pkgname))
  invisible()
}

.redcapmissing_startup_build_message <- function(
  pkgname = "redcapmissing",
  version = .redcapmissing_startup_get_version(pkgname)
) {
  release_name <- "Forever-searching"
  release_update <- "Improved scope reporting"

  message_lines <- .redcapmissing_startup_banner_lines(
    pkgname = pkgname,
    version = version,
    release_name = release_name,
    release_update = release_update
  )

  paste(message_lines, collapse = "\n")
}

.redcapmissing_startup_get_version <- function(pkgname) {
  version <- suppressWarnings(
    utils::packageDescription(pkgname, fields = "Version")
  )

  if (length(version) != 1L || is.na(version) || !nzchar(version)) {
    return("unknown")
  }

  version
}

.redcapmissing_startup_banner_lines <- function(
  pkgname,
  version,
  release_name,
  release_update
) {
  package_style <- cli::make_ansi_style("#38bdf8")
  release_style <- cli::make_ansi_style("#6ee7b7")
  version_style <- cli::make_ansi_style("#f97316")
  update_style <- cli::make_ansi_style("#c4b5fd")

  c(
    package_style(paste0("> ", pkgname)),
    paste0(
      "  ",
      release_style(release_name),
      "  ",
      version_style(paste0("{v", version, "}"))
    ),
    update_style(paste0("  :: ", release_update))
  )
}
