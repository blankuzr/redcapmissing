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
  release_name <- "eye-spy"
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
  .redcapmissing_startup_banner_line(
    pkgname = pkgname,
    version = version,
    release_name = release_name
  )
}

.redcapmissing_startup_banner_line <- function(
  pkgname,
  version,
  release_name
) {
  prompt_style <- cli::make_ansi_style("#ff8a00")
  package_style <- cli::make_ansi_style("#ff8a00")
  version_style <- cli::make_ansi_style("#ff2d20")
  release_style <- cli::make_ansi_style("#ffd166")

  paste0(
    prompt_style("> "),
    package_style(pkgname),
    " ",
    version_style(paste0("{v", version, "}")),
    " ~ ",
    release_style(release_name)
  )
}
