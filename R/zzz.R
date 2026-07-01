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

  message_lines <- c(
    .redcapmissing_startup_style_banner(
      .redcapmissing_startup_banner_lines()
    ),
    .redcapmissing_startup_build_metadata_line(
      version = version,
      release_name = release_name,
      release_update = release_update
    )
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

.redcapmissing_startup_banner_lines <- function() {
  ascii_banner <- c(
    "####  ##### ####   ###   ###  ####  #   # #####  ####  #### ##### #   #  ###",
    "#   # #     #   # #   # #   # #   # ## ##   #   #     #       #   ##  # #   #",
    "#   # #     #   # #     #   # #   # # # #   #   #     #       #   # # # #",
    "####  ####  #   # #     ##### ####  #   #   #    ###   ###    #   #  ## #  ##",
    "# #   #     #   # #     #   # #     #   #   #       #     #   #   #   # #   #",
    "#  #  #     #   # #   # #   # #     #   #   #       #     #   #   #   # #   #",
    "#   # ##### ####   ###  #   # #     #   # ##### ####  ####  ##### #   #  ###"
  )

  chartr("#.", "\u2588\u2591", ascii_banner)
}

.redcapmissing_startup_style_banner <- function(banner_lines) {
  banner_colors <- c("#7dd3fc", "#38bdf8", "#60a5fa", "#818cf8")
  line_colors <- banner_colors[pmin(
    ceiling(seq_along(banner_lines) * length(banner_colors) / length(banner_lines)),
    length(banner_colors)
  )]

  vapply(seq_along(banner_lines), function(line_index) {
    cli::make_ansi_style(line_colors[[line_index]])(banner_lines[[line_index]])
  }, character(1))
}

.redcapmissing_startup_build_metadata_line <- function(
  version,
  release_name,
  release_update
) {
  muted <- cli::make_ansi_style("#64748b")
  version_style <- cli::make_ansi_style("#a78bfa")

  paste0(
    muted("["),
    version_style(paste0("v", version)),
    muted("]"),
    "  ",
    cli::style_bold("Release:"),
    " ",
    .redcapmissing_startup_style_release_name(release_name),
    "  ",
    cli::style_bold("Latest:"),
    " ",
    .redcapmissing_startup_style_update(release_update)
  )
}

.redcapmissing_startup_style_release_name <- function(release_name) {
  cli::make_ansi_style("#38bdf8")(release_name)
}

.redcapmissing_startup_style_update <- function(update) {
  cli::make_ansi_style("#fbbf24")(update)
}
