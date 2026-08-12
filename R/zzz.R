# Package startup hooks.

.onAttach <- function(libname, pkgname) {
  if (!isTRUE(getOption("redcapmissing.startup_message", TRUE))) {
    return(invisible())
  }

  packageStartupMessage(.startup_build_message(pkgname = pkgname))
  invisible()
}

.startup_build_message <- function(
  pkgname = "redcapmissing",
  version = .startup_get_version(pkgname)
) {
  release_name <- "run_plan() workflow"
  release_update <- "Improved scope reporting"

  message_lines <- .startup_build_banner_lines(
    pkgname = pkgname,
    version = version,
    release_name = release_name,
    release_update = release_update
  )

  paste(message_lines, collapse = "\n")
}

.startup_get_version <- function(pkgname) {
  version <- suppressWarnings(
    utils::packageDescription(pkgname, fields = "Version")
  )

  if (length(version) != 1L || is.na(version) || !nzchar(version)) {
    return("unknown")
  }

  version
}

.startup_build_banner_lines <- function(
  pkgname,
  version,
  release_name,
  release_update
) {
  target_style <- cli::make_ansi_style("#f7fcff")
  sweep_style <- cli::make_ansi_style("#22d3ee")
  blip_style <- cli::make_ansi_style("#ef4444")
  blue_style <- cli::make_ansi_style("#3b82f6")
  green_style <- cli::make_ansi_style("#22c55e")
  gold_style <- cli::make_ansi_style("#fbbf24")
  version_style <- cli::make_ansi_style("#94a3b8")
  release_style <- cli::make_ansi_style("#c084fc")

  c(
    paste0(strrep(" ", 13L), target_style("\u25c9")),
    paste0(strrep(" ", 12L), sweep_style("\u2571 \u2572")),
    paste0(
      strrep(" ", 11L),
      blip_style("\u25cf"),
      "   ",
      blip_style("\u25cf")
    ),
    paste0(
      strrep(" ", 10L),
      sweep_style("\u2571\u2572"),
      "   ",
      sweep_style("\u2571\u2572")
    ),
    paste0(
      strrep(" ", 9L),
      blue_style("\u00b7"),
      "  ",
      blue_style("\u00b7"),
      " ",
      gold_style("\u00b7"),
      "  ",
      gold_style("\u00b7")
    ),
    "",
    paste0(strrep(" ", 7L), green_style(pkgname)),
    paste0(
      strrep(" ", 7L),
      version_style(paste0("v", version)),
      " ",
      gold_style("\u00b7"),
      " ",
      release_style(release_name)
    )
  )
}
