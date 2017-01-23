#' @title Neuroconductor Installer
#' @description Install function for neuroconductor packages
#' @param repo Package name in neuroconductor
#' @param release Stable or development version
#' @param upgrade_dependencies Should dependencies be updated?
#' passed to \code{\link[devtools]{install}}
#' @param ... additional arguments passed to
#' \code{\link[devtools]{install_github}}
#' @return Result from \code{\link[devtools]{install_github}}
#' @export
#' @importFrom devtools install_github
#' @importFrom utils read.csv
#' @importFrom utils compareVersion install.packages installed.packages
neuro_install = function(repo,
                         release = c("stable", "development"),
                         upgrade_dependencies = TRUE,
                         ...){

  #########################################
  # Checking devtools version
  #########################################
  get_devtools_version = function() {
    ipacks = installed.packages()
    dtools = ipacks[ ipacks[, "Package"] %in% "devtools", "Version"]
    return(dtools)
  }
  # needed for bioc stuff
  req_version = "1.12.0.9000"
  dtools = get_devtools_version()
  install_from_cran = FALSE
  if (length(dtools) == 0 ) {
    install_from_cran = TRUE
  } else {
    # too low of a version
    install_from_cran = compareVersion(dtools, req_version) < 0
  }
  if (install_from_cran) {
    install.packages("devtools")
  }

  # now we assume devtools is installed
  dtools = get_devtools_version()
  if (length(dtools) == 0 ) {
    stop(paste0("devtools tried to install but could not ",
                "- try to install devtools manually.",
                "Version >= ", req_version, " required."
                )
    )
  } else {
    comparison = compareVersion(dtools, "1.12.0.9000")
    if (comparison < 0) {
      devtools::install_github("hadley/devtools")
    }
  }

  #############################
  # Create a data.frame for merging
  #############################
  release = match.arg(release)
  df = data.frame(repo = repo, stringsAsFactors = FALSE)

  tab = neuro_package_table()
  ## import list of packages
  # error if pkg not in list of packages
  check_install = df$repo %in% tab$repo
  if (!all(check_install)) {
    bad_pkgs = df$repo[!check_install]
    bad_pkgs = paste(bad_pkgs, collapse = ", ")
    message(paste0("Available Packages on neuroconductor are ",
            paste(unique(tab$repo), collapse = ", ")))
    stop(paste0("Package(s) ", bad_pkgs,
                " are not in neuroconductor"))
  }
  tab = merge(df, tab, by = "repo", all.x = TRUE)

  # pkg = tab$pkg
  tab$commit_id = tab[, release]
  tab$repo = paste0("neuroconductor/", tab$repo, "@", tab$commit_id)
  devtools::install_github(tab$repo,
                           upgrade_dependencies = upgrade_dependencies,
                           ...)
}

#' @rdname neuro_install
#' @aliases neuroc_install
#' @aliases neuro_install
#' @export
neuroc_install = function(...) {
  neuro_install(...)
}

#' @rdname neuro_install
#' @aliases neurocLite
#' @export
neurocLite = function(...) {
  neuro_install(...)
}

