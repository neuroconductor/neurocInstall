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
                         release = c("stable", "current"),
                         upgrade_dependencies = FALSE,
                         ...){

  #############################
  # Create a data.frame for merging
  #############################
  release = match.arg(release)

  df = data.frame(repo = repo, stringsAsFactors = FALSE)

  tab = neuro_package_table(long = TRUE)
  tab = tab[ tab$release %in% release, ]

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
  tab$version = numeric_version(tab$version)

  # pkg = tab$pkg
  # tab$commit_id = tab[, "commit_id"]
  tab = split(tab, tab$repo)
  tab = lapply(tab, function(x) {
    x$version = x[, "version"]
    max_version = max(x$version)
    x = x[ x$version %in% max_version,, drop = FALSE]
    return(x)
  })
  tab = do.call("rbind", tab)
  tab = data.frame(tab, stringsAsFactors = FALSE)
  tab$repo = paste0("neuroconductor/", tab$repo, "@", tab$commit_id)

  if (!upgrade_dependencies) {
    res = try({
      results = devtools::install_github(
        tab$repo,
        upgrade_dependencies = upgrade_dependencies,
        ...)
    })
    if (inherits(res, "try-error") || any(!results)) {
      stop("Installation failed, please try with upgrade_dependencies = TRUE")
    }
  } else {
    devtools::install_github(tab$repo,
                             upgrade_dependencies = upgrade_dependencies,
                             ...)
  }
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

