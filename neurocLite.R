# neurocInstall package version: 0.2.2
if (!require(devtools)) {
  install.packages("devtools")
}
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
neuro_install = function(repo,
                         release = c("stable", "development"),
                         upgrade_dependencies = TRUE,
                         ...){

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
            paste(unique(tab$repo), collapse = ",")))
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
neuroc_install = function(...) {
  neuro_install(...)
}

#' @rdname neuro_install
neurocLite = function(...) {
  neuro_install(...)
}


#' @title Neuroconductor Package Table
#' @description Returns the table of Neuroconductor packages
#' @return \code{data.frame} of packages with commit IDs
#' @export
#' 
#' @note Package information is obtained from 
#' \url{"http://neuroconductor.org/neurocPackages"}
#'
#' @examples
#' neuro_package_table()
neuro_package_table = function() {
  #############################
  ## grab list of current neuroc packages
  #############################
  # table_url = paste0("http://162.129.222.10/sites/default",
  # "/files/neuroc_packages.txt"

  # table_url = paste0("http://neuroconductor.org/sites/default",
  #                    "/files/neuroc_packages.txt")
  table_url = "http://neuroconductor.org/neurocPackages"
  tab = read.csv(file = table_url,
                 stringsAsFactors = FALSE, header = TRUE,
                 na.strings = "")
  colnames(tab) = c("repo", "version", "stable", "development")
  return(tab)
}

#' @title Neuroconductor Packages
#' @description Returns the vector of Neuroconductor packages
#' @return \code{vector} of packages available on Neuroconductor
#' @export
#'
#' @examples
#' neuro_packages()
neuro_packages = function() {
  tab = neuro_package_table()
  tab = tab$repo
  return(tab)
}
