
#' @title Neuroconductor Installer
#'
#' @param pkg Package name in neuroconductor
#' @param release Stable or development version
#' @return
#' @export
#' @importFrom devtools install_github
#' @examples
neuro_install = function(pkg,
                         release = c("stable", "development")){

  #############################
  # Create a data.frame for merging
  #############################
  release = match.arg(release)
  df = data.frame(pkg = pkg, stringsAsFactors = FALSE)

  #############################
  ## grab list of current neuroc packages
  #############################
  # table_url = "http://"
  table_url = "https://raw.githubusercontent.com/muschellij2/neuroconductor/master/example_text.txt"
  tab = read.delim(file = table_url, stringsAsFactors = FALSE)
  colnames(tab) = c("pkg", "version", "stable", "development")

  ## import list of packages
  # error if pkg not in list of packages
  check_install = df$pkg %in% tab$pkg
  if (!all(check_install)) {
    bad_pkgs = df$"pkg"[check_install]
    bad_pkgs = paste(bad_pkgs, collapse = ", ")
    stop(paste0("Packages ", bad_pkgs,
                " are not in neuroconductor"))
  }
  tab = merge(df, tab, by = "pkg", all.x = TRUE)

  # pkg = tab$pkg
  # commit_id = tab[, release]
  tab$pkg = paste0("neuroconductor/", tab$pkg, tab[, release])
  devtools::install_github(tab$pkg)
}