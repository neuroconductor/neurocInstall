#' @title Neuroconductor Package Table
#' @description Returns the table of Neuroconductor packages
#' @return \code{data.frame} of packages with commit IDs
#' @export
#'
#' @note Package information is obtained from
#' \url{"https://neuroconductor.org/neurocPackages"}
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
  table_url = "https://neuroconductor.org/neurocPackages"
  args = list(file = table_url,
             stringsAsFactors = FALSE, header = TRUE,
             na.strings = "")
  suppressWarnings({
    tab = try( {
      do.call("read.csv", args)
    } , silent = TRUE)
  })
  if (inherits(tab, "try-error")) {
    args$file = gsub("^https", "http", args$file)
    tab = do.call("read.csv", args)
  }

  colnames(tab) = c("repo",
                    "stable.version",
                    "neuroc.stable.version",
                    "stable",
                    "current.version",
                    "neuroc.current.version",
                    "current")

  tab$v = package_version(tab$stable.version)
  ss = split(tab, tab$repo)
  ss = lapply(ss, function(x) {
    x = x[ order(x$v, decreasing = TRUE), ]
    x = x[1,,drop = FALSE]
    x$v = NULL
    x
  })
  tab = do.call("rbind", ss)
  tab = as.data.frame(tab, stringsAsFactors = FALSE)
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
