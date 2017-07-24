#' @title Neuroconductor Package Table
#' @description Returns the table of Neuroconductor packages
#' @return \code{data.frame} of packages with commit IDs
#' @param path Path to the table of package
#' @param long Should the data be "long" (with respect to stable/current)
#' @export
#'
#' @note Package information is obtained from
#' \url{"https://neuroconductor.org/neurocPackages"}
#'
#' @importFrom stats reshape
#' @examples
#' neuro_package_table()
neuro_package_table = function(
  path = "https://neuroconductor.org/neurocPackages",
  long = FALSE
) {
  #############################
  ## grab list of current neuroc packages
  #############################
  # table_url = paste0("http://162.129.222.10/sites/default",
  # "/files/neuroc_packages.txt"

  # table_url = paste0("http://neuroconductor.org/sites/default",
  #                    "/files/neuroc_packages.txt")
  args = list(file = path,
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
                    "version.stable",
                    "neuroc_version.stable",
                    "commit_id.stable",
                    "version.current",
                    "neuroc_version.current",
                    "commit_id.current")

  tab$v = package_version(tab$version.stable)
  ss = split(tab, tab$repo)
  ss = lapply(ss, function(x) {
    x = x[ order(x$v, decreasing = TRUE), ]
    x = x[1,,drop = FALSE]
    x$v = NULL
    x
  })
  tab = do.call("rbind", ss)
  tab = as.data.frame(tab, stringsAsFactors = FALSE)

  rownames(tab) = NULL
  if (long) {
    cn = colnames(tab)
    varying = cn[ cn != "repo"]
    tab = reshape(data = tab, direction = "long", idvar = "repo", varying = varying,
            times = c("current", "stable"), timevar = "release")
    rownames(tab) = NULL
  }
  return(tab)
}



#' @title Neuroconductor Packages
#' @description Returns the vector of Neuroconductor packages
#' @return \code{vector} of packages available on Neuroconductor
#' @param ... Arguments passed to \code{\link{neuro_package_table}}
#'
#' @export
#'
#' @examples
#' neuro_packages()
neuro_packages = function(...) {
  tab = neuro_package_table(...)
  tab = tab$repo
  tab = unique(tab)
  return(tab)
}
