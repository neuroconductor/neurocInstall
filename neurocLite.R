# neurocInstall package version: 0.10.1
pkg_ver = '# neurocInstall package version: 0.10.1'
source("https://bioconductor.org/biocLite.R")
biocLite(suppressUpdates = TRUE,
         suppressAutoUpdate = TRUE,
         ask = FALSE)
# if (!require("neurocInstall")) {
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
message(paste("Using neurocLite version:", pkg_ver))

	#' @title Neuroconductor Installer
	#' @description Install function for neuroconductor packages
	#' @param repo Package name in neuroconductor
	#' @param release Stable or current (development) versions/branches
	#' @param release_repo Repo for release repository, passed to
	#' \code{\link{install.packages}}.  If \code{release_repo = "github"},
	#' then it will install using GitHub.  If you set this using
	#' \code{\link{make_release_version}} or specify the URL directly,
	#' it will override \code{release} option.
	#'
	#' @param upgrade_dependencies Should dependencies be updated?
	#' passed to \code{\link[devtools]{install}} if using
	#' \code{release_repo = "github"}
	#' @param ... additional arguments passed to
	#' \code{\link{install.packages}}
	#' or \code{\link[devtools]{install_github}} if
	#' \code{release_repo = "github"}
	#'
	#' @return Result from  \code{\link{install.packages}} or
	#' \code{\link[devtools]{install_github}}
	#'
	#' @export
	#' @importFrom devtools install_github
	#' @importFrom utils read.csv
	#' @importFrom utils compareVersion install.packages installed.packages
	#'
	#' @examples
	#'    tlib = tempfile()
	#'    dir.create(tlib, showWarnings = FALSE)
	#'    neuro_install("cifti", lib = tlib)
	#'    neuro_install("cifti", type = "source")
	#'
	neuro_install = function(
	  repo,
	  release = c("stable", "current"),
	  release_repo = make_release_version("2017/nov"),
	  upgrade_dependencies = FALSE,
	  ...){
	
	  #############################
	  # Create a data.frame for merging
	  #############################
	  release = match.arg(release)
	
	  l_repo = trimws(tolower(release_repo))
	
	  if (!l_repo %in% "github") {
	    x = install.packages(pkgs = repo, repos = release_repo, ...)
	    not_installed = repo[!repo %in% installed.packages()]
	    if (length(not_installed) > 0) {
	      msg = paste0("Package(s): ", paste(not_installed, sep = ", "),
	                   " released binaries/sources were not installed,",
	                   " please try to install with release_repo = \"github\"")
	      warning(msg)
	    }
	    return(invisible(x))
	  }
	
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
	
	#' @title Make Full Package Version
	#' @description Makes a package version to have all the same length.
	#' This is helpful when using \code{\link{compareVersion}}.
	#'
	#' @param x Character vector of package versions
	#'
	#' @return Character vector of versions, each with the same length.
	#' @export
	#'
	#' @examples
	#' x = c("1.6", "1.6.0")
	#' compareVersion(x[1], x[2])
	#' x2 = make_full_version(x)
	#' compareVersion(x2[1], x2[2])
	#' x = c("1.6", "1.6.0")
	#' compareVersion(x2[1], x2[2])
	make_full_version = function(x) {
	  nx = names(x)
	  x = as.character(x)
	  r <- lapply(strsplit(x, "[.-]"), as.integer)
	  lx = sapply(r, length)
	  mlx = max(lx)
	  r <- lapply(r, function(ver) {
	    c(ver, rep(0, length = mlx - length(ver)))
	  })
	  x = sapply(r, paste, collapse = ".")
	  names(x) = nx
	  return(x)
	}
	
	
	
	#' Latest Neuroconductor Release Location
	#'
	#' @param secure Should https vs. http be used
	#' @param release Stable or current (development) versions
	#' @return URL fo release page
	#' @export
	#'
	#' @examples
	#' latest_neuroc_release()
	latest_neuroc_release = function(
	  release = c("stable", "current"),
	  secure = TRUE) {
	  # release_version = "2017/nov/"
	  release = match.arg(release)
	  release_version = paste0("latest/", release, "/")
	  release_version = make_release_version(release_version, secure = secure)
	  return(release_version)
	}
	
	#' @rdname latest_neuroc_release
	#' @param release_path path to the release on
	#' \url{https://neuroconductor.org/releases/}
	#' @export
	make_release_version = function(release_path = NULL, secure = TRUE) {
	  if (is.null(release_path)) {
	    # read from the page Adi Makes
	    # currently fail
	    stop("Need to read in the page from Adi - JOHN!")
	  }
	  release_path = paste0(
	    "http", ifelse(secure, "s", ""), "://neuroconductor.org/releases/",
	    release_path)
	  release_path
	}
	

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
	
	  xcn = colnames(tab) = c("repo",
	                          "version.stable",
	                          "neuroc_version.stable",
	                          "commit_id.stable",
	                          "version.current",
	                          "neuroc_version.current",
	                          "commit_id.current")
	
	  tab$v = package_version(tab$version.stable)
	  if (nrow(tab) == 0 & !long) {
	    return(tab)
	  }
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
	    if (nrow(tab) == 0) {
	      cn = sapply(strsplit(xcn, "[.]"), function(x) x[1])
	      cn = unique(cn)
	      tab = matrix(NA, nrow = 0, ncol = length(cn))
	      tab = data.frame(tab)
	      colnames(tab) = cn
	    } else {
	      tab = reshape(
	        data = tab, direction = "long",
	        idvar = "repo", varying = varying,
	        times = c("current", "stable"), timevar = "release")
	    }
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
# } else {
#   require("neurocInstall")
#   pkgs = devtools::session_info()
#   pkgs = pkgs$packages
#   pkg_ver = pkgs$version[ pkgs$package %in% "neurocInstall"]
#   message(paste0("Using neurocInstall version: ", pkg_ver,
#           ", using neurocInstall::neurocLite for installation.")
#   )
# }
