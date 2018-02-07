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
    df = release_versions()
    release_path = df$release[1]
  }
  release_path = paste0(
    "http", ifelse(secure, "s", ""), "://neuroconductor.org/releases/",
    release_path)
  release_path
}



#' @rdname latest_neuroc_release
#' @importFrom utils download.file
#' @export
release_versions = function(secure = TRUE) {
  # read from the page Adi Makes
  # currently fail
  url = paste0("http", ifelse(secure, "s", ""),
               "://neuroconductor.org/api/releases/")
  destfile = tempfile(fileext = ".txt")
  x = download.file(url = url, destfile = destfile, quiet = TRUE)
  if (x != 0) {
    warning(paste0(
      "Releases did not download, may be error with downloading ",
      url))
  }
  releases = readLines(destfile)
  releases = sub("^releases/", "", releases)
  ss = t(sapply(strsplit(releases, "/"), rbind))
  colnames(ss) = c("year", "month")
  df = data.frame(release = releases, stringsAsFactors = FALSE)
  df = cbind(df, ss, stringsAsFactors = FALSE)
  df = df[ df$year != "latest", , drop = FALSE]
  df$year = as.numeric(df$year)
  df$date = paste0(df$year, "-", df$month, "-01")
  df$date = as.Date(x = df$date, format = "%Y-%b-%d")
  df = df[ order(df$date, decreasing = TRUE), , drop = FALSE]
  return(df)
}
