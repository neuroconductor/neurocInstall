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
    release_version = paste0(
   "http", ifelse(secure, "s", ""), "://neuroconductor.org/releases/",
    release_version)
}
