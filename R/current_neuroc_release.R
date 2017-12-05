#' Current Neuroconductor Release Location
#'
#' @param secure Should https vs. http be used
#' @return URL fo release page
#' @export
#'
#' @examples
#' current_neuroc_release()
current_neuroc_release = function(secure = TRUE) {
  release_version = "2017/nov/"
  release_version = paste0(
   "http", ifelse(secure, "s", ""), "://neuroconductor.org/releases/",
    release_version)
}
