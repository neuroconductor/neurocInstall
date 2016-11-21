if (!require("neurocInstall")) {
  if (!require(devtools)) {
    install.packages("devtools")
  }
  message(paste("Using neurocLite version:", pkg_ver))
