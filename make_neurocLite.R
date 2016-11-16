version = read.dcf("DESCRIPTION")[, "Version"]
top = readLines("neuro_top.R")
content = readLines("R/neuro_install.R")
version = paste0("# neurocInstall package version: ", version)
out = c(version, top, content)
content = readLines("R/neuro_package_table.R")
out = c(out, "", content)
writeLines(text = out, con = "neurocLite.R")
