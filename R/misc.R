.onAttach <- function(libname, pkgname) {
  msg <- paste0("package '", pkgname,
                "' (version ", utils::packageVersion(pkgname), ")",
                " is loaded",
                "\nmore info at https://github.com/marieflutre/UrbanTempo")
  packageStartupMessage(msg)
}
