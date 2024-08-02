.onLoad <- function(libname, pkgname) {
    options(osum.data.class.width = NULL,
            osum.format.extent = TRUE,
            osum.information = c("data.class", "storage.mode", "mode", "typeof", "extent", "object.size"),
            osum.max.rows = NULL)
}
