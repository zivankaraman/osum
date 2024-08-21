#' @title Options Settings for Package \code{osum}
#'
#' @description This function enables users to customize and review specific \emph{options}
#' for the \code{osum} package.
#'
#' @param osum.data.class.width integer indicating the width of the \code{data.class} field when
#' it is a list (when \code{objects.summary} was called with \code{all.classes} = \code{TRUE}).
#' @param osum.format.extent logical indicating whether the \code{extent} field should be formatted as
#' product (d1 x d2) or left as list (d1, d2). Default: TRUE
#' @param osum.information character vector specifying what information to return by default. This can be any subset of
#' \code{c("data.class", "storage.mode", "mode", "typeof", "extent", "object.size")}, in any order.
#' The default is to return all six types of information, in the order shown.
#' \code{osum.information} is subject to partial matching, that is, only enough initial letters of each
#' string element are needed to guarantee unique recognition.
#' @param osum.max.rows integer, maximal number of rows to print.
#' @return For \code{osum.options()}, a list of all the \code{osum} package options sorted by name.
#' For \code{osum.options(name)}, a list of length one containing the set value, or
#' \code{NULL} if it is unset.  For uses setting one or more options, a list
#' with the previous values of the options changed (returned invisibly).
#' @seealso \code{\link{options}} for global \emph{options},
#' \code{\link{print.objects.summary}} and \code{\link{summary.objects.summary}} for
#' using specific \code{osum} package options.
#' @details Invoking \code{osum.options()} with no arguments returns a list with the current
#' values of the \code{osum} package options.
#' To access the value of a single option, one can pass a character string with its
#' name name as argument, e.g. \code{osum.options("osum.max.rows")}, which will return a named
#' \emph{list} of length one with the option's value.
#' @examples
#' old_opt <- osum.options(osum.data.class.width = 12, osum.max.rows = 25)
#' cat("current values of all 'osum' options:", sep = "\n")
#' print(osum.options())
#' cat("previous values of the changed 'osum' options:", sep = "\n")
#' print(old_opt)
#' @export
osum.options <- function(osum.data.class.width, osum.format.extent, osum.information, osum.max.rows) {
    opt <- options()
    old_opt <- opt[grep("^osum\\.", names(opt))]
    passed_args <- names(as.list(match.call())[-1])
    if (length(passed_args) == 0L) {
        return(old_opt)
    } else {
        if (!missing(osum.data.class.width)) {
            if (is.character(osum.data.class.width)) {
                out <- old_opt[osum.data.class.width]
                if (any(is.na(names(out)))) {
                    bad <- is.na(names(out))
                    msg <- sprintf("unkown option%s %s",
                                   ifelse(length(bad) > 1L, "s", ""),
                                   paste(osum.data.class.width[bad], collapse = ", "))
                    stop(msg)
                }
                return(out)
            } else {
                osum.data.class.width <- as.integer(osum.data.class.width)
                if ((length(osum.data.class.width) > 1L) || is.na(osum.data.class.width)) stop("osum.data.class.width must be an integer value")
                options(osum.data.class.width = osum.data.class.width)
            }
        }
        if (!missing(osum.format.extent)) {
            osum.format.extent <- as.logical(osum.format.extent)
        if ((length(osum.format.extent) > 1L) || is.na(osum.format.extent)) stop("osum.format.extent must be a logical value")
            options(osum.format.extent = osum.format.extent)
        }
        if (!missing(osum.information)) {
            osum.information <- as.character(osum.information)
            if (any(is.na(osum.information))) stop("osum.information must be a character vector")
            all.fields <- c("data.class", "storage.mode", "mode", "typeof", "extent", "object.size")
            which.info <- unique(pmatch(osum.information, all.fields, duplicates.ok = FALSE))
            if (any(is.na(which.info))) {
                msg <- paste0("Elements of `osum.information' must (partially) match ",
                              deparse(all.fields, width.cutoff = 500), ", with no duplicates.")
                stop(msg)
            }
            osum.information <- all.fields[which.info]
            options(osum.information = osum.information)
        }
        if (!missing(osum.max.rows)) {
            osum.max.rows <- as.integer(osum.max.rows)
            if ((length(osum.max.rows) > 1L) || is.na(osum.max.rows)) stop("osum.max.rows must be an integer value")
            options(osum.max.rows = osum.max.rows)
        }
    }
    out <- old_opt[passed_args]
    names(out) <- passed_args
    invisible(out)
}
