#' @title Print Method for \code{objects.summary} Objects
#' @description Print an object of class \code{objects.summary} with some specific formatting
#' options before using \code{print.data.frame}.
#' @name print.objects.summary
#' @param x object of class \code{objects.summary}
#' @param ... further arguments to be passed down to \code{print.data.frame} (should not include \code{max})
#' @param data.class.width integer indicating the width of the \code{data.class} field when
#' it is a list (when \code{objects.summary} was called with \code{all.classes} = \code{TRUE}).
#' Default: getOption("osum.data.class.width", default = NULL)
#' @param format.extent logical indicating whether the \code{extent} field should be formatted as
#' product (d1 x d2) or left as list (d1, d2). Default: getOption("osum.format.extent", default = TRUE)
#' @param max.rows integer, maximal number of rows to print. Default: getOption("osum.max.rows", default = NULL)
#' @return No return value, called for side effects.
#' @details \code{max.rows} computes an adequate value to be passed as \code{max} argument to \code{print.data.frame}.
#' By default, when NULL, getOption("max.print") is used by \code{print.data.frame}.
#' @seealso \code{\link{objects.summary}}, \code{\link{print.data.frame}}
#' @examples
#' print(objects.summary("package:datasets", all.classes = FALSE),
#'     format.extent = FALSE, max.rows = 6)
#' print(objects.summary("package:datasets", all.classes = TRUE, data.class = "array"),
#'     data.class.width = 22, format.extent = TRUE, max.rows = 6)
#' @method print objects.summary
#' @aliases print.objects.summary
#' @export
print.objects.summary <- function(x, ..., data.class.width = getOption("osum.data.class.width", default = NULL),
                                  format.extent = getOption("osum.format.extent", default = TRUE),
                                  max.rows = getOption("osum.max.rows", default = NULL)) {
    if (!is.na(match("data.class", names(x), nomatch = NA))) {
        if (is.list(x$data.class)) {
            x$data.class <- format.AsIs(x$data.class, width = data.class.width)
        }
    }
    if (!is.na(match("extent", names(x), nomatch = NA))) {
        if (isTRUE(format.extent)) {
            x$extent <- factor(sapply(x$extent, FUN = function(x) paste(x, collapse = " x ")))
        } else {
            x$extent <- factor(format(x$extent))
        }
    }
    dots = list(...)
    if (!is.null(max.rows)) {
        if (max.rows < 0L) {
            dots$max <- prod(dim(x))
        } else {
            dots$max <- max.rows * ncol(x)
        }
    }
    do.call(print.data.frame, c(list(x), dots))
}


# @usage
# ## S3 method for class 'objects.summary'
# \\method{print}{objects.summary}(x, ..., data.class.width, format.extent, max.rows)
