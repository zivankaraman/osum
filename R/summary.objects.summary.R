#' @title Summary Method for \code{objects.summary} Objects
#' @description Summarize an object of class \code{objects.summary} with some specific formatting
#' options before using \code{summary.data.frame}.
#' @name summary.objects.summary
#' @param object object of class \code{objects.summary}
#' @param ... further arguments to be passed down to \code{summary.data.frame}
#' @param data.class.width integer indicating the width of the \code{data.class} field when
#' it is a list (when \code{objects.summary} was called with \code{all.classes} = \code{TRUE}).
#' Default: getOption("osum.data.class.width", default = NULL)
#' @param format.extent logical indicating whether the \code{extent} field should be formatted as
#' product (d1 x d2) or left as list (d1, d2). Default: getOption("osum.format.extent", default = TRUE)
#' @return A matrix of class  "\code{\link{table}}", obtained by applying \code{\link{summary}}
#' to each column of the object (after applying the specific formatting according to the
#' arguments' values) and collating the results.
#' @seealso \code{\link{objects.summary}}, \code{\link{summary.data.frame}}, \code{\link{quantile}}
#' @examples
#' os <- objects.summary("package:datasets")
#' print(summary(os, format.extent = FALSE, maxsum = 10, quantile.type = 7))
#' print(summary(os, format.extent = TRUE, maxsum = 12, quantile.type = 1))
#' @method summary objects.summary
#' @aliases summary.objects.summary
#' @export
summary.objects.summary <- function(object, ..., data.class.width = getOption("osum.data.class.width", default = NULL),
                                    format.extent = getOption("osum.format.extent", default = TRUE)) {
    if (!is.na(match("data.class", names(object), nomatch = NA))) {
        if (is.list(object$data.class)) {
            # object$data.class <- factor(sapply(object$data.class, FUN = function(object) paste(object, collapse = ", ")))
            object$data.class <- factor(format.AsIs(object$data.class, width = data.class.width))
        }
    }
    if (!is.na(match("extent", names(object), nomatch = NA))) {
        if (isTRUE(format.extent)) {
            object$extent <- factor(sapply(object$extent, FUN = function(x) paste(x, collapse = " x ")))
        } else {
            object$extent <- factor(format(object$extent))
        }
    }
    summary.data.frame(object, ...)
}


# @usage
# ## S3 method for class 'objects.summary'
# \\method{summary}{objects.summary}(object, ..., data.class.width, format.extent)
