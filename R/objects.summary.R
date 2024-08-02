#' @title Summary Information About R Objects
#'
#' @description
#' Returns data class, storage mode, mode, typeof, dimensions, and size information for R objects from the
#' specified environment.  When invoked with no argument at the top-level prompt,
#' \code{objects.summary} shows what data sets and functions a user has defined in the current session.
#' When invoked with no argument inside a function, \code{objects.summary} returns the information
#' for the function's local variables: this can be useful in conjunction with \code{\link{browser}}.
#'
#' @usage objects.summary(where, all.objects = FALSE, pattern, names. = NULL,
#'     what = getOption("osum.information", default = c("data.class", "storage.mode",
#'         "mode", "typeof", "extent", "object.size")),
#'     all.classes = FALSE, data.class. = NULL, storage.mode. = NULL, mode. = NULL,
#'     typeof. = NULL, filter., order., reverse = FALSE)
#' @param where which environment to use in listing the available objects.
#' Defaults to the \emph{current} environment. This argument can specify the environment
#' in any form; see the \sQuote{Details} section.
#' @param all.objects a logical value. If \code{TRUE}, information for all objects is
#' returned. If \code{FALSE}, the information about objects whose names begin with a \samp{.}
#' is omitted. Default: FALSE
#' @param pattern an optional \link{regular expression}.  Only names matching
#' \code{pattern} are returned.  \code{\link{glob2rx}} can be used to convert
#' wildcard patterns to regular expressions.
#' @param names. an optional character vector naming objects to summarize. Default: NULL
#' @param what character vector specifying what information to return. This can be any subset of
#' \code{c("data.class", "storage.mode", "mode", "typeof", "extent", "object.size")}, in any order.
#' The default is to return all six types of information, in the order shown.
#' \code{what} is subject to partial matching, that is, only enough initial letters of each
#' string element are needed to guarantee unique recognition.
#' @param all.classes logical flag specifying whether the entire class vector of an object
#' or just the first element should be used, both in selection based on argument \code{data.class}
#' and in the returned summary. This has bearing only on objects with a class attribute.
#' By default only the first class element is used.
#' @param data.class. character vector of data classes (see \code{\link{data.class}}). Selects objects belonging to one of the named data classes.
#' If \code{all.classes=TRUE}, each element of an object's class attribute is considered, not just the first.
#' @param storage.mode. character vector of storage modes (see \code{\link{storage.mode}}). Selects objects with one of the named storage modes.
#' @param mode. character vector of modes (see \code{\link{mode}}). Selects objects with one of the named modes.
#' @param typeof. character vector of types (see \code{\link{typeof}}). Selects objects with one of the named types.
#' @param filter. logical expression indicating elements (rows) to keep: missing values are taken as false.
#' Note that the expression will be evaluated in the data frame with object attributes, so
#' columns should be referred to (by name) as variables in the expression (see the examples).
#' This argument is crafted after the \code{select} argument of the base \code{subset} function.
#' @param order. expression involving (unquoted) attributes names, controlling the sort order
#' of the object entries (printed as rows) in the summary.
#' For example, \code{order=object.size} means sort the objects on the increasing values of
#' the \code{object.size} component of the summary.
#' \code{order=c(data.class, -object.size)} means sort the objects alphabetically by \code{data.class},
#' and then the decreasing values of the \code{object.size}.
#' If \code{order} is omitted, the entries are sorted alphabetically by object name.
#' This argument is crafted after the arguments to the base \code{order} function.
#'
#'
#' The attribute names to be used in \code{filter} and \code{order} expressions must be
#' fully specified (no partial matching possible).
#'
#' @param reverse logical flag: if \code{TRUE}, the final sort order is reversed, even if
#' this order depends on object names (this is different from the original S-PLUS function). Default: \code{FALSE}
#'
#' @details
#' The \code{where} argument can specify the environment from which object names
#' are taken in one of several forms: as an integer (the position in the
#' \code{\link{search}} list); as the character string name of an element in
#' the search list; or as an explicit \code{\link{environment}} (including
#' using \code{\link{sys.frame}} to access the currently active function
#' calls). By default, the environment of the call to \code{objects.summary}
#' is used.
#'
#' Unless an explicit environment is provided, the \code{where} argument should
#' designate an element of the search list. However, if it is a character of the form
#' "package:pkg_name" and if the package named "pkg_name" is installed, it is silently
#' loaded, its objects retrieved, and then it is unloaded when the function exits.
#' Depending on the time it takes to load the package, the execution might be slower than
#' getting the information about an attached package.
#'
#' It is possible to use the attributes that are not returned (not listed in \code{what})
#' in the \code{filter} and \code{order} expressions.
#'
#' @return An object of (S3) class \code{"objects.summary"}, which inherits from class \code{"data.frame"}.
#' Its components (printed as columns) are those specified in argument \code{what}. Each component
#' contains one type of information for all selected objects. They are at most the following:
#' \itemize{
#'  \item{\code{data.class}}{ - a factor (if \code{all.classes=FALSE)}, or a list of character
#'  vectors (if \code{all.classes=TRUE)} containing the data class information. This is defined
#'  as in the function \code{\link{data.class}}, with the exception that when \code{all.classes=TRUE}, the summary
#'  will contain the entire class attribute for each object which has one, whereas function \code{data.class}
#'  returns only the first element of this vector.}
#'  \item{\code{storage.mode}}{ - a factor giving the storage mode information, as returned by function \code{\link{storage.mode}}.}
#'  \item{\code{mode}}{ - a factor giving the mode information, as returned by function \code{\link{mode}}.}
#'  \item{\code{typeof}}{ - a factor giving the R internal type or storage mode information, as returned by function \code{\link{typeof}}.}
#'  \item{\code{extent}}{ -  a list, each of whose components is a numeric vector giving the dimension of an object, or its length if it is dimensionless.}
#'  \item{\code{object.size}}{ - a numeric vector giving the object sizes in bytes, as returned by function \code{\link{object.size}}.}
#' }
#'
#' The purpose of the dedicated class \code{objects.summary} is only to provide customized
#' \code{print} and \code{summary} methods.
#'
#' @seealso \code{\link{glob2rx}} for converting wildcard patterns to regular
#' expressions;
#' \code{\link{ls.str}} for a long listing based on \code{\link{str}};
#' \code{\link{apropos}} (or \code{\link{find}}) for finding objects in the
#' whole search path; \code{\link{grep}} for more details on \sQuote{regular
#' expressions}; \code{\link{subset}} for filtering; \code{\link{order}} for sorting;
#' \code{\link{class}}, \code{\link{data.class}}, \code{\link{methods}}, etc., for
#' object-oriented programming.
#' @references TIBCO Spotfire S+® 8.2 Function Guide, November 2010, TIBCO Software Inc.
#' @encoding UTF-8
#' @examples
#' if (interactive()) {
#'     .Ob <- 1
#'     a <- letters[1:5]
#'     x <- rnorm(20)
#'     i <- 1:10
#'     l <- list(a = a, i = i, x = x)
#'     df <- iris
#'     arr <- iris3
#'     myfunc <- function() {ls()}
#'     objects.summary()
#'     objects.summary(pattern = "O")
#'     objects.summary(pattern = "O", all.objects = TRUE)
#'
#'     objects.summary(mode = "function")
#'     objects.summary("package:grDevices", filter = mode != "function")
#'     objects.summary("package:datasets", all.classes = TRUE,
#'         filter = sapply(data.class, length) > 1)
#'     # shows an empty list because inside myfunc no variables are defined
#'     myfunc <- function() {objects.summary()}
#'     myfunc()
#'
#'     # define a local variable inside myfunc
#'     myfunc <- function() {y <- 1; objects.summary()}
#'     myfunc()                # shows "y"
#' }
#'
#' @export
#' @keywords environment misc utilities
objects.summary <- function(where, all.objects = FALSE, pattern, names. = NULL,
                            what = getOption("osum.information", default = c("data.class", "storage.mode", "mode", "typeof", "extent", "object.size")),
                            all.classes = FALSE, data.class. = NULL, storage.mode. = NULL,
                            mode. = NULL, typeof. = NULL, filter., order., reverse = FALSE) {
    # what information to return
    all.fields <- c("data.class", "storage.mode", "mode", "typeof", "extent", "object.size")
    which.info <- unique(pmatch(what, all.fields, duplicates.ok = TRUE))
    if (any(is.na(which.info))) {
        msg <- paste0("Elements of `what' must (partially) match ",
                      deparse(all.fields, width.cutoff = 500), ", with no duplicates.")
        stop(msg)
    }
    what <- all.fields[which.info]
    # filter expression
    if (!missing(filter.)) {
        filter.expr <- substitute(filter.)
        filter.vars <- all.vars(filter.expr)
    } else {
        filter.vars <- NULL
    }
    # order expression
    if (!missing(order.)) {
        order.expr <- substitute(order.)
        order.vars <- all.vars(order.expr)
    } else {
        order.vars <- NULL
    }
    # prepare 0-rows output
    empty.out <- data.frame(
        data.class = character(0),
        storage.mode = character(0),
        mode = character(0),
        typeof = character(0),
        extent = character(0),
        object.size = integer(0))[, what]
    # determine which environment to return information about
    pos <- -1L
    if (!missing(where)) {
        pos <- tryCatch(where, error = function(e) e)
        if (inherits(pos, "error")) {
            name <- substitute(name)
            if (!is.character(name))
                name <- deparse(name)
            warning(gettextf("%s converted to character string",
                             sQuote(name)), domain = NA)
            pos <- name
        }
    }
    if (is.character(pos) && (substring(pos, 1, 8) == "package:")) {
        if (is.na(match(pos, search()))) {
            pkgName <- sub("package:", "", pos)
            ans <- try(suppressPackageStartupMessages(library(pkgName, character.only = TRUE)), silent = TRUE)
            if (inherits(ans, "try-error")) {
                msg <- sprintf("unable to attach package '%s'", pkgName)
                stop(msg)
            }
            on.exit(detach(pos, character.only = TRUE))
        }
    }
    envir <- as.environment(pos)
    # get list of objects from specified environment
    # don't need sorted as will order by name if no other order is specified
    # apply pattern filter if provided
    if (!missing(pattern)) {
        all.names <- ls(envir, all.names = all.objects, pattern = pattern, sorted = FALSE)
    } else {
        all.names <- ls(envir, all.names = all.objects, sorted = FALSE)
    }
    # apply explicit names
    explicit.names <- !is.null(names.)
    if (explicit.names) {
        lst <- intersect(names., all.names)
        missing <- setdiff(names., all.names)
        if (length(missing) > 0L) {
            msg <- sprintf("%d name%s not found: %s", length(missing),
                           ifelse(length(missing) > 1L, "s", ""),
                           paste(missing, collapse = ", "))
            warning(msg)
        }
    } else {
            lst <- all.names
    }
    # check that the list of objects to return information about is not empty
    if (length(lst) == 0L) {
        return(empty.out)
    }
    # check selection criteria, if any
    select.cl <- !explicit.names && !is.null(data.class.)
    select.sm <- !explicit.names && !is.null(storage.mode.)
    select.mode <- !explicit.names && !is.null(mode.)
    select.type <- !explicit.names && !is.null(typeof.)
    # check which information must be computed, either because it is explicitly required
    # or used for filtering and/or sorting
    do.class <- any(which.info == 1) || select.cl || !is.na(match("data.class", filter.vars)) || !is.na(match("data.class", order.vars))
    do.smode <- any(which.info == 2) || select.sm  || !is.na(match("storage.mode", filter.vars)) || !is.na(match("storage.mode", order.vars))
    do.mode <- any(which.info == 3) || select.mode || !is.na(match("mode", filter.vars)) || !is.na(match("mode", order.vars))
    do.type <- any(which.info == 4) || select.type || !is.na(match("typeof", filter.vars)) || !is.na(match("typeof", order.vars))
    do.extent <- any(which.info == 5) || !is.na(match("extent", filter.vars)) || !is.na(match("extent", order.vars))
    do.size <- any(which.info == 6) || !is.na(match("object.size", filter.vars)) || !is.na(match("object.size", order.vars))
    # get the information potentially used for filtering/sorting
    N <- length(lst)
    if (do.class) {
        cl <- if (isTRUE(all.classes)) {
            lapply(lst, FUN = function(x) class(get(x, envir = envir)))
        } else {
            sapply(lst, FUN = function(x)  data.class(get(x, envir = envir)))
        }
    } else {
        cl <- (rep(NA_character_, N))
    }
    if (do.smode) {
        sm <- (sapply(lst, FUN = function(x) storage.mode(get(x, envir = envir))))
    } else {
        sm <- (rep(NA_character_, N))
    }
    if (do.mode) {
        mode <- (sapply(lst, FUN = function(x) mode(get(x, envir = envir))))
    } else {
        mode <- (rep(NA_character_, N))
    }
    if (do.type) {
        type <- (sapply(lst, FUN = function(x) typeof(get(x, envir = envir))))
    } else {
        type <- (rep(NA_character_, N))
    }
    if (do.extent) {
        len <- sapply(lst, FUN = function(x) length(get(x, envir = envir)))
        shape <- lapply(lst, FUN = function(x) {
            d <- dim(get(x, envir = envir))
            if (is.null(d)) NA else d
        })
        shape[is.na(shape)] <- len[is.na(shape)]
    } else {
        shape <- (rep(NA_integer_, N))
    }
    if (do.size) {
        size <- sapply(lst, FUN = function(x) utils::object.size(get(x, envir = envir)))
    } else {
        size <- rep(NA, N)
    }
    # apply filters, if any
    selected <- rep(TRUE, N)
    if (select.cl) {
        ndx <- sapply(cl, FUN = function(x) any(match(x, data.class., nomatch = 0)))
        selected <- selected & ndx
    }
    if (select.sm) {
        ndx <- as.logical(match(sm, storage.mode., nomatch = 0))
        selected <- selected & ndx
    }
    if (select.mode) {
        ndx <- as.logical(match(mode, mode., nomatch = 0))
        selected <- selected & ndx
    }
    if (select.type) {
        ndx <- as.logical(match(type, typeof., nomatch = 0))
        selected <- selected & ndx
    }
    if (!missing(filter.)) {
        df <- data.frame(
            data.class = if (isTRUE(all.classes)) I(cl) else as.character(cl),
            storage.mode = sm,
            mode = mode,
            typeof = type,
            extent = I(shape),
            object.size = size)
        filter.expr <- substitute(filter.)
        filter.rows <- eval(filter.expr, df, parent.frame())
        if (!is.logical(filter.rows))
            stop("'filter.' must be logical")
        ndx <- filter.rows & !is.na(filter.rows)
        selected <- selected & ndx
    }
    lst <- lst[selected]
    N <- length(lst)
    # check that the list of objects to return information about is not empty after filtering
    if (N == 0L) {
        return(empty.out)
    } else {
        # filter the information
        cl <- cl[selected]
        sm <- sm[selected]
        mode <- mode[selected]
        type <- type[selected]
        shape <- shape[selected]
        size <- size[selected]
    }
    # order the result
    if (missing(order.)) {
        # order on names if no other order specified
        ord <- order(lst)
    } else {
        df <- data.frame(
            data.class = if (isTRUE(all.classes)) I(cl) else as.character(cl),
            storage.mode = sm,
            mode = mode,
            typeof = type,
            extent = I(shape),
            object.size = size)
        # Capture the expression for the order criteria
        order.expr <- substitute(order.)
        if (length(order.expr) == 1L) order.expr <- substitute(c(order.))
        # Create a list of evaluated expressions within the context of the data frame
        order.eval <- lapply(order.expr[-1], function(x) eval(x, df, parent.frame()))
        # Use the order function to get the sorted indices
        ord <- do.call(order, order.eval)
    }
    if (reverse) ord <- rev(ord)
    lst <- lst[ord]
    eval(expression(cl <- cl[ord], sm <- sm[ord], mode <- mode[ord], type <- type[ord],
            shape <- shape[ord], size <- size[ord])[which.info])
    # build the final data.frame
    out <- data.frame(
        data.class = if (isTRUE(all.classes)) I(cl) else factor(cl),
        storage.mode = factor(sm),
        mode = factor(mode),
        typeof = factor(type),
        extent = I(shape),
        object.size = size,
        row.names = lst)[, which.info, drop = FALSE]
    # apply specific class, used for printing and summary
    class(out) <- c("objects.summary", class(out))
    # all done
    return(out)
}
