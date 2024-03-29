#' @rdname equal
#' @title Compare two objects
#' @aliases print.UNFtest
#' @description Function to compare the size, structure, arrangement, and UNFs of two objects.
#' @param x A vector, matrix, dataframe, list, or object of class \dQuote{UNF}, or a one-element character vector containing a UNF signature.
#' @param y A vector, matrix, dataframe, list, or object of class \dQuote{UNF}, or a one-element character vector containing a UNF signature.
#' @param \ldots Additional arguments passed to \code{\link{unf}}.
#' @details Compares two objects using \code{all.equal} and additional details based on the UNF of the two objects (and, for lists, dataframes, and matrices) the constituent vectors thereof. The print method for class \code{UNFtest} prints the UNFs for both objects and summarizes any differences between the objects. This is helpful for identifying mismatching variables.
#' @return An object of class \code{UNFtest} containing the results of \code{unf} for both objects and both \code{identical} and \code{all.equal} for the comparison of the two.
#' @author Thomas J. Leeper
#' @examples
#' a <- data.frame(x1=1:10, x2=11:20)
#' b <- data.frame(x1=1:10, x2=11:20+.0005)
#' a %unf% a
#' a %unf% b
#' unf_equal(a, b, digits = 3)
#' 
#' unf(a) %unf% "UNF6:aKW4lAFNBH8vfrnrDbQZjg=="
#'
#' @seealso \code{\link{unf}}
#' @export
`%unf%` <- function(x, y) unf_equal(x, y)

#' @rdname equal
#' @export
unf_equal <- function(x, y, ...) {
    ident <- identical(x, y)
    if (inherits(x, 'UNF')) {
        unfx <- x
        if (!inherits(y, 'UNF')) {
            unfy <- unf(y, version = attr(unfx, 'version'), ...)
        } else if (inherits(y, 'UNF') & (attr(x, 'version') != attr(y, 'version'))) {
            unfy <- unf(y, version = attr(unfx, 'version'), ...)
        } else {
            unfy <- y
        }
    } else if (inherits(y, 'UNF')) {
        unfy <- y
        if (!inherits(x, 'UNF')) {
            unfx <- unf(x, version = attr(unfy, 'version'), ...)
        } else if(inherits(x, 'UNF') & (attr(y, 'version') != attr(x, 'version'))) {
            unfx <- unf(x, version = attr(unfy, 'version'), ...)
        } else {
            unfx <- x
        }
    } else {
        unfx <- unf(x, ...)
        unfy <- unf(y, ...)
    }
    
    sorted <- NULL
    dimx <- dimy <- NULL
    x.rows <- y.rows <- NULL
    if (is.vector(x) | is.vector(y)) {
        if ((is.character(x) && length(x) == 1) || (is.character(y) && length(y) == 1)) {
            if (is.character(x) && length(x) == 1) {
                unfx <- structure(list(), class = "UNF")
                if (grepl("^UNF", x)) {
                    s <- strsplit(x, ":")[[1]]
                    unfx$unf <- s[length(s)]
                    unfx$formatted <- x
                } else {
                    unfx$unf <- x
                }
            }
            if (is.character(y) && length(y) == 1) {
                unfy <- structure(list(), class = "UNF")
                if (grepl("^UNF", y)) {
                    s <- strsplit(y, ":")[[1]]
                    unfy$unf <- s[length(s)]
                    unfy$formatted <- y
                } else {
                    unfy$unf <- y
                }
            }
            if (identical(unfx$unf, unfy$unf)) {
                ident <- TRUE
            }
            dimx <- NULL
            dimy <- NULL
            sorted <- NULL
        } else {
            if (is.vector(x) & !inherits(x, 'UNF')) {
                dimx <- c(length(x),1)
            }
            if (is.vector(y) & !inherits(y, 'UNF')) {
                dimy <- c(length(y),1)
            }
            if (is.vector(x) & is.vector(y) & (!is.list(x) & !is.list(y))) {
                sorted <- identical(sort(x),sort(y))
            }
        }
    } else if ((is.data.frame(x) & is.data.frame(y)) & (!inherits(x, 'UNF') & !inherits(y, 'UNF'))) {
        sorted <- identical(x[do.call(order, x), ,drop=FALSE], y[do.call(order, y), ,drop=FALSE])
        u <- sort(intersect(names(x),names(y)))
        if (!sorted & length(u)) {
            x.rows <- apply(x[,u,drop=FALSE], 1, function(i) digest(as.character(i)))
            y.rows <- apply(y[,u,drop=FALSE], 1, function(i) digest(as.character(i)))
        } else {
            x.rows <- y.rows <- NULL
        }
        dimx <- dim(x)
        dimy <- dim(y)
    }
    
    structure(list(identical = ident,
         unfmatch = (unfx$unf == unfy$unf),
         sorted = sorted,
         dim.x = dimx,
         dim.y = dimy,
         unf.x = unfx,
         unf.y = unfy,
         x.vars = if (!is.null(unfx$variables)) unfx$variables else NULL,
         y.vars = if (!is.null(unfy$variables)) unfy$variables else NULL,
         x.rows = x.rows,
         y.rows = y.rows,
         equal = all.equal(x, y)), class = "UNFtest")
}

#' @export
print.UNFtest <- function(x, ...){
    printxvars <- printyvars <- TRUE
    printxrows <- printyrows <- TRUE
    if (is.null(x$x.rows)) {
        printxrows <- FALSE
    }
    if (is.null(x$y.rows)) {
        printyrows <- FALSE
    }
    ax <- attributes(x$unf.x)
    ay <- attributes(x$unf.y)
    if (x$identical) {
        cat('Objects are identical\n\n')
        printxvars <- printyvars <- FALSE
        printxrows <- printyrows <- FALSE
    } else if ((!is.null(ax$version) & !is.null(ay$version)) && !ax$version==ay$version) {
        cat('Objects use different UNF versions\n\n')
        printxvars <- printyvars <- FALSE
        printxrows <- printyrows <- FALSE
    } else if (x$unfmatch) {
        a <- attributes(x$unf.x)
        cat('Objects are a UNF (v',
            paste(a$version,':',a$digits,',',a$characters,sep=''),
            ') match\n\n',sep='')
    } else if (!is.null(x$sorted) && x$sorted) {
        cat('Objects are identical but sorted\n\n')
        printxvars <- printyvars <- FALSE
        printxrows <- printyrows <- FALSE
    } else if (x$unf.x$unf %in% x$unf.y$variables) {
        cat('x is a variable from y\n\n')
        printxvars <- printyvars <- FALSE
        printxrows <- printyrows <- FALSE
    } else if (x$unf.y$unf %in% x$unf.x$variables) {
        cat('y is a variable from x\n\n')
        printxvars <- printyvars <- FALSE
        printxrows <- printyrows <- FALSE
    } else {
        cat('Objects are not identical\n\n')
        if ((!is.null(x$dim.x) && !is.null(x$dim.y)) && any(!x$dim.x==x$dim.y)){
            cat('x dimensions: ', x$dim.x, '\n\n')
            cat('y dimensions: ', x$dim.y, '\n\n')
        }
    }
    print(x$unf.x)
    if (printxvars) {
        misx <- x$x.vars[!x$x.vars %in% x$y.vars]
        if (length(misx)) {
            cat('Mismatched variables:\n')
            n <- ifelse(!is.na(names(misx)), names(head(misx,10)), head(which(!x$x.vars %in% x$y.vars), 10))
            cat(paste(n,head(misx,10),sep=': '), sep='\n')
            if (length(misx)>10) {
                cat('[',length(misx)-10,' additional mismatches not printed]\n',sep='')
            }
        }
    }
    if (printxrows) {
        misx <- which(!x$x.rows %in% x$y.rows)
        if(length(misx)){
            cat('Rows seemingly not in y:\n')
            print(misx)
        }
    }
    
    cat('\n')
    print(x$unf.y)
    if (printyvars) {
        misy <- x$y.vars[!x$y.vars %in% x$x.vars]
        if (length(misy)) {
            cat('Mismatched variables:\n')
            n <- ifelse(!is.na(names(misy)), names(head(misy,10)), head(which(!x$y.vars %in% x$x.vars), 10))
            cat(paste(n,head(misy,10),sep=': '), sep='\n')
            if (length(misy)>10) {
                cat('[',length(misy)-10,' additional mismatches not printed]\n',sep='')
            }
        }
    }
    if (printyrows) {
        misy <- which(!x$y.rows %in% x$x.rows)
        if (length(misy)) {
            cat('Rows seemingly not in x:\n')
            print(misy)
        }
    }
    invisible(x)
}
