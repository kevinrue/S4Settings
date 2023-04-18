#' IncrementalReadOnlySettings class
#'
#' Incremental read-only settings can be set and extended, but never edited.
#'
#' @seealso [IncrementalReadOnlySettings()]
#'
#' @section Methods:
#' `$<-` can be used to assign a read-only value to a name that does not exist yet.
#'
#' @docType class
#' @examples
#' settings <- IncrementalReadOnlySettings(
#'   a = 1,
#'   b = "hello",
#'   c = TRUE,
#'   d = 5L
#' )
#'
#' settings
#'
#' settings$e <- 2+3i
#' settings
#'
#' \dontrun{
#' ## existing settings cannot be edited
#' settings$e <- 2-3i
#' }
setClass("IncrementalReadOnlySettings",
    contains = "list"
)

#' Constructor for class IncrementalReadOnlySettings
#'
#' @param ... Named values to set.
#'
#' @return An object of class [`IncrementalReadOnlySettings-class`].
#' @export
#' @importFrom methods new
#'
#' @examples
#' IncrementalReadOnlySettings(
#'   a = 1,
#'   b = "hello",
#'   c = TRUE,
#'   d = 5L
#' )
IncrementalReadOnlySettings <- function(...) {
    slots_list <- list()

    args_list <- list(...)
    for (arg_name in names(args_list)) {
        arg_value <- args_list[[arg_name]]
        slots_list <- .add_setting(slots_list, arg_name, arg_value)
    }

    new("IncrementalReadOnlySettings", slots_list)
}

setMethod("show", "IncrementalReadOnlySettings", function(object) {
    cat("class:", class(object), "\n")
    l <- object@.Data
    names(l) <- names(object)
    print(l)
})

#' @rdname IncrementalReadOnlySettings-class
#' @aliases $<-,IncrementalReadOnlySettings-method
setReplaceMethod("$", "IncrementalReadOnlySettings", function(x, name, value) {
    x <- .add_setting(x, name, value)
    x
})

.add_setting <- function(x, name, value) {
    if (name %in% names(x)) {
        stop(sprintf("Value %s already set. Cannot be overriden in class %s", dQuote(name), dQuote("IncrementalReadOnlySettings")))
    }
    x[[name]] <- value
    x
}
