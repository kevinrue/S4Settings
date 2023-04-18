#' IncrementalReadOnlySettings class
#'
#' Incremental read-only settings can be set and extended, but never edited.
#'
#' @param x An object of class `IncrementalReadOnlySettings`.
#' @param name Name of setting being assigned.
#' @param value Value of setting being assigned.
#'
#' @seealso [IncrementalReadOnlySettings()]
#'
#' @section Methods:
#' `$<-` can be used to assign a read-only value to a name that does not exist yet.
#'
#' @importClassesFrom S4Vectors SimpleList
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
    contains = "SimpleList"
)

#' Constructor for class IncrementalReadOnlySettings
#'
#' @param ... Named values to set.
#'
#' @return An object of class [`IncrementalReadOnlySettings-class`].
#' @export
#' @importFrom methods new
#' @importFrom S4Vectors SimpleList
#'
#' @examples
#' IncrementalReadOnlySettings(
#'   a = 1,
#'   b = "hello",
#'   c = TRUE,
#'   d = 5L
#' )
IncrementalReadOnlySettings <- function(...) {
    slots_list <- SimpleList()

    args_list <- list(...)
    for (arg_name in names(args_list)) {
        arg_value <- args_list[[arg_name]]
        slots_list <- .add_setting(slots_list, arg_name, arg_value)
    }

    new("IncrementalReadOnlySettings", slots_list)
}

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
