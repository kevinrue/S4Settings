#' IncrementalReadOnlySettings class
#'
#' @return An object of class `IncrementalReadOnlySettings`.
#' @export
#'
#' @examples
#' IncrementalReadOnlySettings(
#'   a = 1,
#'   b = "hello",
#'   c = TRUE,
#'   d = 5L
#' )
setClass("IncrementalReadOnlySettings",
  contains = "list"
)

#' @export
#' @rdname IncrementalReadOnlySettings-class
IncrementalReadOnlySettings <- function(...) {
  slots_list <- list()

  args_list <- list(...)
  for (name in names(args_list)) {
    if (name %in% names(slots_list)) {
      stop(sprintf("Value %s already set. Cannot be overriden in class %s", dQuote(name), dQuote("IncrementalReadOnlySettings")))
    }
    slots_list[[name]] <- args_list[[name]]
  }

  new("IncrementalReadOnlySettings", slots_list)
}

setMethod("show", "IncrementalReadOnlySettings", function(object) {
  cat("class:", class(object), "\n")
  l <- object@.Data
  names(l) <- names(object)
  print(l)
})
