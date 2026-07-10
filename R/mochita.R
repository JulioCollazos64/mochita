mochita <- function() {
  Mochita$new()
}

Mochita <- R6::R6Class(
  "Mochita",
  public = list(
    initialize = function() {
      for (method in httpMethods) {
        f <- function(path) {}
        body(f) <- substitute(
          {
            stopifnot(is.character(path))

            private$path <- path
            private$method <- method

            invisible(self)
          },
          env = list(
            method = quote(method)
          )
        )

        self[[method]] <- f
      }
    },
    getPrivate = function() {
      private
    }
  ),
  private = list(
    path = character(0),
    method = character(0)
  ),
  lock_objects = FALSE,
  cloneable = FALSE
)
