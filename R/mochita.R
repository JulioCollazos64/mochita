mochita <- function(app) {
  Mochita$new(app)
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
            private$method <- METHOD

            invisible(self)
          },
          env = list(
            METHOD = method
          )
        )

        self[[method]] <- f
      }
    },
    set = function(name, value) {
      private$headers[[name]] <- value
      invisible(self)
    },
    expect = function(...) {
      args <- list(...)

      stopifnot(length(args) <= 2)

      if (length(args) == 1) {
        stopifnot(is.numeric(..1))
        status <- ..1
        test <- function(res) {
          testthat::expect_equal(res$status, status)
        }
        private$tests <- append(private$tests, test)
        return(invisible(self))
      }

      name <- ..1
      value <- ..2

      stopifnot(is.character(name), is.character(value))

      private$responseHeaders <- c(private$responseHeaders, name)

      test <- function(res) {
        testthat::expect_identical(
          res$headers[[name]],
          value
        )
      }

      private$tests <- append(private$tests, test)

      invisible(self)
    },
    getPrivate = function() {
      private
    }
  ),
  private = list(
    path = character(0),
    method = character(0),
    headers = list(),
    tests = list(),
    responseHeaders = character(0)
  ),
  lock_objects = FALSE,
  cloneable = FALSE
)
