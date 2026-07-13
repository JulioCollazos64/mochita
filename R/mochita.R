#' Test an HTTP server
#'
#' `mochita()` builds the specification for testing an `httpuv` app.
#' It returns a `Mochita` object whose methods can be chained together
#' to build up a request, describe the expectations it
#' should meet, and finally send it with `$perform()`.
#'
#' @param app App definition, see [httpuv::startServer].
#'
#' @returns A `Mochita` object with the following chainable methods:
#' \describe{
#'   \item{`$get(path)`, `$head(path)`, `$post(path)`, `$put(path)`,
#'     `$delete(path)`, `$connect(path)`, `$options(path)`, `$trace(path)`,
#'     `$patch(path)`}{Set the HTTP method and path of the request.}
#'   \item{`$set(name, value)`}{Set a request header.}
#'   \item{`$expect()`}{Add an expectation to check against the
#'     response, once performed. Accepts a status code (optionally with a
#'     body to match), a header name/value pair, a body to match, or a
#'     custom function taking the response.}
#'   \item{`$perform()`}{Start the app, send the request, and run the
#'     recorded expectations against the response.}
#' }
#'
#' @examples
#' \dontrun{
#' mochita(app)$get("/")$
#'   set("Accept", "text/plain")$
#'   expect(200)$
#'   expect("Content-Type", "text/plain")$
#'   perform()
#' }
#'
#' @export
mochita <- function(app) {
  Mochita$new(app)
}

Mochita <- R6::R6Class(
  "Mochita",
  public = list(
    initialize = function(app) {
      private$app <- app

      for (method in httpMethods) {
        f <- function(path) {}
        body(f) <- substitute(
          {
            stopifnot(is.character(path))

            test <- Test$new(METHOD, path, private$app)

            invisible(test)
          },
          env = list(
            METHOD = method
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
    app = NULL
  ),
  lock_objects = FALSE,
  cloneable = FALSE
)

#' @keywords internal
#' @noRd
ignore_unused_imports <- function() {
  R6::R6Class
  httpuv::startServer
  nanonext::ncurl_aio
  testthat::expect_identical
}
