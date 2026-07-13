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
    send = function(body) {
      private$body <- body
      invisible(self)
    },
    expect = function(a, b = NULL) {
      # status
      if (is.numeric(a)) {
        status <- a
        if (is.null(b)) {
          test <- function(res) {
            testthat::expect_equal(res$status, status)
          }
        } else {
          body <- b
          test <- function(res) {
            testthat::expect_equal(res$status, status)
            testthat::expect_identical(res$body, body)
          }
        }
        private$tests <- append(private$tests, test)
        return(invisible(self))
      }

      if (is.function(a)) {
        private$tests <- append(private$tests, a)
        return(invisible(self))
      }

      # header
      if (is.character(b)) {
        name <- a
        value <- b

        test <- function(res) {
          testthat::expect_identical(
            res$headers[[name]],
            value
          )
        }

        private$tests <- append(private$tests, test)
        return(invisible(self))
      }

      # body

      test <- function(res) {
        testthat::expect_identical(
          res$body,
          a
        )
      }
      private$tests <- append(private$tests, test)
      invisible(self)
    },
    perform = function() {
      server <- httpuv::startServer(
        host = "127.0.0.1",
        port = httpuv::randomPort(),
        app = private$app
      )

      on.exit(
        {
          httpuv::stopServer(server)
        },
        add = TRUE
      )

      url <- paste0(
        "http://",
        server$getHost(),
        ":",
        server$getPort(),
        private$path
      )

      # From: https://github.com/r-lib/nanonext/blob/main/README.md

      aio <- nanonext::ncurl_aio(
        url,
        method = toupper(private$method),
        headers = private$headers,
        response = TRUE,
        data = private$body
      )

      while (nanonext::unresolved(aio)) {
        nanonext::run_event_loop(1000)
      }

      res <- list(
        status = aio$status,
        headers = aio$headers,
        body = aio$data
      )

      private$performTests(private$tests, res)
    },
    getPrivate = function() {
      private
    }
  ),
  private = list(
    path = character(0),
    method = character(0),
    headers = list(),
    body = NULL,
    tests = list(),
    app = NULL,
    performTests = function(tests, response) {
      test_that("$perform results", {
        for (test in tests) {
          test(response)
        }
      })
    }
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
