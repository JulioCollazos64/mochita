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
        response = private$responseHeaders
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
    tests = list(),
    responseHeaders = character(0),
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
