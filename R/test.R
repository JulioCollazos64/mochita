Test <- R6::R6Class(
  "Test",
  public = list(
    initialize = function(method, path, app) {
      private$app <- app
      private$path <- path
      private$method <- toupper(method)
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
      on.exit(httpuv::stopServer(server), add = TRUE)

      url <- private$serverAddress(server, private$path)

      # From: https://github.com/r-lib/nanonext/blob/main/README.md

      aio <- nanonext::ncurl_aio(
        url,
        method = private$method,
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
    }
  ),
  private = list(
    app = NULL,
    path = character(0),
    method = character(0),
    headers = list(),
    body = NULL,
    tests = list(),
    serverAddress = function(server, path) {
      paste0("http://", server$getHost(), ":", server$getPort(), path)
    },
    performTests = function(tests, response) {
      testthat::test_that(
        paste("$perform results for", private$method, "on", private$path),
        {
          for (test in tests) {
            test(response)
          }
        }
      )
    }
  )
)
