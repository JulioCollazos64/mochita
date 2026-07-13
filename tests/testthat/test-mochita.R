describe("mochita(app)", {
  it("should start a server and perform the tests", {
    mochita(app)$get("/")$set(
      "User-Agent",
      "my cool browser"
    )$set("Accept", "text/plain")$expect(200)$expect(
      "Content-Type",
      "text/plain"
    )$expect("my cool browser")$perform()

    it("should test HTML responses", {
      mochita(app)$get("/html")$set(
        "User-Agent",
        "a cool browser"
      )$set("Accept", "text/html")$expect(200)$expect(
        "Content-Type",
        "text/html"
      )$expect(
        function(res) {
          text <- res$body
          testthat::expect_identical(
            text,
            '<p class="user-agent">a cool browser</p>'
          )
        }
      )$perform()
    })

    it("should accept request body set via the send method", {
      mochita(app)$post("/json")$set("Accept", "application/json")$expect(
        "Content-Type",
        "application/json"
      )$send('{"len":"3"}')$expect(200L, '{"x":[1,2,3]}')$perform()
    })

    it("should allow object assigning", {
      # From upstream:
      # Passing the app or url each time is not necessary, if you're testing the
      # same host you may simply re-assign the request variable with the
      # initialization app or url, a new Test is created per request.VERB()
      req <- mochita(app)

      req$get("/")$set("User-Agent", "firefox")$expect("firefox")$perform()

      req$get("/")$set("User-Agent", "google")$expect("google")$perform()
    })
  })
})
