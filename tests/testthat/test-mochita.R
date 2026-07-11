describe("mochita(app)", {
  it("should start a server and perform the tests", {
    mochita(app)$get("/")$set(
      "User-Agent",
      "my cool browser"
    )$set("Accept", "text/plain")$expect(200)$expect(
      "Content-Type",
      "text/plain"
    )$perform()
  })
})
