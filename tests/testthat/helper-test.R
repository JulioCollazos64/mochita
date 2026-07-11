router <- routing::Router$new()
router$get("/", \(req, res) {
  list(
    body = req$HTTP_USER_AGENT,
    status = 200L,
    headers = list(
      `Content-Type` = "text/plain"
    )
  )
})

app <- list(
  call = function(req) {
    router$handle(req)
  }
)
app
