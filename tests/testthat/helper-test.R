app <- list(
  call = function(req) {
    router$handle(req)
  }
)
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

router$get("/html", \(req, res) {
  list(
    body = sprintf('<p class="user-agent">%s</p>', req$HTTP_USER_AGENT),
    status = 200L,
    headers = list(
      `Content-Type` = "text/html"
    )
  )
})
