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

router$post("/json", \(req, res) {
  body <- yyjsonr::read_json_raw(req$rook.input$read())

  body <- yyjsonr::write_json_str(
    list(x = seq_len(body$len)),
    opts = yyjsonr::opts_write_json(auto_unbox = TRUE)
  )

  list(
    body = body,
    status = 200L,
    headers = list(
      `Content-Type` = "application/json"
    )
  )
})
