function (request) {
  root <- klassR:::GetBaseUrl()
  gsub_request(request, root, "", fixed=TRUE)
}