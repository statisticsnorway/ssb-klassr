function (response) {
  root <- klassR:::GetBaseUrl()
  gsub_response(request, root, "", fixed=TRUE)
}