api_build <- function(queue) {
  api <- pkgapi::pkgapi$new()
  api$handle(endpoint_root(queue))
  api$handle(endpoint_submit(queue))
  api$handle(endpoint_list(queue))
  api$handle(endpoint_status(queue))
  api$handle(endpoint_result(queue))
  api
}


target_root <- function(queue) {
  pkgbuilder <- scalar(as.character(packageVersion("pkgbuilder")))
  function() {
    list(pkgbuilder = pkgbuilder,
         versions = queue$versions)
  }
}


endpoint_root <- function(queue) {
  returning <- pkgapi::pkgapi_returning_json("Root.schema", schema_root())
  pkgapi::pkgapi_endpoint$new(
    "GET", "/", target_root(queue),
    returning = returning)
}


target_list <- function(queue) {
  function(version) {
    queue$list(version)
  }
}


endpoint_list <- function(queue) {
  returning <- pkgapi::pkgapi_returning_json("List.schema", schema_root())
  pkgapi::pkgapi_endpoint$new(
    "GET", "/<version>/list", target_list(queue),
    returning = returning)
}


target_submit <- function(queue) {
  function(version, data) {
    data <- from_json(data)
    id <- queue$submit(version, data$ref, data$additional_dependencies)
    scalar(id)
  }
}


endpoint_submit <- function(queue) {
  returning <- pkgapi::pkgapi_returning_json("Submit.schema", schema_root())
  pkgapi::pkgapi_endpoint$new(
    "POST", "/<version>/submit", target_submit(queue),
    pkgapi::pkgapi_input_body_json("data", "Submission.schema", schema_root()),
    returning = returning)
}


target_status <- function(queue) {
  function(version, id, log = FALSE, skip = 0L) {
    ret <- queue$status(version, id, log)
    ret$status <- scalar(ret$status)
    if (log && skip > 0) {
      ret$log <- ret$log[-seq_len(skip)]
    }
    ret
  }
}


endpoint_status <- function(queue) {
  returning <- pkgapi::pkgapi_returning_json("Status.schema", schema_root())
  pkgapi::pkgapi_endpoint$new(
    "GET", "/<version>/status/<id>", target_status(queue),
    pkgapi::pkgapi_input_query(log = "logical"),
    returning = returning)
}


target_result <- function(queue) {
  function(version, id) {
    path <- queue$result(version, id)
    if (is.null(path)) {
      msg <- sprintf("Result not found (version: '%s', id: '%s')",
                     version, id)
      pkgapi::pkgapi_stop(msg, status_code = 404L)
    } else {
      pkgapi::pkgapi_add_headers(read_binary(path),
                                 header_filename(basename(path)))
    }
  }
}


endpoint_result <- function(queue) {
  pkgapi::pkgapi_endpoint$new(
    "GET", "/<version>/result/<id>", target_result(queue),
    returning = pkgapi::pkgapi_returning_binary())
}


schema_root <- function() {
  system.file("schema", package = "pkgbuilder")
}


header_filename <- function(filename) {
  list("Content-Disposition" = sprintf('attachment; filename="%s"', filename))
}
