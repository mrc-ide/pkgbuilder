##' Run a pkgbuilder server
##'
##' @title Run a pkgbuilder server
##'
##' @param versions A vector of 2-digit version numbers corresponding
##'   to the versions of R to build packages for.  Workers will need
##'   to be provided separately to build the packages.
##'
##' @param workdir A directory to work in. This must be shared between
##'   the workers and the server, so do not use a temporary directory.
##'
##' @param port The port to run on. Your operating system will likely
##'   restrict you from using very low port numbers, so use something
##'   like 8080
##'
##' @return Never returns - runs a HTTP server as a side-effect
##' @export
pb_server <- function(versions, workdir, port) {
  queue <- queue$new(versions, workdir)
  api <- api_build(queue)
  api$run(port = port)
}


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
    "POST", "/<version>/submit/ref", target_submit(queue),
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
    pkgapi::pkgapi_input_query(log = "logical", skip = "integer"),
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
