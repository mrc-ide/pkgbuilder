##' Run a pkgbuilder worker
##'
##' @title Run a pkgbuilder worker
##'
##' @param workdir Working directory for the queue - must be shared
##'   with the server process.
##'
##' @param timeout A per-job timeout.  Builds that take longer than
##'   this many seconds will be terminated.
##'
##' @return Never returns, runs a worker as a side-effect
##'
##' @export
pb_worker <- function(workdir, timeout = NULL) {
  path <- path_queue(workdir)
  timeout <- check_timeout(timeout)

  version <- r_version2()
  check_version(version, vcapply(liteq::list_queues(path), "[[", "name"))
  queue <- liteq::ensure_queue(version, path)

  ## Run the loop forever. Practically this will run without stopping,
  ## and the check here is only for testing where we'll have the poll
  ## stop
  repeat {
    if (!worker_poll(queue, version, timeout)) {
      break
    }
  }
}


worker_build <- function(version, id, data, timeout) {
  logfile <- path_log(data$workdir, version, id)
  dir.create(dirname(logfile), FALSE, TRUE)
  args <- data

  ## This is using callr as a way of getting all the output (stdout
  ## and stderr) stored into a file. We might be able to pull this off
  ## with functions in withr and not use the additional process, but
  ## it might be for the best really?
  tryCatch({
    path <- callr::r(
      function(ref, extra_dependencies, workdir)
        pkgbuilder::pb_build(ref, extra_dependencies, workdir),
      args = data, stdout = logfile, stderr = logfile, timeout = timeout)
    dest <- path_binary(data$workdir, version, id)
    dir_create(dest)
    file.rename(path, file.path(dest, basename(path)))
    TRUE
  }, error = function(e) FALSE)
}


worker_poll <- function(queue, version, timeout) {
  ## try_consume is meant to return immediately, but does not
  ## always do so for me (can take a few seconds). However,
  ## practically this is not very interesting to do.
  m <- liteq::consume(queue)
  id <- m$title
  data <- from_json(m$message)
  message(sprintf("Running '%s' (%s)", data$ref, id))
  success <- worker_build(version, id, data, timeout)
  if (success) {
    message(sprintf("Built '%s' (%s)", data$ref, id))
    liteq::ack(m)
  } else {
    message(sprintf("Failed '%s' (%s)", data$ref, id))
    liteq::nack(m)
  }
  TRUE
}


check_timeout <- function(timeout) {
  if (is.null(timeout)) {
    timeout <- Inf
  }
  if (length(timeout) != 1L || !is.numeric(timeout) || timeout < 0) {
    stop("Expected a single positive numeric value for 'timeout'")
  }
  timeout
}
