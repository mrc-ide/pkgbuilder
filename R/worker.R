pb_worker <- function(version, workdir, timeout = NULL) {
  path <- path_queue(workdir)

  check_version(version, vcapply(liteq::list_queues(path), "[[", "name"))
  timeout <- check_timeout(timeout)

  queue <- liteq::ensure_queue(version, path)

  function() {
    worker_poll(queue, version, timeout)
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
  success <- worker_build(version, id, from_json(m$message), timeout)
  if (success) {
    liteq::ack(m)
  } else {
    liteq::nack(m)
  }
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
