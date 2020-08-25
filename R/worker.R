pb_worker <- function(version, workdir) {
  path <- path_queue(workdir)

  check_version(version, vcapply(liteq::list_queues(path), "[[", "name"))

  queue <- liteq::ensure_queue(version, path)

  function() {
    worker_poll(queue, version)
  }
}


worker_build <- function(version, id, data) {
  logfile <- path_log(data$workdir, version, id)
  dir.create(dirname(logfile), FALSE, TRUE)
  args <- data

  tryCatch({
    path <- callr::r(
      function(ref, extra_dependencies, workdir)
        pkgbuilder::pb_build(ref, extra_dependencies, workdir),
      args = data, stdout = logfile, stderr = logfile)
    dest <- path_binary(data$workdir, version, id)
    dir_create(dest)
    file.rename(path, file.path(dest, basename(path)))
    TRUE
  }, error = function(e) FALSE)
}


worker_poll <- function(queue, version) {
  ## try_consume is meant to return immediately, but does not
  ## always do so for me (can take a few seconds). However,
  ## practically this is not very interesting to do.
  m <- liteq::consume(queue)
  id <- m$title
  success <- worker_build(version, id, from_json(m$message))
  if (success) {
    liteq::ack(m)
  } else {
    liteq::nack(m)
  }
}
