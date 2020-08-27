queue <- R6::R6Class(
  "queue",

  private = list(
    workdir = NULL,
    queue = NULL,

    is_complete = function(version, id) {
      file.exists(file.path(path_binary(private$workdir, version, id)))
    },

    read_log = function(version, id) {
      path <- path_log(private$workdir, version, id)
      if (file.exists(path)) {
        read_lines_safely(path)
      } else {
        NULL
      }
    }
  ),

  public = list(
    versions = NULL,

    initialize = function(versions, workdir) {
      dir_create(workdir)

      self$versions <- versions
      private$workdir <- workdir
      private$queue <- path_queue(workdir)
      for (v in self$versions) {
        liteq::ensure_queue(v, private$queue)
        dir_create(path_log(workdir, v, ""))
        dir_create(path_binary(workdir, v, ""))
      }
      lockBinding("versions", self)
    },

    submit = function(version, ref, extra_dependencies) {
      check_version(version, self$versions)

      lq <- liteq::ensure_queue(version, private$queue)
      id <- ids::random_id()

      data <- list(ref = scalar(ref),
                   extra_dependencies = extra_dependencies,
                   workdir = scalar(private$workdir))

      liteq::list_queues(private$queue)
      liteq::publish(lq, id, as.character(to_json(data)))
      id
    },

    list = function(version) {
      check_version(version, self$versions)
      lq <- liteq::ensure_queue(version, private$queue)
      data <- liteq::list_messages(lq)
      data_frame(id = data$title, status = data$status)
    },

    status = function(version, id, log = FALSE) {
      list <- self$list(version)

      status <- list$status[match(id, list$id)]
      if (is.na(status)) {
        if (private$is_complete(version, id)) {
          status <- "COMPLETE"
        } else {
          status <- "UNKNOWN"
        }
      }

      if (log && status != "UNKNOWN") {
        log_txt <- private$read_log(version, id)
      } else {
        log_txt <- NULL
      }

      list(status = status, log = log_txt)
    },

    result = function(version, id) {
      check_version(version, self$versions)
      if (private$is_complete(version, id)) {
        dir(path_binary(private$workdir, version, id), full.names = TRUE)
      } else {
        NULL
      }
    }
  ))


path_queue <- function(workdir) {
  file.path(workdir, "db.sqlite")
}


path_log <- function(workdir, version, id) {
  file.path(workdir, "logs", version, id)
}


path_binary <- function(workdir, version, id) {
  file.path(workdir, "binary", version, id)
}


check_version <- function(version, valid) {
  if (!(version %in% valid)) {
    stop(sprintf("'version' must be one of %s",
                 paste(squote(valid), collapse = ", ")))
  }
}
