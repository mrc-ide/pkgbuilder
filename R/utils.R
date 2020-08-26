`%||%` <- function(a, b) { # nolint
  if (is.null(a)) b else a
}


## Create temporary directories with a sensible type
temp_dir <- function(type, workdir = NULL) {
  ret <- tempfile(tmpdir = workdir %||% tempdir(),
                  pattern = sprintf("pb_%s_", type))
  dir.create(ret, FALSE, TRUE)
  ret
}


dir_create <- function(path) {
  dir.create(path, FALSE, TRUE)
}
