`%||%` <- function(a, b) { # nolint
  if (is.null(a)) b else a
}


## Create temporary directories with a sensible type
temp_dir <- function(type, workdir = NULL) {
  ret <- tempfile(tmpdir = workdir %||% tempdir(),
                  pattern = sprintf("pb_%s_", type))
  dir_create(ret)
  ret
}


scalar <- function(x) {
  jsonlite::unbox(x)
}


to_json <- function(x, ...) {
  jsonlite::toJSON(x, ..., null = "null")
}


from_json <- function(x) {
  jsonlite::fromJSON(x, simplifyVector = TRUE,
                     simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
}


squote <- function(x) {
  sprintf("'%s'", x)
}


dir_create <- function(path) {
  dir.create(path, FALSE, TRUE)
}


read_lines_safely <- function(path) {
  tryCatch(readLines(path), error = function(e) character())
}


vcapply <- function(x, fun, ...) {
  vapply(x, fun, "", ...)
}


data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
}


r_version2 <- function(version = getRversion()) {
  as.character(version[1, 1:2])
}


forever <- function(callback) {
  repeat {
    callback()
  }
}


read_binary <- function(path) {
  readBin(path, raw(), file.size(path))
}
