same_path <- function(a, b) {
  file.exists(a) && file.exists(b) && normalizePath(a) == normalizePath(b)
}
