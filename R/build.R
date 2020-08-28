##' Build a binary package, given a remote-style reference to a
##' package (e.g., `user/repo`)
##'
##' @title Build binary package
##'
##' @param ref A remote-style refrence to a package (e.g., `user/repo`)
##'
##' @param extra_dependencies An optional character vector of
##'   additional dependencies to install, also remote-style references.
##'
##' @param workdir An optional working directory to use. If non-NULL,
##'   then source trees will persist across calls to `pb_build`, which
##'   will make things a little faster.
##'
##' @export
pb_build <- function(ref, extra_dependencies = NULL, workdir = NULL) {
  mirror <- update_mirror(ref, workdir)

  src <- update_source_tree(ref, mirror, workdir)
  on.exit(unlink(src, recursive = TRUE), add = TRUE)

  lib <- install_dependencies(src, extra_dependencies, workdir)
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)

  build_binary(src, lib, workdir)
}


## Additional_repos is not supported, and setting additional repos in
## options(repos) does not seem to have an effect; this will cause a
## little grief, but hopefully not too much.
install_dependencies <- function(path, extra, workdir) {
  lib <- temp_dir("lib", workdir)
  on.exit(unlink(lib, recursive = TRUE))
  if (!is.null(extra)) {
    message("Installing additional dependencies")
    install_extra(extra, lib)
  }

  message("Preparing local library")
  install_deps(path, lib)

  on.exit()

  lib
}


install_extra <- function(extra, lib) {
  config <- list(library = lib)
  install_proposal(
    pkgdepends::new_pkg_installation_proposal(extra, config = config), lib)
}


install_deps <- function(path, lib) {
  config <- list(library = lib, dependencies = TRUE)
  name <- paste0("deps::", path)
  install_proposal(
    pkgdepends::new_pkg_installation_proposal(name, config = config), lib)
}


install_proposal <- function(proposal, lib) {
  proposal$solve()
  proposal$stop_for_solution_error()
  proposal$download()
  proposal$stop_for_download_error()
  plan <- proposal$get_install_plan()
  print(pkgdepends::install_package_plan(plan = plan, lib = lib))
}


build_binary <- function(path, lib, workdir) {
  dest <- temp_dir("bin", workdir)
  message("Building binary")
  dir_create(dest)
  withr::with_libpaths(
    lib,
    pkgbuild::build(path, dest, binary = TRUE))
}


update_mirror <- function(ref, workdir) {
  ref <- pkgdepends::parse_pkg_ref(ref)
  if (ref$type != "github") {
    stop("Non-github refs not yet supported")
  }

  ## TODO: optionally use ssh urls
  url <- sprintf("https://github.com/%s/%s", ref$username, ref$repo)
  mirror <- file.path(workdir %||% tempdir(), "mirror",
                      ref$type, ref$username, ref$repo)
  dir_create(dirname(mirror))
  if (file.exists(mirror)) {
    message(sprintf("Updating repo '%s'", url))
    gert::git_fetch("origin", repo = mirror)
  } else {
    message(sprintf("Cloning '%s'", url))
    gert::git_clone(url, mirror, mirror = TRUE)
  }

  mirror
}


update_source_tree <- function(ref, mirror, workdir) {
  ref <- pkgdepends::parse_pkg_ref(ref)$commitish
  message("Preparing source tree")
  src <- temp_dir("src", workdir)
  gert::git_clone(mirror, src)
  if (nzchar(ref)) {
    message(sprintf("Checking out ref '%s'", ref))
    ## TODO: this does not work for non-branch refs (hashes, prs, tags)
    gert::git_branch_create(ref, paste0("origin/", ref),
                            checkout = TRUE, repo = src)
  }
  src
}
