context("build")

test_that("install_dependencies does not install NULL extra deps", {
  skip_if_not_installed("mockery")
  mock_install_extra <- mockery::mock()
  mock_install_deps <- mockery::mock()

  path <- tempfile()
  extra <- NULL
  workdir <- tempfile()

  res <- with_mock(
    "pkgbuilder::install_extra" = mock_install_extra,
    "pkgbuilder::install_deps" = mock_install_deps,
    install_dependencies(path, extra, workdir))

  mockery::expect_called(mock_install_extra, 0)

  mockery::expect_called(mock_install_deps, 1)
  args <- mockery::mock_args(mock_install_deps)[[1]]
  lib <- args[[2]]
  expect_true(same_path(dirname(lib), workdir))
  expect_match(basename(lib), "^pb_lib_")
  expect_equal(args, list(path, lib))
})


test_that("install_dependencies installs extra deps", {
  skip_if_not_installed("mockery")
  mock_install_extra <- mockery::mock()
  mock_install_deps <- mockery::mock()

  path <- tempfile()
  extra <- c("a/b", "c/d")
  workdir <- tempfile()

  res <- with_mock(
    "pkgbuilder:::install_extra" = mock_install_extra,
    "pkgbuilder:::install_deps" = mock_install_deps,
    install_dependencies(path, extra, workdir))

  mockery::expect_called(mock_install_extra, 1)
  args <- mockery::mock_args(mock_install_extra)[[1]]
  lib <- args[[2]]
  expect_true(same_path(dirname(lib), workdir))
  expect_match(basename(lib), "^pb_lib_")
  expect_equal(args, list(extra, lib))

  mockery::expect_called(mock_install_deps, 1)
  args <- mockery::mock_args(mock_install_deps)[[1]]
  expect_equal(args, list(path, lib))
})


test_that("build_binary sets .libPaths()", {
  skip_if_not_installed("mockery")
  mock_build <- mockery::mock(.libPaths())

  path <- tempfile()
  lib <- tempfile()
  workdir <- tempfile()
  dir_create(lib)
  on.exit(unlink(lib, recursive = TRUE))

  res <- with_mock(
    "pkgbuild::build" = mock_build,
    build_binary(path, lib, workdir))
  expect_true(same_path(res[[1]], lib))
  mockery::expect_called(mock_build, 1)

  args <- mockery::mock_args(mock_build)[[1]]
  dest <- args[[2]]
  expect_true(same_path(dirname(dest), workdir))
  expect_match(basename(dest), "^pb_bin_")

  expect_equal(args, list(path, dest, binary = TRUE))
})


test_that("update mirror accepts only github references", {
  expect_error(
    update_mirror(pkgdepends::parse_pkg_ref("pkg"), NULL),
    "Non-github refs not yet supported")
  expect_error(
    update_mirror(pkgdepends::parse_pkg_ref("standard::pkg"), NULL),
    "Non-github refs not yet supported")
})


test_that("update mirror clones path if it does not exist", {
  skip_if_not_installed("mockery")
  workdir <- tempfile()
  ref <- pkgdepends::parse_pkg_ref("user/repo@branch")
  mock_git_fetch <- mockery::mock()
  mock_git_clone <- mockery::mock()
  expected_mirror <- file.path(workdir, "mirror/github/user/repo")

  mirror <- with_mock(
    "gert::git_fetch" = mock_git_fetch,
    "gert::git_clone" = mock_git_clone,
    update_mirror(ref, workdir))

  expect_equal(mirror, expected_mirror)
  mockery::expect_called(mock_git_fetch, 0)
  mockery::expect_called(mock_git_clone, 1)
  expect_equal(
    mockery::mock_args(mock_git_clone)[[1]],
    list("https://github.com/user/repo",
         mirror,
         mirror = TRUE))
})


test_that("update mirror fetches path if it does exist", {
  skip_if_not_installed("mockery")
  workdir <- tempfile()
  ref <- pkgdepends::parse_pkg_ref("user/repo")
  mock_git_fetch <- mockery::mock()
  mock_git_clone <- mockery::mock()
  expected_mirror <- file.path(workdir, "mirror/github", ref$ref)
  dir_create(expected_mirror)
  on.exit(unlink(expected_mirror, recursive = TRUE))

  mirror <- with_mock(
    "gert::git_fetch" = mock_git_fetch,
    "gert::git_clone" = mock_git_clone,
    update_mirror(ref, workdir))

  expect_equal(mirror, expected_mirror)
  mockery::expect_called(mock_git_fetch, 1)
  mockery::expect_called(mock_git_clone, 0)
  expect_equal(
    mockery::mock_args(mock_git_fetch)[[1]],
    list("origin", repo = mirror))
})


test_that("update source tree from mirror gets correct reference", {
  skip_if_not_installed("mockery")
  workdir <- tempdir()
  mirror <- tempdir()
  ref <- "user/repo@branch"

  mock_git_clone <- mockery::mock()
  mock_git_branch_create <- mockery::mock()

  src <- with_mock(
    "gert::git_clone" = mock_git_clone,
    "gert::git_branch_create" = mock_git_branch_create,
    update_source_tree(ref, mirror, workdir))

  expect_true(same_path(dirname(src), workdir))
  expect_match(basename(src), "^pb_src_")

  mockery::expect_called(mock_git_clone, 1)
  expect_equal(mockery::mock_args(mock_git_clone)[[1]],
               list(workdir, src))

  mockery::expect_called(mock_git_branch_create, 1)
  expect_equal(mockery::mock_args(mock_git_branch_create)[[1]],
               list("branch", "origin/branch", checkout = TRUE, repo = src))
})


test_that("update source tree from mirror ignores missing reference", {
  skip_if_not_installed("mockery")
  workdir <- tempdir()
  mirror <- tempdir()
  ref <- "user/repo"

  mock_git_clone <- mockery::mock()
  mock_git_branch_create <- mockery::mock()

  src <- with_mock(
    "gert::git_clone" = mock_git_clone,
    "gert::git_branch_create" = mock_git_branch_create,
    update_source_tree(ref, mirror, workdir))

  expect_true(same_path(dirname(src), workdir))
  expect_match(basename(src), "^pb_src_")

  mockery::expect_called(mock_git_clone, 1)
  expect_equal(mockery::mock_args(mock_git_clone)[[1]],
               list(workdir, src))

  mockery::expect_called(mock_git_branch_create, 0)
})


test_that("pb_build passes expected arguments and cleans up", {
  skip_if_not_installed("mockery")
  ref <- pkgdepends::parse_pkg_ref("user/repo")
  extra_dependencies <- "user/extra"
  workdir <- tempfile()
  mirror <- tempfile()
  src <- tempfile()
  lib <- tempfile()
  dir_create(src)
  dir_create(lib)

  mock_update_mirror <- mockery::mock(mirror)
  mock_update_source_tree <- mockery::mock(src)
  mock_install_dependencies <- mockery::mock(lib)
  mock_build_binary <- mockery::mock()

  with_mock(
    "pkgbuilder::update_mirror" = mock_update_mirror,
    "pkgbuilder::update_source_tree" = mock_update_source_tree,
    "pkgbuilder::install_dependencies" = mock_install_dependencies,
    "pkgbuilder::build_binary" = mock_build_binary,
    pb_build(ref$ref, extra_dependencies, workdir))

  mockery::expect_called(mock_update_mirror, 1)
  expect_equal(mockery::mock_args(mock_update_mirror)[[1]],
               list(ref, workdir))

  mockery::expect_called(mock_update_source_tree, 1)
  expect_equal(mockery::mock_args(mock_update_source_tree)[[1]],
               list(ref, mirror, workdir))

  mockery::expect_called(mock_install_dependencies, 1)
  expect_equal(mockery::mock_args(mock_install_dependencies)[[1]],
               list(src, extra_dependencies, workdir))

  mockery::expect_called(mock_build_binary, 1)
  expect_equal(mockery::mock_args(mock_build_binary)[[1]],
               list(src, lib, workdir))

  expect_false(file.exists(src))
  expect_false(file.exists(lib))
})
