context("integration: build")

test_that("build a package", {
  workdir <- tempfile()
  capture.output(res <- pb_build("reside-ic/defer", workdir = workdir))
  expect_true(file.exists(res))
  expect_true(file.exists(file.path(workdir, "mirror/github/reside-ic/defer")))

  lib <- tempfile()
  dir_create(lib)
  install.packages(res, lib, repos = NULL)

  expect_true("defer" %in% dir(lib))
})


test_that("build a package with compiled code", {
  workdir <- tempfile()
  capture.output(res <- pb_build("mrc-ide/ring", workdir = workdir))
  expect_true(file.exists(res))
  expect_true(file.exists(file.path(workdir, "mirror/github/mrc-ide/ring")))

  lib <- tempfile()
  dir_create(lib)
  install.packages(res, lib, repos = NULL)

  expect_true("ring" %in% dir(lib))
})


## This example really wants creation of two packages that depend on
## each other but that will never go to cran, but that's a level of
## faff I am not excited about getting into at this point. However,
## here we do not get 'ring' in the library as it's a LinkingTo
## dependency...
test_that("build a package with extra dependencies", {
  workdir <- tempfile()
  capture.output(res <- pb_build("mrc-ide/dde", "mrc-ide/ring", workdir))
  expect_true(file.exists(res))
  expect_true(file.exists(file.path(workdir, "mirror/github/mrc-ide/dde")))

  lib <- tempfile()
  dir_create(lib)
  install.packages(res, lib, repos = NULL)

  expect_true("dde" %in% dir(lib))
})
