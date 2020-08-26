context("api")

test_that("Can list when queue is empty", {
  workdir <- tempfile()
  version <- "4.0"
  q <- queue$new(version, workdir)

  endpoint <- endpoint_list(q)
  res <- endpoint$run(version)
  expect_equal(res$status_code, 200L)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$data, data_frame(id = character(), status = character()))
})


test_that("Can list when queue not empty", {
  workdir <- tempfile()
  version <- "4.0"
  q <- queue$new(version, workdir)
  id1 <- q$submit(version, "user/repo", NULL)
  id2 <- q$submit(version, "user/repo", NULL)

  endpoint <- endpoint_list(q)
  res <- endpoint$run(version)
  expect_equal(res$status_code, 200L)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$data, data_frame(id = c(id1, id2), status = "READY"))
})


test_that("Can get status", {
  workdir <- tempfile()
  version <- "4.0"
  q <- queue$new(version, workdir)
  id <- q$submit(version, "user/repo", NULL)
  endpoint <- endpoint_status(q)

  res <- endpoint$run(version, id)
  expect_equal(res$status_code, 200L)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$data, list(status = scalar("READY"), log = NULL))
})


test_that("can get logs from status", {
  q <- list(
    status = function(version, id, log = FALSE) {
      list(status = "RUNNING",
           log = if (log) log_data else NULL)
    })

  version <- "4.0"
  id <- ids::random_id()
  status <- "RUNNING"
  log_data <- letters[1:5]

  endpoint <- endpoint_status(q)

  res <- endpoint$run(version, id)
  expect_equal(res$status_code, 200L)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$data, list(status = scalar("RUNNING"), log = NULL))

  expect_identical(endpoint$run(version, id, FALSE, 10), res)

  res <- endpoint$run(version, id, TRUE)
  expect_equal(res$status_code, 200L)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$data, list(status = scalar("RUNNING"), log = log_data))

  res <- endpoint$run(version, id, TRUE, 3)
  expect_equal(res$status_code, 200L)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$data, list(status = scalar("RUNNING"), log = log_data[4:5]))

  res <- endpoint$run(version, id, TRUE, 20)
  expect_equal(res$status_code, 200L)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$data, list(status = scalar("RUNNING"), log = character(0)))
})


test_that("can download binary", {
  tmp <- tempfile()
  dir_create(tmp)
  filename <- file.path(tmp, "pkg_0.1.2.tar.gz")
  bytes <- as.raw(0:255)
  writeBin(bytes, filename)

  version <- "4.0"
  id <- ids::random_id()

  q <- list(result = mockery::mock(filename))
  endpoint <- endpoint_result(q)

  res <- endpoint$run(version, id)
  expect_equal(res$status_code, 200L)
  expect_equal(res$headers, header_filename(basename(filename)))
  expect_equal(res$body, bytes)

  mockery::expect_called(q$result, 1)
  expect_equal(mockery::mock_args(q$result)[[1]], list(version, id))
})


test_that("Sensible error on unknown id", {
  version <- "4.0"
  id <- "abcdef"

  q <- list(result = mockery::mock(NULL))
  endpoint <- endpoint_result(q)

  res <- endpoint$run(version, id)
  expect_equal(res$status_code, 404L)
  expect_null(res$data)
  expect_s3_class(res$error, "pkgapi_error")
  expect_equal(
    res$error$data[[1]]$detail,
    scalar("Result not found (version: '4.0', id: 'abcdef')"))

  mockery::expect_called(q$result, 1)
  expect_equal(mockery::mock_args(q$result)[[1]], list(version, id))
})


test_that("Submit to queue with additional dependencies", {
  version <- "4.0"
  id <- ids::random_id()

  q <- list(submit = mockery::mock(id))
  endpoint <- endpoint_submit(q)
  data <- to_json(list(ref = scalar("user/repo"),
                       additional_dependencies = c("a/b", "c/d")))

  res <- endpoint$run(version, data)

  expect_equal(res$status_code, 200L)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$data, scalar(id))

  mockery::expect_called(q$submit, 1)
  expect_equal(mockery::mock_args(q$submit)[[1]],
               list(version, "user/repo", c("a/b", "c/d")))
})


test_that("Submit to queue without additional dependencies", {
  version <- "4.0"
  id <- ids::random_id()

  q <- list(submit = mockery::mock(id))
  endpoint <- endpoint_submit(q)
  data <- to_json(list(ref = scalar("user/repo"),
                       additional_dependencies = NULL))

  res <- endpoint$run(version, data)

  expect_equal(res$status_code, 200L)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$data, scalar(id))

  mockery::expect_called(q$submit, 1)
  expect_equal(mockery::mock_args(q$submit)[[1]],
               list(version, "user/repo", NULL))
})


test_that("root endpoint", {
  workdir <- tempfile()
  version <- c("3.5", "4.0")
  q <- queue$new(version, workdir)
  endpoint <- endpoint_root(q)

  res <- endpoint$run()

  expect_equal(res$status_code, 200L)
  expect_equal(res$content_type, "application/json")
  expect_equal(
    res$data,
    list(pkgbuilder = scalar(as.character(packageVersion("pkgbuilder"))),
         versions = version))
})


test_that("construct api", {
  workdir <- tempfile()
  version <- c("3.5", "4.0")
  q <- queue$new(version, workdir)
  api <- api_build(q)
  expect_is(api, "pkgapi")

  res <- api$request("GET", "/")
  expect_equal(from_json(res$body)$data$versions, version)

  data <- to_json(list(ref = scalar("user/repo")))
  res <- api$request("POST", "/4.0/submit", body = data)
  id <- from_json(res$body)$data
  expect_match(id, "^[[:xdigit:]]{32}$")

  res <- api$request("GET", "/4.0/list")
  expect_equal(from_json(res$body)$data,
               list(list(id = id, status = "READY")))

  res <- api$request("GET", paste0("/4.0/status/", id))
  expect_equal(from_json(res$body)$data,
               list(status = "READY", log = NULL))
})
