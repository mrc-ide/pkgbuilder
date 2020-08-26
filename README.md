## pkgbuilder

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build status](https://github.com/mrc-ide/pkgbuilder/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/pkgbuilder/actions)
[![codecov.io](https://codecov.io/github/mrc-ide/pkgbuilder/coverage.svg?branch=master)](https://codecov.io/github/mrc-ide/pkgbuilder?branch=master)
[![CodeFactor](https://www.codefactor.io/repository/github/mrc-ide/pkgbuilder/badge)](https://www.codefactor.io/repository/github/mrc-ide/pkgbuilder)
<!-- badges: end -->

## Installation

We require the in-development version of `pak` with its bundled library, at least for now.  Our installation instructions are a bit unusual as a result:

```r
install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/")
pak::pkg_install("mrc-ide/pkgbuilder")
```

## Usage

Bring up an API using

```r
pkgbuilder::pb_server(c("3.6", "4.0"), "pb", 8080)
```

This will block, but expose a server on port 8080

```
$ curl -s http://localhost:8080/ | jq
{
  "status": "success",
  "errors": null,
  "data": {
    "pkgbuilder": "0.0.1",
    "versions": [
      "3.6",
      "4.0"
    ]
  }
}
```

You also need to start workers that will build packages

```r
pkgbuilder::pb_worker("pb")
```

Submit a job by posting to `/<version>/submit`

```
$ curl -s --data '{"ref": "mrc-ide/dust"}' -H "Content-Type: application/json" \
  http://localhost:8080/4.0/submit | jq
{
  "status": "success",
  "errors": null,
  "data": "4289a9a84808765ad1facf5af752ac77"
}
```

Retrieve job status using `/<version>/status/<id>`

```
$ curl -s http://localhost:8080/4.0/status/4289a9a84808765ad1facf5af752ac77 | jq
{
  "status": "success",
  "errors": null,
  "data": {
    "status": "WORKING",
    "log": null
  }
}
```

Retrieve the file using `/<version>/result/<id>`

```
$ curl -OJ http://localhost:8080/4.0/result/4289a9a84808765ad1facf5af752ac77
curl: Saved to filename 'dust_0.4.9.tgz'
```

## License

MIT © Imperial College of Science, Technology and Medicine
