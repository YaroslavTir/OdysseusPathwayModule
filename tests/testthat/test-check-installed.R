# --- Tests for check_installed() ---

test_that("check_installed rejects non-character or empty pkg", {
  expect_error(
    OdysseusPathwayModule:::check_installed(pkg = 123),
    "`pkg` must be a character vector"
  )
  expect_error(
    OdysseusPathwayModule:::check_installed(pkg = character(0)),
    "`pkg` must be a character vector"
  )
})

test_that("check_installed returns TRUE for an installed package", {
  expect_true(OdysseusPathwayModule:::check_installed("testthat"))
})

test_that("check_installed returns TRUE when version is satisfied (>=)", {
  expect_true(
    OdysseusPathwayModule:::check_installed("testthat", version = "0.0.1", compare = ">=")
  )
})

test_that("check_installed defaults compare to >= when version supplied", {

  expect_true(
    OdysseusPathwayModule:::check_installed("testthat", version = "0.0.1")
  )
})

test_that("check_installed version check with == operator succeeds", {
  current <- as.character(utils::packageVersion("testthat"))
  expect_true(
    OdysseusPathwayModule:::check_installed("testthat", version = current, compare = "==")
  )
})

test_that("check_installed version check with <= operator succeeds", {
  expect_true(
    OdysseusPathwayModule:::check_installed("testthat", version = "99999.0.0", compare = "<=")
  )
})

test_that("check_installed version check with < operator succeeds", {
  expect_true(
    OdysseusPathwayModule:::check_installed("testthat", version = "99999.0.0", compare = "<")
  )
})

test_that("check_installed version check with > operator on low version", {
  expect_true(
    OdysseusPathwayModule:::check_installed("testthat", version = "0.0.1", compare = ">")
  )
})

test_that("check_installed version check with != operator", {
  expect_true(
    OdysseusPathwayModule:::check_installed("testthat", version = "0.0.1", compare = "!=")
  )
})

test_that("check_installed errors on unknown comparison operator", {
  expect_error(
    OdysseusPathwayModule:::check_installed("testthat", version = "0.0.1", compare = "~="),
    "Unknown comparison operator"
  )
})

test_that("check_installed stops for single missing package (non-interactive)", {
  err <- expect_error(
    OdysseusPathwayModule:::check_installed("thisNotExistPkg999xyz"),
    "required but not installed"
  )
  expect_match(conditionMessage(err), 'install.packages\\("thisNotExistPkg999xyz"\\)')
  expect_match(conditionMessage(err), "package is required")
})

test_that("check_installed stops for multiple missing packages (non-interactive)", {
  err <- expect_error(
    OdysseusPathwayModule:::check_installed(c("noSuchA999", "noSuchB999")),
    "packages are required"
  )
  expect_match(conditionMessage(err), "install.packages\\(c\\(")
})

test_that("check_installed includes reason in error message", {
  expect_error(
    OdysseusPathwayModule:::check_installed(
      "thisNotExistPkg999xyz",
      reason = "needed for testing"
    ),
    "Reason: needed for testing"
  )
})

test_that("check_installed stops when version needs update (single pkg)", {
  err <- expect_error(
    OdysseusPathwayModule:::check_installed("testthat", version = "99999.0.0", compare = ">="),
    "needs an update"
  )
  expect_match(conditionMessage(err), "installed:")
  expect_match(conditionMessage(err), "required: >= 99999.0.0")
})

test_that("check_installed stops when version needs update (multiple pkgs)", {
  # Use two packages guaranteed to be installed during tests
  err <- expect_error(
    OdysseusPathwayModule:::check_installed(
      c("testthat", "utils"),
      version = "99999.0.0",
      compare = ">="
    ),
    "need an update"
  )
  expect_match(conditionMessage(err), "packages")
})

test_that("check_installed invokes custom action and returns FALSE", {
  captured <- NULL
  result <- OdysseusPathwayModule:::check_installed(
    "thisNotExistPkg999xyz",
    action = function(pkg, version, compare, reason) {
      captured <<- list(pkg = pkg, version = version,
                        compare = compare, reason = reason)
    }
  )
  expect_false(result)
  expect_equal(captured$pkg, "thisNotExistPkg999xyz")
})

test_that("check_installed custom action receives reason", {
  captured_reason <- NULL
  OdysseusPathwayModule:::check_installed(
    "thisNotExistPkg999xyz",
    reason = "testing reason",
    action = function(pkg, version, compare, reason) {
      captured_reason <<- reason
    }
  )
  expect_equal(captured_reason, "testing reason")
})

test_that("check_installed custom action for version-update path", {
  captured_pkgs <- NULL
  OdysseusPathwayModule:::check_installed(
    "testthat",
    version = "99999.0.0",
    compare = ">=",
    action = function(pkg, version, compare, reason) {
      captured_pkgs <<- pkg
    }
  )
  expect_equal(captured_pkgs, "testthat")
})

test_that("check_installed with both missing and needs-update uses action", {
  captured_pkgs <- NULL
  OdysseusPathwayModule:::check_installed(
    c("thisNotExistPkg999xyz", "testthat"),
    version = c(NA, "99999.0.0"),
    compare = c(">=", ">="),
    action = function(pkg, version, compare, reason) {
      captured_pkgs <<- pkg
    }
  )
  expect_true("thisNotExistPkg999xyz" %in% captured_pkgs)
  expect_true("testthat" %in% captured_pkgs)
})

test_that("check_installed version NA is skipped", {
  expect_true(
    OdysseusPathwayModule:::check_installed("testthat", version = NA_character_)
  )
})
