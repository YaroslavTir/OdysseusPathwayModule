# Copyright 2026 Odysseus Data Services
#
# This file is part of OdysseusPathwayModule
#
# Adapted from CohortPathways by OHDSI (https://github.com/OHDSI/CohortPathways).
# The core implementation of CohortPathway analysis was developed by Christopher
# Knoll (github: chrisknoll), and enhanced by members of the OHDSI community:
# mick-iqvia (github), Odysseus Inc., and many other collaborators on the
# OHDSI/WebAPI team.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


# --- Standalone helpers (replaces purrr dependency) ---
# Adapted from upstream-src/standalone.R



#' Safely Extract Nested Elements
#'
#' A lightweight replacement for \code{purrr::pluck()} that extracts
#' deeply nested elements from lists using integer positions, character
#' names, or accessor functions.
#'
#' @param .x A list or vector to extract from.
#' @param ... Accessors (integer, character, or function) applied sequentially.
#' @param .default Value to return if the element is missing or \code{NULL}.
#'
#' @return The extracted element, or \code{.default} if not found.
#'
#' @keywords internal
#' @noRd
.pluck <- function(.x, ..., .default = NULL) {
  accessors <- list(...)
  if (length(accessors) == 0) return(.x)

  result <- .x
  for (accessor in accessors) {
    if (is.null(result)) return(.default)
    if (is.numeric(accessor)) {
      accessor <- as.integer(accessor)
      if (accessor > length(result) || accessor < 1) return(.default)
      result <- result[[accessor]]
    } else if (is.character(accessor)) {
      if (!accessor %in% names(result)) return(.default)
      result <- result[[accessor]]
    } else if (is.function(accessor)) {
      result <- accessor(result)
    } else {
      stop("Accessor must be an integer, character, or function")
    }
  }
  if (is.null(result)) return(.default)
  result
}

#' Map and Column-Bind Results
#'
#' A lightweight replacement for \code{purrr::map_dfc()} that applies a
#' function to each element of a list and column-binds the results into a
#' single data frame.
#'
#' @param .x A list or vector to iterate over.
#' @param .f A function to apply to each element of \code{.x}.
#' @param ... Additional arguments passed to \code{.f}.
#'
#' @return A \code{data.frame} formed by column-binding the results.
#'
#' @keywords internal
#' @noRd
.map_dfc <- function(.x, .f, ...) {
  result_list <- lapply(.x, .f, ...)
  do.call(cbind, lapply(result_list, as.data.frame))
}


# --- Package check utility ---

#' Check Required Packages and Versions
#'
#' Verifies that the specified packages are installed and optionally meet
#' minimum version requirements. In interactive sessions, offers to install
#' missing packages; in non-interactive sessions, throws an informative error.
#'
#' @param pkg A character vector of package names to check for installation
#'   and version requirements.
#' @param reason Optional string providing context for why the packages are
#'   needed, included in error messages.
#' @param version Optional character vector of required package versions
#'   corresponding to \code{pkg}. Can be a single version or one per package.
#' @param compare Optional character vector of comparison operators (e.g.
#'   \code{">="}, \code{"=="}) corresponding to \code{pkg} for version checks.
#'   Can be a single operator or one per package. Defaults to \code{">="}
#'   if \code{version} is provided without \code{compare}.
#' @param action Optional function to call if any packages are missing or do
#'   not meet version requirements. The function will be passed the arguments
#'   \code{pkg}, \code{version}, \code{compare}, and \code{reason} for custom
#'   handling (e.g., automated installation). If not provided, an interactive
#'   prompt will be offered to install missing packages, or an error will be
#'   thrown in non-interactive sessions.
#' @param call The call to report in error messages if packages are missing.
#'   Defaults to the parent call.
#'
#' @return Invisibly returns \code{TRUE} if all packages are installed and
#'   meet version requirements. Otherwise, throws an error or invokes
#'   \code{action}.
#'
#' @keywords internal
#' @noRd
check_installed <- function(pkg,
                            reason = NULL,
                            version = NULL,
                            compare = NULL,
                            action = NULL,
                            call = sys.call(-1L)) {


  # --- input validation ---

  if (!is.character(pkg) || length(pkg) == 0L) {
    stop("`pkg` must be a character vector of package names.", call. = FALSE)
  }

  if (!is.null(version)) {
    version <- rep_len(as.character(version), length(pkg))
  }

  if (!is.null(compare)) {
    compare <- rep_len(as.character(compare), length(pkg))
  } else if (!is.null(version)) {
    compare <- rep_len(">=", length(pkg))
  }

  # --- check each package ---
  missing_pkg     <- character(0L)
  needs_update    <- character(0L)
  missing_reasons <- character(0L)

  for (i in seq_along(pkg)) {
    p <- pkg[i]

    if (!requireNamespace(p, quietly = TRUE)) {
      missing_pkg <- c(missing_pkg, p)
      next
    }

    # version check
    if (!is.null(version) && !is.na(version[i])) {
      cmp <- if (!is.null(compare)) compare[i] else ">="
      current <- as.character(utils::packageVersion(p))
      satisfied <- utils::compareVersion(current, version[i])

      ok <- switch(cmp,
                   ">="  = satisfied >= 0L,
                   ">"   = satisfied >  0L,
                   "=="  = satisfied == 0L,
                   "<="  = satisfied <= 0L,
                   "<"   = satisfied <  0L,
                   "!="  = satisfied != 0L,
                   stop(sprintf("Unknown comparison operator `%s`.", cmp),
                        call. = FALSE)
      )

      if (!ok) {
        needs_update <- c(needs_update, p)
        missing_reasons <- c(
          missing_reasons,
          sprintf(
            "  - %s (installed: %s, required: %s %s)",
            p, current, cmp, version[i]
          )
        )
      }
    }
  }

  # --- nothing to do ---
  if (length(missing_pkg) == 0L && length(needs_update) == 0L) {
    return(invisible(TRUE))
  }

  # --- build informative message ---
  parts <- character(0L)

  if (length(missing_pkg) > 0L) {
    noun <- if (length(missing_pkg) == 1L) "package" else "packages"
    parts <- c(parts, sprintf(
      "The following %s %s required but not installed:\n  - %s",
      noun,
      if (length(missing_pkg) == 1L) "is" else "are",
      paste(missing_pkg, collapse = "\n  - ")
    ))
  }

  if (length(needs_update) > 0L) {
    noun <- if (length(needs_update) == 1L) "package" else "packages"
    parts <- c(parts, sprintf(
      "The following %s %s installed but %s an update:\n%s",
      noun,
      if (length(needs_update) == 1L) "is" else "are",
      if (length(needs_update) == 1L) "needs" else "need",
      paste(missing_reasons, collapse = "\n")
    ))
  }

  msg <- paste(parts, collapse = "\n\n")

  if (!is.null(reason)) {
    msg <- paste0(msg, "\n\nReason: ", reason)
  }

  # --- custom action ---
  if (is.function(action)) {
    action(pkg      = c(missing_pkg, needs_update),
           version  = version,
           compare  = compare,
           reason   = reason)
    return(invisible(FALSE))
  }

  # --- interactive: offer to install ---
  all_needed <- unique(c(missing_pkg, needs_update))

  if (interactive()) {
    message(msg)
    ans <- utils::menu(
      choices = c("Yes", "No"),
      title   = sprintf(
        "\nWould you like to install/update %s now?",
        paste(all_needed, collapse = ", ")
      )
    )

    if (identical(ans, 1L)) {
      utils::install.packages(all_needed)

      # re-check after installation
      still_bad <- character(0L)
      for (p in all_needed) {
        if (!requireNamespace(p, quietly = TRUE)) {
          still_bad <- c(still_bad, p)
        }
      }
      if (length(still_bad) > 0L) {
        stop(
          sprintf(
            "Installation failed for: %s",
            paste(still_bad, collapse = ", ")
          ),
          call. = FALSE
        )
      }
      return(invisible(TRUE))
    }
  }

  # --- non-interactive or user declined ---
  install_cmd <- sprintf(
    'install.packages(%s)',
    if (length(all_needed) == 1L) {
      sprintf('"%s"', all_needed)
    } else {
      sprintf('c(%s)', paste(sprintf('"%s"', all_needed), collapse = ", "))
    }
  )

  stop(
    sprintf("%s\n\nRun `%s` to install.", msg, install_cmd),
    call. = FALSE
  )
}


# --- Core utility ---

#' Extract Bit-Position Components from a Sum of Powers of Two
#'
#' Given an integer \code{x} that represents a sum of distinct powers of two,
#' this function returns the zero-based bit positions (exponents) that compose
#' the sum. For example, \code{extractBitSum(5)} returns \code{c(2, 0)}
#' because \eqn{5 = 2^2 + 2^0}.
#'
#' @param x A non-negative integer representing a sum of powers of two.
#'
#' @return An integer vector of zero-based bit positions (exponents) whose
#'   corresponding powers of two sum to \code{x}, returned in descending order.
#'
#' @keywords internal
#' @noRd
extractBitSum <- function(x) {
  lengthVar <- round(log(x = x, base = 2) + 2)
  series <- c(2^(0:lengthVar))
  remainder <- x
  combination <- c()
  while (remainder != 0) {
    component <- match(TRUE, series > remainder) - 1
    remainder <- remainder - series[component]
    combination[length(combination) + 1] <- component - 1
  }
  return(combination)
}
