mock_dbconnector <- function(mocks, code) {
  ns <- asNamespace("DatabaseConnector")
  original <- list()

  for (name in names(mocks)) {
    original[[name]] <- get(name, envir = ns)
    unlockBinding(name, ns)
    assign(name, mocks[[name]], envir = ns)
    lockBinding(name, ns)
  }

  on.exit({
    for (name in names(original)) {
      unlockBinding(name, ns)
      assign(name, original[[name]], envir = ns)
      lockBinding(name, ns)
    }
  }, add = TRUE)

  force(code)
}


test_that("executeCohortPathways runs full post-index flow with mocked DatabaseConnector", {
  query_mock <- function(connection, sql, cohort_ids = NULL, ...) {
    if (grepl("cohort_definition_id AS cohort_id", sql, fixed = TRUE)) {
      return(data.frame(
        cohortId = as.integer(cohort_ids),
        cohortEntries = rep(10L, length(cohort_ids)),
        cohortSubjects = rep(9L, length(cohort_ids))
      ))
    }

    if (grepl("SELECT * FROM #pa_stats", sql, fixed = TRUE)) {
      return(data.frame(
        pathwayAnalysisGenerationId = 1L,
        targetCohortId = 10L,
        countValue = 5L
      ))
    }

    if (grepl("SELECT * FROM #pa_paths", sql, fixed = TRUE)) {
      return(data.frame(
        step1 = c(2L, 6L),
        step2 = c(0L, 0L),
        countValue = c(5L, 3L)
      ))
    }

    if (grepl("SELECT * FROM #pa_events", sql, fixed = TRUE)) {
      return(data.frame(eventCohortId = c(1L, 2L), countValue = c(5L, 3L)))
    }

    stop("Unexpected SQL query in mock.")
  }

  execute_calls <- list()
  exec_mock <- function(...) {
    execute_calls[[length(execute_calls) + 1]] <<- list(...)
    invisible(NULL)
  }

  get_tables_mock <- function(...) c("cohort")

  result <- mock_dbconnector(
    mocks = list(
      getTableNames = get_tables_mock,
      renderTranslateQuerySql = query_mock,
      renderTranslateExecuteSql = exec_mock
    ),
    code = OdysseusPathwayModule::executeCohortPathways(
      connection = structure(list(), class = "mock_connection"),
      cohortDatabaseSchema = "main",
      cohortTableName = "cohort",
      outcomeDatabaseSchema = "main",
      outcomeTableName = "cohort",
      targetCohortIds = c(10L),
      eventCohortIds = c(1L, 2L),
      analysisType = "post-index",
      maxDepth = 2,
      collapseWindow = 30
    )
  )

  expect_type(result, "list")
  expect_named(result, c(
    "pathwayAnalysisStatsData",
    "pathwaysAnalysisPathsData",
    "pathwaysAnalysisEventsData",
    "pathwaycomboIds",
    "pathwayAnalysisCodesLong",
    "isCombo",
    "pathwayAnalysisCodesData"
  ))

  expect_gt(nrow(result$pathwaysAnalysisPathsData), 0)
  expect_gt(nrow(result$pathwaycomboIds), 0)
  expect_true(any(result$pathwayAnalysisCodesLong$code > 0))

  # At least one main execution call plus one cleanup call.
  expect_gte(length(execute_calls), 2)
})


test_that("executeCohortPathways passes pre-index lookback params to SQL execution", {
  query_mock <- function(connection, sql, cohort_ids = NULL, ...) {
    if (grepl("cohort_definition_id AS cohort_id", sql, fixed = TRUE)) {
      return(data.frame(
        cohortId = as.integer(cohort_ids),
        cohortEntries = rep(10L, length(cohort_ids)),
        cohortSubjects = rep(9L, length(cohort_ids))
      ))
    }

    if (grepl("SELECT * FROM #pa_stats", sql, fixed = TRUE)) {
      return(data.frame(pathwayAnalysisGenerationId = 1L, targetCohortId = 10L, countValue = 5L))
    }

    if (grepl("SELECT * FROM #pa_paths", sql, fixed = TRUE)) {
      return(data.frame(step1 = 2L, countValue = 5L))
    }

    if (grepl("SELECT * FROM #pa_events", sql, fixed = TRUE)) {
      return(data.frame(eventCohortId = 1L, countValue = 5L))
    }

    stop("Unexpected SQL query in mock.")
  }

  execute_calls <- list()
  exec_mock <- function(...) {
    execute_calls[[length(execute_calls) + 1]] <<- list(...)
    invisible(NULL)
  }

  get_tables_mock <- function(...) c("cohort")

  mock_dbconnector(
    mocks = list(
      getTableNames = get_tables_mock,
      renderTranslateQuerySql = query_mock,
      renderTranslateExecuteSql = exec_mock
    ),
    code = OdysseusPathwayModule::executeCohortPathways(
      connection = structure(list(), class = "mock_connection"),
      cohortDatabaseSchema = "main",
      cohortTableName = "cohort",
      outcomeDatabaseSchema = "main",
      outcomeTableName = "cohort",
      targetCohortIds = 10L,
      eventCohortIds = c(1L, 2L),
      analysisType = "pre-index",
      lookbackStartDay = -90,
      lookbackEndDay = -1,
      maxDepth = 2,
      collapseWindow = 30
    )
  )

  non_cleanup <- execute_calls[vapply(
    execute_calls,
    function(x) is.null(x$lookback_start_day) == FALSE,
    logical(1)
  )]

  expect_gte(length(non_cleanup), 1)
  expect_equal(non_cleanup[[1]]$lookback_start_day, -90)
  expect_equal(non_cleanup[[1]]$lookback_end_day, -1)
})


test_that("executeCohortPathways errors when cohort table is not present", {
  get_tables_mock <- function(...) c("other_table")

  query_mock <- function(...) {
    stop("Should not query counts if table lookup fails")
  }

  exec_mock <- function(...) {
    invisible(NULL)
  }

  expect_error(
    mock_dbconnector(
      mocks = list(
        getTableNames = get_tables_mock,
        renderTranslateQuerySql = query_mock,
        renderTranslateExecuteSql = exec_mock
      ),
      code = OdysseusPathwayModule::executeCohortPathways(
        connection = structure(list(), class = "mock_connection"),
        cohortDatabaseSchema = "main",
        cohortTableName = "cohort",
        targetCohortIds = 10L,
        eventCohortIds = 1L
      )
    ),
    "Target cohort table"
  )
})

test_that("executeCohortPathways returns empty code tables when no positive combos are present", {
  query_mock <- function(connection, sql, cohort_ids = NULL, ...) {
    if (grepl("cohort_definition_id AS cohort_id", sql, fixed = TRUE)) {
      return(data.frame(
        cohortId = as.integer(cohort_ids),
        cohortEntries = rep(10L, length(cohort_ids)),
        cohortSubjects = rep(9L, length(cohort_ids))
      ))
    }

    if (grepl("SELECT * FROM #pa_stats", sql, fixed = TRUE)) {
      return(data.frame(pathwayAnalysisGenerationId = 1L, targetCohortId = 10L, countValue = 5L))
    }

    if (grepl("SELECT * FROM #pa_paths", sql, fixed = TRUE)) {
      return(data.frame(step1 = 0L, step2 = NA_integer_, countValue = 5L))
    }

    if (grepl("SELECT * FROM #pa_events", sql, fixed = TRUE)) {
      return(data.frame(eventCohortId = integer(), countValue = integer()))
    }

    stop("Unexpected SQL query in mock.")
  }

  get_tables_mock <- function(...) c("cohort")
  exec_mock <- function(...) invisible(NULL)

  result <- mock_dbconnector(
    mocks = list(
      getTableNames = get_tables_mock,
      renderTranslateQuerySql = query_mock,
      renderTranslateExecuteSql = exec_mock
    ),
    code = OdysseusPathwayModule::executeCohortPathways(
      connection = structure(list(), class = "mock_connection"),
      cohortDatabaseSchema = "main",
      cohortTableName = "cohort",
      targetCohortIds = 10L,
      eventCohortIds = c(1L, 2L),
      analysisType = "pre-index",
      lookbackStartDay = -365,
      lookbackEndDay = -1,
      maxDepth = 2,
      collapseWindow = 30
    )
  )

  expect_equal(nrow(result$pathwaycomboIds), 0)
  expect_equal(nrow(result$pathwayAnalysisCodesLong), 0)
  expect_equal(nrow(result$pathwayAnalysisCodesData), 0)
  expect_equal(nrow(result$isCombo), 0)
})
