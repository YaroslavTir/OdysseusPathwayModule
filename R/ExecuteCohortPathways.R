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


#' Execute cohort pathway analysis.
#'
#' @description
#' Runs the cohort pathways on all instantiated combinations of target and event cohorts.
#' Assumes the cohorts have already been instantiated.
#'
#' Supports two analysis types:
#' \itemize{
#'   \item \code{"post-index"} (default): events occurring **after** the target cohort
#'         index date (within the target cohort period).
#'   \item \code{"pre-index"}: events occurring **before** the target cohort index date,
#'         in a configurable lookback window. For example, what events occurred in
#'         the 365 days before cohort entry.
#' }
#'
#' Target and event cohorts can reside in different schemas/tables:
#' \itemize{
#'   \item \code{cohortDatabaseSchema} / \code{cohortTableName} for target cohorts.
#'   \item \code{outcomeDatabaseSchema} / \code{outcomeTableName} for event (outcome) cohorts.
#'         If not specified, defaults to the same as the target.
#' }
#'
#' @param connectionDetails   An object of type connectionDetails as created using the
#'                            \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                            DatabaseConnector package. Can be left NULL if \code{connection} is
#'                            provided.
#' @param connection          An object of type \code{connection} as created using the
#'                            \code{\link[DatabaseConnector]{connect}} function in the
#'                            DatabaseConnector package. Can be left NULL if connectionDetails
#'                            is provided, in which case a new connection will be opened at the start
#'                            of the function, and closed when the function finishes.
#' @param cohortDatabaseSchema   Schema name where your **target** cohort table resides.
#'                               For SQL Server this should include both database and schema,
#'                               e.g. 'scratch.dbo'.
#' @param cohortTableName        The name of the **target** cohort table (default: "cohort").
#' @param outcomeDatabaseSchema  Schema name where your **event/outcome** cohort table resides.
#'                               Defaults to \code{cohortDatabaseSchema} if not specified.
#' @param outcomeTableName       The name of the **event/outcome** cohort table.
#'                               Defaults to \code{cohortTableName} if not specified.
#' @param targetCohortIds     A vector of one or more Cohort Ids corresponding to target cohort(s).
#' @param eventCohortIds      A vector of one or more Cohort Ids corresponding to event cohort(s).
#' @param analysisType        Character. Either \code{"post-index"} (default) to analyze events
#'                            after the index date, or \code{"pre-index"} to analyze events
#'                            before the index date.
#' @param lookbackStartDay    (Only used when \code{analysisType = "pre-index"}.)
#'                            Start of the lookback window in days relative to the index date
#'                            (default: -365). Must be negative.
#' @param lookbackEndDay      (Only used when \code{analysisType = "pre-index"}.)
#'                            End of the lookback window in days relative to the index date
#'                            (default: -1). Must be negative.
#' @param tempEmulationSchema   Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#' @param minCellCount          (Default = 5) The minimum cell count for fields containing person
#'                              counts or fractions.
#' @param allowRepeats          (Default = FALSE) Allow cohort events/combos to appear multiple
#'                              times in the same pathway.
#' @param maxDepth              (Default = 5) Maximum number of steps in a given pathway.
#' @param collapseWindow        (Default = 30) Any dates found within the specified collapse days
#'                              will be reassigned the earliest date.
#' @return                      A list of data frames containing pathway analysis results.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("Eunomia", quietly = TRUE) &&
#'     exists("createCohorts", where = asNamespace("Eunomia"), inherits = FALSE)) {
#'   connectionDetails <- Eunomia::getEunomiaConnectionDetails()
#'   createCohorts <- get("createCohorts", envir = asNamespace("Eunomia"))
#'   createCohorts(connectionDetails)
#'
#'   postIndexResults <- executeCohortPathways(
#'     connectionDetails = connectionDetails,
#'     cohortDatabaseSchema = "main",
#'     cohortTableName = "cohort",
#'     targetCohortIds = 4,
#'     eventCohortIds = c(1, 2, 3),
#'     maxDepth = 3
#'   )
#'
#'   preIndexResults <- executeCohortPathways(
#'     connectionDetails = connectionDetails,
#'     cohortDatabaseSchema = "main",
#'     cohortTableName = "cohort",
#'     targetCohortIds = 4,
#'     eventCohortIds = c(1, 2, 3),
#'     analysisType = "pre-index",
#'     lookbackStartDay = -365,
#'     lookbackEndDay = -1,
#'     maxDepth = 3
#'   )
#'
#'   names(postIndexResults)
#'   names(preIndexResults)
#' }
#' }
#'
#' @export
executeCohortPathways <- function(connectionDetails = NULL,
                                  connection = NULL,
                                  cohortDatabaseSchema,
                                  cohortTableName = "cohort",
                                  outcomeDatabaseSchema = cohortDatabaseSchema,
                                  outcomeTableName = cohortTableName,
                                  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                  targetCohortIds,
                                  eventCohortIds,
                                  analysisType = "post-index",
                                  lookbackStartDay = -365,
                                  lookbackEndDay = -1,
                                  minCellCount = 5,
                                  allowRepeats = FALSE,
                                  maxDepth = 5,
                                  collapseWindow = 30) {
  start <- Sys.time()

  # --- Input validation (base R) ---
  analysisType <- match.arg(analysisType, c("post-index", "pre-index"))

  if (!is.character(cohortDatabaseSchema) || length(cohortDatabaseSchema) < 1) {
    stop("'cohortDatabaseSchema' must be a non-empty character string.")
  }
  if (!is.character(outcomeDatabaseSchema) || length(outcomeDatabaseSchema) < 1) {
    stop("'outcomeDatabaseSchema' must be a non-empty character string.")
  }
  if (!is.numeric(minCellCount) || length(minCellCount) != 1 || minCellCount < 0) {
    stop("'minCellCount' must be a single non-negative number.")
  }
  if (!is.logical(allowRepeats) || length(allowRepeats) != 1) {
    stop("'allowRepeats' must be a single logical value.")
  }
  if (!is.numeric(maxDepth) || length(maxDepth) != 1 || maxDepth < 1) {
    stop("'maxDepth' must be a single positive number.")
  }
  if (!is.numeric(collapseWindow) || length(collapseWindow) != 1 || collapseWindow < 0) {
    stop("'collapseWindow' must be a single non-negative number.")
  }

  if (analysisType == "pre-index") {
    if (!is.numeric(lookbackStartDay) || length(lookbackStartDay) != 1 || lookbackStartDay >= 0) {
      stop("'lookbackStartDay' must be a single negative number.")
    }
    if (!is.numeric(lookbackEndDay) || length(lookbackEndDay) != 1 || lookbackEndDay >= 0) {
      stop("'lookbackEndDay' must be a single negative number.")
    }
    if (lookbackStartDay >= lookbackEndDay) {
      stop(
        paste0(
          "'lookbackStartDay' (", lookbackStartDay,
          ") must be less than 'lookbackEndDay' (", lookbackEndDay, ").",
          " Example: lookbackStartDay = -365, lookbackEndDay = -1."
        )
      )
    }
  }

  message(paste0("Run Cohort Pathways (", analysisType, ") started at ", start))

  if (analysisType == "pre-index") {
    message(
      sprintf(
        "   Pre-index lookback window: [%d, %d] days relative to index date.",
        lookbackStartDay, lookbackEndDay
      )
    )
  }

  # allow repeats is used as text 'true' or 'false' in sql
  allowRepeatsStr <- if (allowRepeats) "true" else "false"

  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }

  # --- Verify target cohort table exists ---
  cohortTableName <- tolower(cohortTableName)
  tablesInCohortSchema <-
    DatabaseConnector::getTableNames(
      connection = connection,
      databaseSchema = cohortDatabaseSchema
    ) |>
    tolower()

  if (!cohortTableName %in% c(tablesInCohortSchema, "")) {
    stop(
      paste0(
        "Target cohort table '", toupper(cohortTableName),
        "' not found in schema '", cohortDatabaseSchema, "'"
      )
    )
  }

  # --- Verify event/outcome cohort table exists ---
  outcomeTableName <- tolower(outcomeTableName)
  tablesInOutcomeSchema <-
    DatabaseConnector::getTableNames(
      connection = connection,
      databaseSchema = outcomeDatabaseSchema
    ) |>
    tolower()

  if (!outcomeTableName %in% c(tablesInOutcomeSchema, "")) {
    stop(
      paste0(
        "Event/outcome cohort table '", toupper(outcomeTableName),
        "' not found in schema '", outcomeDatabaseSchema, "'"
      )
    )
  }

  # --- Count cohorts ---
  # Target cohort counts
  targetCounts <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT cohort_definition_id AS cohort_id,
              COUNT(*) AS cohort_entries,
              COUNT(DISTINCT subject_id) AS cohort_subjects
          FROM @cohort_database_schema.@cohort_table
          {@cohort_ids != ''} ? {WHERE cohort_definition_id IN (@cohort_ids)}
          GROUP BY cohort_definition_id;",
    cohort_database_schema = cohortDatabaseSchema,
    cohort_table = cohortTableName,
    cohort_ids = targetCohortIds,
    snakeCaseToCamelCase = TRUE
  )

  # Event cohort counts
  eventCounts <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT cohort_definition_id AS cohort_id,
              COUNT(*) AS cohort_entries,
              COUNT(DISTINCT subject_id) AS cohort_subjects
          FROM @cohort_database_schema.@cohort_table
          {@cohort_ids != ''} ? {WHERE cohort_definition_id IN (@cohort_ids)}
          GROUP BY cohort_definition_id;",
    cohort_database_schema = outcomeDatabaseSchema,
    cohort_table = outcomeTableName,
    cohort_ids = eventCohortIds,
    snakeCaseToCamelCase = TRUE
  )

  if (nrow(targetCounts) == 0 ||
      !any(targetCounts$cohortId %in% targetCohortIds)) {
    stop("None of the target cohorts are instantiated.")
  }

  if (nrow(eventCounts) == 0 ||
      !any(eventCounts$cohortId %in% eventCohortIds)) {
    stop("None of the event cohorts are instantiated.")
  }

  nTargetFound <- sum(targetCounts$cohortId %in% targetCohortIds)
  nEventFound <- sum(eventCounts$cohortId %in% eventCohortIds)

  if (nTargetFound < length(targetCohortIds) ||
      nEventFound < length(eventCohortIds)) {
    message("Not all cohorts have more than 0 records.")
    message(
      sprintf("    Found %s of %s target cohorts instantiated.", nTargetFound, length(targetCohortIds))
    )
    message(
      sprintf("    Found %s of %s event cohorts instantiated.", nEventFound, length(eventCohortIds))
    )
  }

  targetCohortTable <- paste0(cohortDatabaseSchema, ".", cohortTableName)
  eventCohortTable <- paste0(outcomeDatabaseSchema, ".", outcomeTableName)

  instantiatedEventCohortIds <-
    intersect(x = eventCohortIds, y = eventCounts$cohortId)
  instantiatedTargetCohortIds <-
    intersect(x = targetCohortIds, y = targetCounts$cohortId)

  # --- Select SQL based on analysis type ---
  sqlFile <- if (analysisType == "pre-index") {
    "RunPreIndexPathwayAnalysis.sql"
  } else {
    "RunPathwayAnalysis.sql"
  }

  pathwayAnalysisSql <-
    SqlRender::readSql(
      sourceFile = system.file(
        "sql", "sql_server", sqlFile,
        package = utils::packageName()
      )
    )

  generationToTarget <- data.frame(
    pathwayAnalysisGenerationId = integer(),
    targetCohortId = integer(),
    stringsAsFactors = FALSE
  )
  eventCohortIdIndexMaps <- data.frame(
    eventCohortId = sort(unique(instantiatedEventCohortIds)),
    stringsAsFactors = FALSE
  )
  eventCohortIdIndexMaps$cohortIndex <- seq_len(nrow(eventCohortIdIndexMaps))

  pathwayAnalysisStatsData <- list()
  pathwaysAnalysisPathsData <- list()
  pathwaysAnalysisEventsData <- list()

  for (i in seq_along(instantiatedTargetCohortIds)) {
    targetCohortId <- instantiatedTargetCohortIds[[i]]

    generationId <-
      (as.integer(format(Sys.Date(), "%Y%m%d")) * 1000) +
      sample(x = 1:1000, size = 1, replace = FALSE)

    eventCohortIdIndexMap <- paste0(
      "SELECT ", eventCohortIdIndexMaps$eventCohortId,
      " AS cohort_definition_id, ", eventCohortIdIndexMaps$cohortIndex,
      " AS cohort_index",
      collapse = " union all "
    )

    message(
      paste0(
        "   Generating ", analysisType, " pathways for target cohort: ",
        targetCohortId,
        ". Generation id: ", generationId, "."
      )
    )

    # Build SQL parameters
    sqlParams <- list(
      connection = connection,
      sql = pathwayAnalysisSql,
      profile = FALSE,
      progressBar = TRUE,
      reportOverallTime = FALSE,
      tempEmulationSchema = tempEmulationSchema,
      allow_repeats = allowRepeatsStr,
      combo_window = collapseWindow,
      max_depth = maxDepth,
      pathway_target_cohort_id = targetCohortId,
      target_cohort_table = targetCohortTable,
      event_cohort_table = eventCohortTable,
      generation_id = generationId,
      event_cohort_id_index_map = eventCohortIdIndexMap
    )

    # Add lookback parameters for pre-index analysis
    if (analysisType == "pre-index") {
      sqlParams$lookback_start_day <- lookbackStartDay
      sqlParams$lookback_end_day <- lookbackEndDay
    }

    do.call(DatabaseConnector::renderTranslateExecuteSql, sqlParams)

    pathwayAnalysisStatsData[[i]] <-
      DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = "SELECT * FROM #pa_stats;",
        snakeCaseToCamelCase = TRUE
      )
    if (!"pathwayAnalysisGenerationId" %in% names(pathwayAnalysisStatsData[[i]])) {
      pathwayAnalysisStatsData[[i]]$pathwayAnalysisGenerationId <- rep(generationId, nrow(pathwayAnalysisStatsData[[i]]))
    }
    if (!"targetCohortId" %in% names(pathwayAnalysisStatsData[[i]])) {
      pathwayAnalysisStatsData[[i]]$targetCohortId <- rep(targetCohortId, nrow(pathwayAnalysisStatsData[[i]]))
    }

    pathwaysAnalysisPathsData[[i]] <-
      DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = "SELECT * FROM #pa_paths;",
        snakeCaseToCamelCase = TRUE
      )
    if (!"pathwayAnalysisGenerationId" %in% names(pathwaysAnalysisPathsData[[i]])) {
      pathwaysAnalysisPathsData[[i]]$pathwayAnalysisGenerationId <- rep(generationId, nrow(pathwaysAnalysisPathsData[[i]]))
    }
    if (!"targetCohortId" %in% names(pathwaysAnalysisPathsData[[i]])) {
      pathwaysAnalysisPathsData[[i]]$targetCohortId <- rep(targetCohortId, nrow(pathwaysAnalysisPathsData[[i]]))
    }

    pathwaysAnalysisEventsData[[i]] <-
      DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = "SELECT * FROM #pa_events;",
        snakeCaseToCamelCase = TRUE
      )
    if (!"pathwayAnalysisGenerationId" %in% names(pathwaysAnalysisEventsData[[i]])) {
      pathwaysAnalysisEventsData[[i]]$pathwayAnalysisGenerationId <- rep(generationId, nrow(pathwaysAnalysisEventsData[[i]]))
    }
    if (!"targetCohortId" %in% names(pathwaysAnalysisEventsData[[i]])) {
      pathwaysAnalysisEventsData[[i]]$targetCohortId <- rep(targetCohortId, nrow(pathwaysAnalysisEventsData[[i]]))
    }

    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = "DROP TABLE IF EXISTS #pa_paths;
             DROP TABLE IF EXISTS #pa_stats;
             DROP TABLE IF EXISTS #pa_events;",
      profile = FALSE,
      progressBar = TRUE,
      reportOverallTime = FALSE,
      tempEmulationSchema = tempEmulationSchema
    )

    generationToTarget <- rbind(
      generationToTarget,
      data.frame(
        pathwayAnalysisGenerationId = generationId,
        targetCohortId = targetCohortId,
        stringsAsFactors = FALSE
      )
    )
  }

  pathwayAnalysisStatsData <- do.call(rbind, pathwayAnalysisStatsData)
  pathwaysAnalysisPathsData <- do.call(rbind, pathwaysAnalysisPathsData)
  pathwaysAnalysisEventsData <- do.call(rbind, pathwaysAnalysisEventsData)

  stepCols <- grep("^step", names(pathwaysAnalysisPathsData), value = TRUE)
  comboIdsAll <- unlist(pathwaysAnalysisPathsData[, stepCols], use.names = FALSE)
  comboIdsVec <- sort(unique(comboIdsAll[!is.na(comboIdsAll) & comboIdsAll > 0]))
  pathwaycomboIds <- data.frame(comboIds = comboIdsVec)

  if (nrow(pathwaysAnalysisPathsData) > 0 && length(stepCols) > 0) {
    comboByGeneration <- do.call(
      rbind,
      lapply(stepCols, function(stepCol) {
        data.frame(
          pathwayAnalysisGenerationId = pathwaysAnalysisPathsData$pathwayAnalysisGenerationId,
          comboId = pathwaysAnalysisPathsData[[stepCol]],
          stringsAsFactors = FALSE
        )
      })
    )
    comboByGeneration <- comboByGeneration[
      !is.na(comboByGeneration$comboId) & comboByGeneration$comboId > 0,
      c("pathwayAnalysisGenerationId", "comboId")
    ]
    comboByGeneration <- unique(comboByGeneration)
    comboByGeneration <- merge(
      comboByGeneration,
      generationToTarget,
      by = "pathwayAnalysisGenerationId"
    )
  } else {
    comboByGeneration <- data.frame(
      pathwayAnalysisGenerationId = integer(),
      comboId = numeric(),
      targetCohortId = integer(),
      stringsAsFactors = FALSE
    )
  }

  if (nrow(pathwaycomboIds) == 0) {
    pathwayAnalysisCodesLong <- data.frame(
      pathwayAnalysisGenerationId = integer(),
      code = numeric(),
      targetCohortId = integer(),
      eventCohortId = integer(),
      isCombo = integer(),
      numberOfEvents = integer()
    )
    isCombo <- data.frame(
      targetCohortId = integer(),
      comboId = numeric(),
      numberOfEvents = integer(),
      isCombo = integer()
    )
    pathwayAnalysisCodesData <- data.frame(
      pathwayAnalysisGenerationId = integer(),
      code = numeric(),
      isCombo = integer()
    )
  } else {
    pathwayAnalysisCodesLong <- NULL
    for (i in seq_len(nrow(comboByGeneration))) {
      cohortIndex <- extractBitSum(x = comboByGeneration$comboId[i])
      combisData <- data.frame(cohortIndex = cohortIndex, stringsAsFactors = FALSE)
      combisData$comboId <- comboByGeneration$comboId[i]
      combisData$targetCohortId <- comboByGeneration$targetCohortId[i]
      combisData$pathwayAnalysisGenerationId <-
        comboByGeneration$pathwayAnalysisGenerationId[i]
      combisData <- merge(combisData, eventCohortIdIndexMaps, by = "cohortIndex")
      pathwayAnalysisCodesLong <- rbind(combisData, pathwayAnalysisCodesLong)
    }

    codesLongSub <- unique(
      pathwayAnalysisCodesLong[, c("targetCohortId", "comboId", "eventCohortId")]
    )
    isCombo <- aggregate(
      eventCohortId ~ targetCohortId + comboId,
      data = codesLongSub,
      FUN = length
    )
    names(isCombo)[names(isCombo) == "eventCohortId"] <- "numberOfEvents"
    isCombo$isCombo <- ifelse(isCombo$numberOfEvents > 1, 1L, 0L)

    pathwayAnalysisCodesLong <- merge(
      pathwayAnalysisCodesLong, isCombo,
      by = c("targetCohortId", "comboId")
    )
    pathwayAnalysisCodesLong <- pathwayAnalysisCodesLong[, c(
      "pathwayAnalysisGenerationId", "comboId", "targetCohortId",
      "eventCohortId", "isCombo", "numberOfEvents"
    )]
    names(pathwayAnalysisCodesLong)[names(pathwayAnalysisCodesLong) == "comboId"] <- "code"

    # code already identifies the same event-cohort combo across all target cohorts
    # (shared eventCohortIdIndexMaps); keying on the per-iteration generationId as well
    # would duplicate a shared code once per target cohort that uses it.
    pathwayAnalysisCodesData <- unique(
      pathwayAnalysisCodesLong[, c("code", "isCombo")]
    )
  }

  allData <- list(
    pathwayAnalysisStatsData = pathwayAnalysisStatsData,
    pathwaysAnalysisPathsData = pathwaysAnalysisPathsData,
    pathwaysAnalysisEventsData = pathwaysAnalysisEventsData,
    pathwaycomboIds = pathwaycomboIds,
    pathwayAnalysisCodesLong = pathwayAnalysisCodesLong,
    isCombo = isCombo,
    pathwayAnalysisCodesData = pathwayAnalysisCodesData
  )

  delta <- Sys.time() - start
  message(
    "Computing Cohort Pathways took ",
    signif(delta, 3), " ", attr(delta, "units")
  )
  return(allData)
}
