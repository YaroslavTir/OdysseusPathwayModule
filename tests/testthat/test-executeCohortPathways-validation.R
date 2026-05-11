test_that("executeCohortPathways validates schema type before connecting", {
  expect_error(
    OdysseusPathwayModule::executeCohortPathways(
      cohortDatabaseSchema = 1,
      targetCohortIds = 1,
      eventCohortIds = 1
    ),
    "cohortDatabaseSchema"
  )
})

test_that("executeCohortPathways validates pre-index lookback window order", {
  expect_error(
    OdysseusPathwayModule::executeCohortPathways(
      cohortDatabaseSchema = "main",
      targetCohortIds = 1,
      eventCohortIds = 1,
      analysisType = "pre-index",
      lookbackStartDay = -1,
      lookbackEndDay = -365
    ),
    "must be less than"
  )
})

test_that("executeCohortPathways validates analysisType choices", {
  expect_error(
    OdysseusPathwayModule::executeCohortPathways(
      cohortDatabaseSchema = "main",
      targetCohortIds = 1,
      eventCohortIds = 1,
      analysisType = "during-index"
    ),
    "should be one of"
  )
})

test_that("executeCohortPathways validates numeric controls", {
  expect_error(
    OdysseusPathwayModule::executeCohortPathways(
      cohortDatabaseSchema = "main",
      targetCohortIds = 1,
      eventCohortIds = 1,
      minCellCount = -1
    ),
    "minCellCount"
  )

  expect_error(
    OdysseusPathwayModule::executeCohortPathways(
      cohortDatabaseSchema = "main",
      targetCohortIds = 1,
      eventCohortIds = 1,
      maxDepth = 0
    ),
    "maxDepth"
  )

  expect_error(
    OdysseusPathwayModule::executeCohortPathways(
      cohortDatabaseSchema = "main",
      targetCohortIds = 1,
      eventCohortIds = 1,
      collapseWindow = -10
    ),
    "collapseWindow"
  )
})

test_that("executeCohortPathways validates allowRepeats flag", {
  expect_error(
    OdysseusPathwayModule::executeCohortPathways(
      cohortDatabaseSchema = "main",
      targetCohortIds = 1,
      eventCohortIds = 1,
      allowRepeats = "FALSE"
    ),
    "allowRepeats"
  )
})

test_that("executeCohortPathways validates pre-index day sign", {
  expect_error(
    OdysseusPathwayModule::executeCohortPathways(
      cohortDatabaseSchema = "main",
      targetCohortIds = 1,
      eventCohortIds = 1,
      analysisType = "pre-index",
      lookbackStartDay = 0,
      lookbackEndDay = -1
    ),
    "lookbackStartDay"
  )

  expect_error(
    OdysseusPathwayModule::executeCohortPathways(
      cohortDatabaseSchema = "main",
      targetCohortIds = 1,
      eventCohortIds = 1,
      analysisType = "pre-index",
      lookbackStartDay = -30,
      lookbackEndDay = 0
    ),
    "lookbackEndDay"
  )
})

test_that("core package no longer exports plotting helpers", {
  exports <- getNamespaceExports("OdysseusPathwayModule")

  expect_false("createPathwaySunburst" %in% exports)
  expect_false("createPathwaySankey" %in% exports)
  expect_true("executeCohortPathways" %in% exports)
})