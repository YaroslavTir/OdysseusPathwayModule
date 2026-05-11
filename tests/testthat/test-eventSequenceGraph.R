# --- Shared test fixture ---

.make_cpResults <- function(pathsData, codesLong, isCombo,
                            comboIds = NULL) {
  if (is.null(comboIds)) {
    stepCols <- grep("^step", names(pathsData), value = TRUE)
    vals <- unlist(pathsData[, stepCols], use.names = FALSE)
    comboIds <- sort(unique(vals[!is.na(vals) & vals > 0]))
  }
  list(
    pathwayAnalysisStatsData = data.frame(
      pathwayAnalysisGenerationId = 1L, targetCohortId = 10L, countValue = 20L
    ),
    pathwaysAnalysisPathsData = pathsData,
    pathwaysAnalysisEventsData = data.frame(eventCohortId = 1L, countValue = 10L),
    pathwaycomboIds = data.frame(comboIds = comboIds),
    pathwayAnalysisCodesLong = codesLong,
    isCombo = isCombo,
    pathwayAnalysisCodesData = data.frame(
      pathwayAnalysisGenerationId = rep(1L, nrow(codesLong)),
      code = codesLong$code,
      isCombo = codesLong$isCombo
    )
  )
}

.basic_generationSet <- data.frame(
  cohortId  = c(1L, 2L, 10L),
  cohortName = c("Celecoxib", "Diclofenac", "NSAIDs"),
  stringsAsFactors = FALSE
)

.basic_codesLong <- data.frame(
  pathwayAnalysisGenerationId = c(1L, 1L),
  code = c(2L, 4L),
  targetCohortId = c(10L, 10L),
  eventCohortId = c(1L, 2L),
  isCombo = c(0L, 0L),
  numberOfEvents = c(1L, 1L)
)

.basic_isCombo <- data.frame(
  targetCohortId = c(10L, 10L),
  comboId = c(2L, 4L),
  numberOfEvents = c(1L, 1L),
  isCombo = c(0L, 0L)
)


# --- Tests ---

test_that("buildEventSequenceGraph returns igraph-based structure", {
  cpResults <- .make_cpResults(
    pathsData = data.frame(
      pathwayAnalysisGenerationId = c(1L, 1L, 1L),
      targetCohortId = c(10L, 10L, 10L),
      step1 = c(2L, 4L, 2L),
      step2 = c(4L, 2L, NA),
      step3 = c(NA, NA, NA),
      countValue = c(10L, 5L, 3L)
    ),
    codesLong = .basic_codesLong,
    isCombo   = .basic_isCombo
  )

  esg <- buildEventSequenceGraph(cpResults, .basic_generationSet)

  expect_s3_class(esg, "event_sequence_graph")
  expect_named(esg, c("graph", "sequences", "summary", "eventNameLookup"))

  # igraph object
  expect_s3_class(esg$graph, "igraph")
  expect_true(igraph::is_directed(esg$graph))

  # Vertex attributes
  expect_true("eventName" %in% igraph::vertex_attr_names(esg$graph))
  expect_true("step"      %in% igraph::vertex_attr_names(esg$graph))
  expect_true("count"     %in% igraph::vertex_attr_names(esg$graph))
  expect_true("share"     %in% igraph::vertex_attr_names(esg$graph))

  # Edge attributes
  expect_true("weight"      %in% igraph::edge_attr_names(esg$graph))
  expect_true("probability" %in% igraph::edge_attr_names(esg$graph))
  expect_true("sourceStep"  %in% igraph::edge_attr_names(esg$graph))
  expect_true("targetStep"  %in% igraph::edge_attr_names(esg$graph))

  # Sequences
  expect_s3_class(esg$sequences, "data.frame")
  expect_true("pathway" %in% names(esg$sequences))
  expect_true("depth"   %in% names(esg$sequences))

  # Summary
  expect_equal(esg$summary$totalPathways, 3)
  expect_equal(esg$summary$totalPatients, 18)
  expect_equal(esg$summary$uniqueEvents, 2)
  expect_true(esg$summary$vertexCount > 0)
  expect_true(esg$summary$edgeCount > 0)
})


test_that("igraph vertex names encode event + step", {
  cpResults <- .make_cpResults(
    pathsData = data.frame(
      pathwayAnalysisGenerationId = 1L,
      targetCohortId = 10L,
      step1 = 2L, step2 = 4L,
      countValue = 10L
    ),
    codesLong = .basic_codesLong,
    isCombo   = .basic_isCombo
  )

  esg <- buildEventSequenceGraph(cpResults, .basic_generationSet)
  vnames <- igraph::V(esg$graph)$name

  expect_true(all(grepl("\\[Step \\d+\\]$", vnames)))
  expect_true("Celecoxib [Step 1]" %in% vnames)
  expect_true("Diclofenac [Step 2]" %in% vnames)
})


test_that("igraph edge weights match patient counts and probabilities sum to 1", {
  cpResults <- .make_cpResults(
    pathsData = data.frame(
      pathwayAnalysisGenerationId = c(1L, 1L, 1L),
      targetCohortId = c(10L, 10L, 10L),
      step1 = c(2L, 2L, 4L),
      step2 = c(4L, 2L, 2L),
      countValue = c(10L, 8L, 5L)
    ),
    codesLong = .basic_codesLong,
    isCombo   = .basic_isCombo
  )

  esg <- buildEventSequenceGraph(cpResults, .basic_generationSet)
  ig <- esg$graph

  # Weights are positive integers
  expect_true(all(igraph::E(ig)$weight > 0))

  # Probabilities per source-step must sum to 1
  el <- igraph::as_data_frame(ig, what = "edges")
  for (s in unique(el$sourceStep)) {
    stepEdges <- el[el$sourceStep == s, ]
    for (src in unique(igraph::tail_of(ig, igraph::E(ig)[el$sourceStep == s])$name)) {
      srcEdges <- el[el$sourceStep == s & el$from == src, ]
      expect_equal(sum(srcEdges$probability), 1, tolerance = 1e-10)
    }
  }
})


test_that("buildEventSequenceGraph decodes combo events correctly", {
  comboCodesLong <- data.frame(
    pathwayAnalysisGenerationId = c(1L, 1L, 1L, 1L),
    code = c(2L, 4L, 6L, 6L),
    targetCohortId = c(10L, 10L, 10L, 10L),
    eventCohortId = c(1L, 2L, 1L, 2L),
    isCombo = c(0L, 0L, 1L, 1L),
    numberOfEvents = c(1L, 1L, 2L, 2L)
  )
  comboIsCombo <- data.frame(
    targetCohortId = c(10L, 10L, 10L),
    comboId = c(2L, 4L, 6L),
    numberOfEvents = c(1L, 1L, 2L),
    isCombo = c(0L, 0L, 1L)
  )

  cpResults <- .make_cpResults(
    pathsData = data.frame(
      pathwayAnalysisGenerationId = 1L,
      targetCohortId = 10L,
      step1 = 6L, step2 = 2L,
      countValue = 10L
    ),
    codesLong = comboCodesLong,
    isCombo   = comboIsCombo
  )

  esg <- buildEventSequenceGraph(cpResults, .basic_generationSet)

  expect_equal(esg$eventNameLookup[["6"]], "Celecoxib + Diclofenac")
  expect_equal(esg$eventNameLookup[["2"]], "Celecoxib")
  expect_true(any(grepl("Celecoxib \\+ Diclofenac", esg$sequences$pathway)))
  expect_true(any(grepl("Celecoxib \\+ Diclofenac", igraph::V(esg$graph)$eventName)))
})


test_that("buildEventSequenceGraph respects maxSteps parameter", {
  cpResults <- .make_cpResults(
    pathsData = data.frame(
      pathwayAnalysisGenerationId = 1L,
      targetCohortId = 10L,
      step1 = 2L, step2 = 4L, step3 = 2L,
      countValue = 10L
    ),
    codesLong = .basic_codesLong,
    isCombo   = .basic_isCombo
  )

  esg <- buildEventSequenceGraph(cpResults, .basic_generationSet, maxSteps = 2)

  expect_equal(esg$summary$maxDepth, 2)
  expect_true(all(igraph::E(esg$graph)$sourceStep == 1))
  expect_true(all(igraph::E(esg$graph)$targetStep == 2))
  expect_true(max(igraph::V(esg$graph)$step) <= 2)
})


test_that("buildEventSequenceGraph filters by minCount", {
  cpResults <- .make_cpResults(
    pathsData = data.frame(
      pathwayAnalysisGenerationId = c(1L, 1L),
      targetCohortId = c(10L, 10L),
      step1 = c(2L, 4L),
      step2 = c(4L, 2L),
      countValue = c(15L, 3L)
    ),
    codesLong = .basic_codesLong,
    isCombo   = .basic_isCombo
  )

  esg <- buildEventSequenceGraph(cpResults, .basic_generationSet, minCount = 5)

  expect_equal(esg$summary$totalPathways, 1)
  expect_equal(esg$summary$totalPatients, 15)
})


test_that("igraph supports standard downstream analysis", {
  cpResults <- .make_cpResults(
    pathsData = data.frame(
      pathwayAnalysisGenerationId = c(1L, 1L),
      targetCohortId = c(10L, 10L),
      step1 = c(2L, 4L),
      step2 = c(4L, 2L),
      countValue = c(10L, 5L)
    ),
    codesLong = .basic_codesLong,
    isCombo   = .basic_isCombo
  )

  esg <- buildEventSequenceGraph(cpResults, .basic_generationSet)
  ig <- esg$graph

  # as_data_frame round-trip
  edgeDf <- igraph::as_data_frame(ig, what = "edges")
  expect_true("weight" %in% names(edgeDf))
  expect_true("probability" %in% names(edgeDf))

  vertDf <- igraph::as_data_frame(ig, what = "vertices")
  expect_true("eventName" %in% names(vertDf))
  expect_true("step" %in% names(vertDf))

  # degree
  outDeg <- igraph::degree(ig, mode = "out")
  inDeg  <- igraph::degree(ig, mode = "in")
  expect_type(outDeg, "double")
  expect_type(inDeg, "double")

  # shortest paths (inverted weights)
  dists <- igraph::distances(ig, weights = 1 / igraph::E(ig)$weight)
  expect_true(is.matrix(dists))

  # neighborhood
  nbrs <- igraph::neighbors(ig, igraph::V(ig)[1], mode = "out")
  expect_true(length(nbrs) >= 0)
})


test_that("igraph vertex shares sum to 1 within each step", {
  cpResults <- .make_cpResults(
    pathsData = data.frame(
      pathwayAnalysisGenerationId = c(1L, 1L, 1L),
      targetCohortId = c(10L, 10L, 10L),
      step1 = c(2L, 2L, 4L),
      step2 = c(4L, 2L, 2L),
      countValue = c(10L, 8L, 5L)
    ),
    codesLong = .basic_codesLong,
    isCombo   = .basic_isCombo
  )

  esg <- buildEventSequenceGraph(cpResults, .basic_generationSet)
  vdf <- igraph::as_data_frame(esg$graph, what = "vertices")

  for (s in unique(vdf$step)) {
    stepVerts <- vdf[vdf$step == s, ]
    expect_equal(sum(stepVerts$share), 1, tolerance = 1e-10)
  }
})


test_that("buildEventSequenceGraph errors on missing cpResults components", {
  expect_error(
    buildEventSequenceGraph(
      cpResults = list(pathwaysAnalysisPathsData = data.frame()),
      generationSet = .basic_generationSet
    ),
    "missing required components"
  )
})


test_that("buildEventSequenceGraph errors on invalid generationSet", {
  cpResults <- list(
    pathwaysAnalysisPathsData = data.frame(),
    pathwayAnalysisCodesLong = data.frame(),
    isCombo = data.frame()
  )

  expect_error(
    buildEventSequenceGraph(cpResults, generationSet = data.frame(id = 1, name = "A")),
    "cohortId.*cohortName"
  )

  expect_error(
    buildEventSequenceGraph(cpResults, generationSet = "not a data frame"),
    "data.frame"
  )
})


test_that("print.event_sequence_graph produces output", {
  cpResults <- .make_cpResults(
    pathsData = data.frame(
      pathwayAnalysisGenerationId = 1L,
      targetCohortId = 10L,
      step1 = 2L, step2 = 4L,
      countValue = 10L
    ),
    codesLong = .basic_codesLong,
    isCombo   = .basic_isCombo
  )

  esg <- buildEventSequenceGraph(cpResults, .basic_generationSet)

  output <- capture.output(print(esg))
  expect_true(any(grepl("Event Sequence Graph", output)))
  expect_true(any(grepl("igraph", output)))
  expect_true(any(grepl("Celecoxib", output)))
  expect_true(any(grepl("Vertices", output)))
  expect_true(any(grepl("Edges", output)))
})


test_that("plot.event_sequence_graph runs without error", {
  cpResults <- .make_cpResults(
    pathsData = data.frame(
      pathwayAnalysisGenerationId = c(1L, 1L),
      targetCohortId = c(10L, 10L),
      step1 = c(2L, 4L),
      step2 = c(4L, 2L),
      countValue = c(10L, 5L)
    ),
    codesLong = .basic_codesLong,
    isCombo   = .basic_isCombo
  )

  esg <- buildEventSequenceGraph(cpResults, .basic_generationSet)

  # Plot to null device
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  result <- plot(esg)

  # Returns the igraph invisibly
  expect_s3_class(result, "igraph")
})


test_that("buildEventSequenceGraph errors when cpResults is not a list", {
  expect_error(
    buildEventSequenceGraph(cpResults = "not_a_list", generationSet = .basic_generationSet),
    "must be a list"
  )
})

test_that("buildEventSequenceGraph errors on invalid maxSteps", {
  cpResults <- .make_cpResults(
    pathsData = data.frame(
      pathwayAnalysisGenerationId = 1L, targetCohortId = 10L,
      step1 = 2L, step2 = 4L, countValue = 10L
    ),
    codesLong = .basic_codesLong, isCombo = .basic_isCombo
  )
  expect_error(
    buildEventSequenceGraph(cpResults, .basic_generationSet, maxSteps = -1),
    "maxSteps"
  )
})

test_that("buildEventSequenceGraph errors on invalid minCount", {
  cpResults <- .make_cpResults(
    pathsData = data.frame(
      pathwayAnalysisGenerationId = 1L, targetCohortId = 10L,
      step1 = 2L, step2 = 4L, countValue = 10L
    ),
    codesLong = .basic_codesLong, isCombo = .basic_isCombo
  )
  expect_error(
    buildEventSequenceGraph(cpResults, .basic_generationSet, minCount = -5),
    "minCount"
  )
})

test_that("buildEventSequenceGraph errors when all step columns are NA", {
  cpResults <- .make_cpResults(
    pathsData = data.frame(
      pathwayAnalysisGenerationId = 1L, targetCohortId = 10L,
      step1 = NA_integer_, step2 = NA_integer_, countValue = 10L
    ),
    codesLong = .basic_codesLong, isCombo = .basic_isCombo,
    comboIds = 2L
  )
  expect_error(
    buildEventSequenceGraph(cpResults, .basic_generationSet),
    "No non-empty step columns"
  )
})

test_that("buildEventSequenceGraph errors when minCount filters all rows", {
  cpResults <- .make_cpResults(
    pathsData = data.frame(
      pathwayAnalysisGenerationId = 1L, targetCohortId = 10L,
      step1 = 2L, step2 = 4L, countValue = 1L
    ),
    codesLong = .basic_codesLong, isCombo = .basic_isCombo
  )
  expect_error(
    buildEventSequenceGraph(cpResults, .basic_generationSet, minCount = 100),
    "No pathways remain"
  )
})

test_that("buildEventSequenceGraph errors for single-step-only data", {
  cpResults <- .make_cpResults(
    pathsData = data.frame(
      pathwayAnalysisGenerationId = 1L, targetCohortId = 10L,
      step1 = 2L, countValue = 10L
    ),
    codesLong = .basic_codesLong, isCombo = .basic_isCombo
  )
  expect_error(
    buildEventSequenceGraph(cpResults, .basic_generationSet),
    "No transitions could be created"
  )
})

test_that("plot.event_sequence_graph handles uniform weights and counts", {
  cpResults <- .make_cpResults(
    pathsData = data.frame(
      pathwayAnalysisGenerationId = c(1L, 1L),
      targetCohortId = c(10L, 10L),
      step1 = c(2L, 2L),
      step2 = c(4L, 4L),
      countValue = c(10L, 10L)
    ),
    codesLong = .basic_codesLong, isCombo = .basic_isCombo
  )

  esg <- buildEventSequenceGraph(cpResults, .basic_generationSet)

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  result <- plot(esg, main = "Uniform test")
  expect_s3_class(result, "igraph")
})
