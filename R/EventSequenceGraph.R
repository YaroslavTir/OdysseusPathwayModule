# Copyright 2026 Odysseus Data Services
#
# This file is part of OdysseusPathwayModule
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


#' Build an event sequence graph from pathway analysis results.
#'
#' @description
#' Takes the output of \code{\link{executeCohortPathways}} and a cohort name
#' mapping, decodes the bitmask-encoded combo IDs into human-readable event
#' names, and builds a directed \pkg{igraph} graph representing event
#' transitions across pathway steps.
#'
#' Nodes in the graph represent unique \emph{(event, step)} combinations.
#' Edges represent directed transitions between consecutive steps, weighted by
#' patient counts and annotated with transition probabilities.
#'
#' The returned object includes:
#' \itemize{
#'   \item \strong{graph}: A directed, weighted \code{\link[igraph]{igraph}}
#'         object. Vertex attributes: \code{name}, \code{eventName},
#'         \code{step}, \code{count}, \code{share}. Edge attributes:
#'         \code{weight} (= patient count), \code{probability},
#'         \code{sourceStep}, \code{targetStep}.
#'   \item \strong{sequences}: The fully decoded pathway sequences with a
#'         human-readable \code{pathway} string column.
#'   \item \strong{summary}: High-level statistics (total pathways, patients,
#'         depth, unique events, vertex/edge counts).
#'   \item \strong{eventNameLookup}: Named character vector mapping combo IDs
#'         to event names.
#' }
#'
#' @param cpResults   A list returned by \code{\link{executeCohortPathways}}.
#'                    Must contain at least \code{pathwaysAnalysisPathsData},
#'                    \code{pathwayAnalysisCodesLong}, and \code{isCombo}.
#' @param generationSet A data frame with at least two columns:
#'                      \code{cohortId} (integer) and \code{cohortName} (character).
#'                      Used to map event cohort IDs to descriptive names.
#' @param maxSteps    (Optional) Maximum number of steps to include. If \code{NULL}
#'                    (default), all non-empty steps are used.
#' @param minCount    (Default = 1) Minimum patient count for a pathway to be
#'                    included in the output.
#'
#' @return A list of class \code{"event_sequence_graph"} with components
#'         \code{graph} (igraph object), \code{sequences}, \code{summary}, and
#'         \code{eventNameLookup}.
#'
#' @examples
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   cpResults <- list(
#'     pathwaysAnalysisPathsData = data.frame(
#'       pathwayAnalysisGenerationId = c(1L, 1L),
#'       targetCohortId = c(10L, 10L),
#'       step1 = c(2L, 4L),
#'       step2 = c(4L, NA_integer_),
#'       countValue = c(10L, 5L)
#'     ),
#'     pathwayAnalysisCodesLong = data.frame(
#'       pathwayAnalysisGenerationId = c(1L, 1L),
#'       code = c(2L, 4L),
#'       targetCohortId = c(10L, 10L),
#'       eventCohortId = c(1L, 2L),
#'       isCombo = c(0L, 0L),
#'       numberOfEvents = c(1L, 1L)
#'     ),
#'     isCombo = data.frame(
#'       targetCohortId = c(10L, 10L),
#'       comboId = c(2L, 4L),
#'       numberOfEvents = c(1L, 1L),
#'       isCombo = c(0L, 0L)
#'     )
#'   )
#'
#'   generationSet <- data.frame(
#'     cohortId = c(1L, 2L, 10L),
#'     cohortName = c("Celecoxib", "Diclofenac", "NSAIDs")
#'   )
#'
#'   esg <- buildEventSequenceGraph(cpResults, generationSet)
#'   esg$summary
#'   igraph::gorder(esg$graph)
#' }
#'
#' @export
buildEventSequenceGraph <- function(cpResults,
                                    generationSet,
                                    maxSteps = NULL,
                                    minCount = 1) {

  check_installed("igraph", reason = "buildEventSequenceGraph() uses igraph for graph construction.")

  # --- Input validation ---
  if (!is.list(cpResults)) {
    stop("'cpResults' must be a list (output of executeCohortPathways).")
  }
  required <- c("pathwaysAnalysisPathsData", "pathwayAnalysisCodesLong", "isCombo")
  missing <- setdiff(required, names(cpResults))
  if (length(missing) > 0) {
    stop(
      "'cpResults' is missing required components: ",
      paste(missing, collapse = ", "), "."
    )
  }

  if (!is.data.frame(generationSet)) {
    stop("'generationSet' must be a data.frame.")
  }
  if (!all(c("cohortId", "cohortName") %in% names(generationSet))) {
    stop("'generationSet' must contain 'cohortId' and 'cohortName' columns.")
  }

  if (!is.null(maxSteps)) {
    if (!is.numeric(maxSteps) || length(maxSteps) != 1 || maxSteps < 1) {
      stop("'maxSteps' must be a single positive integer or NULL.")
    }
    maxSteps <- as.integer(maxSteps)
  }
  if (!is.numeric(minCount) || length(minCount) != 1 || minCount < 0) {
    stop("'minCount' must be a single non-negative number.")
  }

  # --- Build event name lookup: comboId -> human-readable name ---
  eventNameLookup <- .buildEventNameLookup(
    cpResults$pathwayAnalysisCodesLong,
    generationSet
  )

  # --- Extract and prepare paths data ---
  pathsData <- cpResults$pathwaysAnalysisPathsData
  stepCols <- grep("^step", names(pathsData), value = TRUE)

  # Remove all-NA step columns
  nonEmpty <- stepCols[colSums(!is.na(pathsData[, stepCols, drop = FALSE])) > 0]

  if (length(nonEmpty) == 0) {
    stop("No non-empty step columns found in pathwaysAnalysisPathsData.")
  }

  if (!is.null(maxSteps)) {
    nonEmpty <- nonEmpty[seq_len(min(maxSteps, length(nonEmpty)))]
  }

  # --- Decode step columns to event names ---
  stepNameCols <- character(length(nonEmpty))
  for (j in seq_along(nonEmpty)) {
    col <- nonEmpty[j]
    nameCol <- sub("^step", "stepName", col)
    stepNameCols[j] <- nameCol
    pathsData[[nameCol]] <- eventNameLookup[as.character(pathsData[[col]])]
  }

  # --- Filter by minCount ---
  pathsData <- pathsData[pathsData$countValue >= minCount, , drop = FALSE]
  if (nrow(pathsData) == 0) {
    stop("No pathways remain after filtering by minCount = ", minCount, ".")
  }

  # --- Build edges data frame (transitions between consecutive steps) ---
  edgesList <- list()
  for (i in seq_along(stepNameCols)[-1]) {
    prevCol <- stepNameCols[i - 1]
    currCol <- stepNameCols[i]

    rows <- !is.na(pathsData[[prevCol]]) & !is.na(pathsData[[currCol]])
    if (!any(rows)) next

    subDf <- data.frame(
      sourceName = pathsData[[prevCol]][rows],
      targetName = pathsData[[currCol]][rows],
      countValue = pathsData$countValue[rows],
      stringsAsFactors = FALSE
    )

    agg <- aggregate(countValue ~ sourceName + targetName, data = subDf, FUN = sum)
    agg$sourceStep <- i - 1L
    agg$targetStep <- as.integer(i)

    edgesList[[length(edgesList) + 1]] <- agg
  }

  if (length(edgesList) == 0) {
    stop("No transitions could be created. The data may only contain single-step pathways.")
  }

  edgesDf <- do.call(rbind, edgesList)
  rownames(edgesDf) <- NULL

  # Compute transition probabilities per source within each step transition
  edgesDf$probability <- NA_real_
  for (s in unique(edgesDf$sourceStep)) {
    idx <- edgesDf$sourceStep == s
    for (src in unique(edgesDf$sourceName[idx])) {
      srcIdx <- idx & edgesDf$sourceName == src
      total <- sum(edgesDf$countValue[srcIdx])
      edgesDf$probability[srcIdx] <- edgesDf$countValue[srcIdx] / total
    }
  }

  # --- Build nodes data frame ---
  nodesList <- list()
  for (i in seq_along(stepNameCols)) {
    col <- stepNameCols[i]
    notNA <- !is.na(pathsData[[col]])
    if (!any(notNA)) next

    tmp <- aggregate(
      pathsData$countValue[notNA],
      by = list(eventName = pathsData[[col]][notNA]),
      FUN = sum
    )
    names(tmp) <- c("eventName", "count")
    tmp$step <- as.integer(i)
    nodesList[[length(nodesList) + 1]] <- tmp
  }

  nodesDf <- do.call(rbind, nodesList)
  rownames(nodesDf) <- NULL

  # Compute share within each step
  nodesDf$share <- NA_real_
  for (s in unique(nodesDf$step)) {
    idx <- nodesDf$step == s
    total <- sum(nodesDf$count[idx])
    nodesDf$share[idx] <- nodesDf$count[idx] / total
  }

  # Create stable vertex IDs: "EventName [Step N]"
  nodesDf$name <- paste0(nodesDf$eventName, " [Step ", nodesDf$step, "]")

  # Map edges to vertex IDs
  edgesDf$from <- paste0(edgesDf$sourceName, " [Step ", edgesDf$sourceStep, "]")
  edgesDf$to   <- paste0(edgesDf$targetName, " [Step ", edgesDf$targetStep, "]")

  # --- Construct igraph object ---
  ig <- igraph::graph_from_data_frame(
    d        = edgesDf[, c("from", "to"), drop = FALSE],
    directed = TRUE,
    vertices = nodesDf[, c("name", "eventName", "step", "count", "share"), drop = FALSE]
  )

  # Set edge attributes
  igraph::E(ig)$weight      <- edgesDf$countValue
  igraph::E(ig)$probability <- edgesDf$probability
  igraph::E(ig)$sourceStep  <- edgesDf$sourceStep
  igraph::E(ig)$targetStep  <- edgesDf$targetStep

  # --- Build decoded sequences ---
  seqCols <- c(stepNameCols, "countValue")
  sequences <- pathsData[, seqCols, drop = FALSE]
  sequences$pathway <- apply(
    sequences[, stepNameCols, drop = FALSE], 1,
    function(row) paste(row[!is.na(row)], collapse = " -> ")
  )
  sequences$depth <- rowSums(!is.na(sequences[, stepNameCols, drop = FALSE]))
  rownames(sequences) <- NULL

  # --- Summary ---
  summaryInfo <- list(
    totalPathways = nrow(sequences),
    totalPatients = sum(sequences$countValue),
    maxDepth      = length(nonEmpty),
    uniqueEvents  = length(unique(nodesDf$eventName)),
    stepsUsed     = length(nonEmpty),
    vertexCount   = igraph::vcount(ig),
    edgeCount     = igraph::ecount(ig)
  )

  result <- list(
    graph           = ig,
    sequences       = sequences,
    summary         = summaryInfo,
    eventNameLookup = eventNameLookup
  )
  class(result) <- "event_sequence_graph"
  return(result)
}


#' @export
print.event_sequence_graph <- function(x, ...) {
  cat("Event Sequence Graph (igraph)\n")
  cat("-----------------------------\n")
  cat("Vertices (event-steps):", x$summary$vertexCount, "\n")
  cat("Edges (transitions):   ", x$summary$edgeCount, "\n")
  cat("Unique events:         ", x$summary$uniqueEvents, "\n")
  cat("Pathway depth:         ", x$summary$maxDepth, "steps\n")
  cat("Total pathways:        ", x$summary$totalPathways, "\n")
  cat("Total patients:        ", x$summary$totalPatients, "\n")
  cat("\nTop sequences:\n")
  topN <- min(10, nrow(x$sequences))
  ordered <- x$sequences[order(-x$sequences$countValue), ]
  for (i in seq_len(topN)) {
    cat(
      sprintf("  %d. [n=%d] %s\n", i, ordered$countValue[i], ordered$pathway[i])
    )
  }
  invisible(x)
}


#' Plot an event sequence graph.
#'
#' @description
#' Produces a layered plot of the event sequence graph using \code{\link[igraph]{plot.igraph}}.
#' Nodes are laid out by step (left to right), with edge widths proportional to
#' patient counts and node sizes proportional to event counts within each step.
#'
#' @param x An \code{event_sequence_graph} object (returned by
#'          \code{\link{buildEventSequenceGraph}}).
#' @param colorPalette Character vector of hex colors for nodes. If \code{NULL}
#'        (default), a built-in palette is used. Colors are mapped by unique event
#'        name (same event = same color across steps).
#' @param edgeWidthRange Numeric vector of length 2. Min and max edge widths
#'        (default: \code{c(0.5, 8)}).
#' @param vertexSizeRange Numeric vector of length 2. Min and max vertex sizes
#'        (default: \code{c(8, 25)}).
#' @param vertexLabelCex Label size multiplier (default: 0.7).
#' @param main Plot title (default: "Event Sequence Graph").
#' @param ... Additional arguments passed to \code{\link[igraph]{plot.igraph}}.
#'
#' @return Invisibly returns the igraph object.
#'
#' @examples
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   cpResults <- list(
#'     pathwaysAnalysisPathsData = data.frame(
#'       pathwayAnalysisGenerationId = c(1L, 1L),
#'       targetCohortId = c(10L, 10L),
#'       step1 = c(2L, 4L),
#'       step2 = c(4L, NA_integer_),
#'       countValue = c(10L, 5L)
#'     ),
#'     pathwayAnalysisCodesLong = data.frame(
#'       pathwayAnalysisGenerationId = c(1L, 1L),
#'       code = c(2L, 4L),
#'       targetCohortId = c(10L, 10L),
#'       eventCohortId = c(1L, 2L),
#'       isCombo = c(0L, 0L),
#'       numberOfEvents = c(1L, 1L)
#'     ),
#'     isCombo = data.frame(
#'       targetCohortId = c(10L, 10L),
#'       comboId = c(2L, 4L),
#'       numberOfEvents = c(1L, 1L),
#'       isCombo = c(0L, 0L)
#'     )
#'   )
#'
#'   generationSet <- data.frame(
#'     cohortId = c(1L, 2L, 10L),
#'     cohortName = c("Celecoxib", "Diclofenac", "NSAIDs")
#'   )
#'
#'   esg <- buildEventSequenceGraph(cpResults, generationSet)
#'   plot(esg)
#' }
#'
#' @export
plot.event_sequence_graph <- function(x,
                                      colorPalette = NULL,
                                      edgeWidthRange = c(0.5, 8),
                                      vertexSizeRange = c(8, 25),
                                      vertexLabelCex = 0.7,
                                      main = "Event Sequence Graph",
                                      ...) {

  check_installed("igraph", reason = "plot.event_sequence_graph() uses igraph for plotting.")


  ig <- x$graph

  # --- Layout: Sugiyama layered layout based on step attribute ---
  steps <- igraph::V(ig)$step
  layout <- igraph::layout_with_sugiyama(ig, layers = steps)$layout

  # --- Node colors: same event name -> same color ---
  uniqueEvents <- unique(igraph::V(ig)$eventName)

  if (is.null(colorPalette)) {
    colorPalette <- grDevices::hcl.colors(
      max(length(uniqueEvents), 3),
      palette = "Set 2"
    )
  }

  colorMap <- stats::setNames(
    rep_len(colorPalette, length(uniqueEvents)),
    uniqueEvents
  )
  vertexColors <- unname(colorMap[igraph::V(ig)$eventName])

  # --- Scale edge widths ---
  weights <- igraph::E(ig)$weight
  if (length(unique(weights)) == 1) {
    edgeWidths <- rep(mean(edgeWidthRange), length(weights))
  } else {
    edgeWidths <- edgeWidthRange[1] +
      (weights - min(weights)) / (max(weights) - min(weights)) *
      (edgeWidthRange[2] - edgeWidthRange[1])
  }

  # --- Scale vertex sizes ---
  counts <- igraph::V(ig)$count
  if (length(unique(counts)) == 1) {
    vertexSizes <- rep(mean(vertexSizeRange), length(counts))
  } else {
    vertexSizes <- vertexSizeRange[1] +
      (counts - min(counts)) / (max(counts) - min(counts)) *
      (vertexSizeRange[2] - vertexSizeRange[1])
  }

  # --- Edge colors: semi-transparent version of source node color ---
  edgeList <- igraph::as_edgelist(ig)
  sourceNames <- edgeList[, 1]
  sourceVertexIdx <- match(sourceNames, igraph::V(ig)$name)
  edgeColors <- vapply(vertexColors[sourceVertexIdx], function(col) {
    rgbVals <- grDevices::col2rgb(col)
    grDevices::rgb(rgbVals[1], rgbVals[2], rgbVals[3],
                   alpha = 100, maxColorValue = 255)
  }, character(1), USE.NAMES = FALSE)

  # --- Vertex labels: eventName only (drop step suffix) ---
  vertexLabels <- igraph::V(ig)$eventName

  # --- Plot ---
  igraph::plot.igraph(
    ig,
    layout        = layout,
    vertex.color  = vertexColors,
    vertex.size   = vertexSizes,
    vertex.label  = vertexLabels,
    vertex.label.cex = vertexLabelCex,
    vertex.label.color = "black",
    edge.width    = edgeWidths,
    edge.color    = edgeColors,
    edge.arrow.size = 0.5,
    edge.curved   = 0.1,
    main          = main,
    ...
  )

  invisible(ig)
}


# --- Internal helper ---

#' Build a named lookup vector: character(comboId) -> "EventA + EventB"
#' @keywords internal
.buildEventNameLookup <- function(codesLong, generationSet) {
  if (nrow(codesLong) == 0) {
    return(character(0))
  }

  # Map eventCohortId -> cohortName
  nameMap <- stats::setNames(
    as.character(generationSet$cohortName),
    as.character(generationSet$cohortId)
  )

  # Subset to unique code-eventCohortId pairs
  codes <- unique(codesLong[, c("code", "eventCohortId"), drop = FALSE])
  codes$eventName <- nameMap[as.character(codes$eventCohortId)]

  # For any unmapped IDs, use a fallback
  codes$eventName[is.na(codes$eventName)] <-
    paste0("Event_", codes$eventCohortId[is.na(codes$eventName)])

  # Aggregate: paste event names for each code (sorted for deterministic output)
  agg <- aggregate(
    eventName ~ code,
    data = codes,
    FUN = function(x) paste(sort(x), collapse = " + ")
  )

  stats::setNames(agg$eventName, as.character(agg$code))
}
