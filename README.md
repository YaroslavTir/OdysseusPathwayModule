
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OdysseusPathwayModule

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/OdysseusPathwayModule)](https://CRAN.R-project.org/package=OdysseusPathwayModule)
<!-- badges: end -->

OdysseusPathwayModule provides cohort pathway analysis for OMOP CDM
cohort data. It supports two analysis modes:

- `post-index`: pathways of events that occur after target cohort entry.
- `pre-index`: pathways of events that occur before target cohort entry
  in a configurable lookback window.

The package focuses on pathway execution and tabular outputs.
Visualization functions live in the companion package
`OdysseusPathwayPlots`.

## Installation

Install from CRAN:

``` r
#install.packages("pak")
pak::pak("OdysseusPathwayModule")
```

Install from GitHub:

``` r
pak::pak("OdyOSG/OdysseusPathwayModule")
```

## Main function

Use `executeCohortPathways()` to run the analysis:

``` r
library(OdysseusPathwayModule)

results <- executeCohortPathways(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = "results",
  cohortTableName = "cohort",
  targetCohortIds = c(7),
  eventCohortIds = c(1:6),
  analysisType = "post-index"
)
```

For pre-index analysis (for example, 365 to 1 days before cohort entry):

``` r
preResults <- executeCohortPathways(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = "results",
  cohortTableName = "cohort",
  targetCohortIds = c(7),
  eventCohortIds = c(1:6),
  analysisType = "pre-index",
  lookbackStartDay = -365,
  lookbackEndDay = -1
)
```

If target and event cohorts are stored in different tables/schemas, use:

``` r
resultsSplit <- executeCohortPathways(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = "results",
  cohortTableName = "target_cohorts",
  outcomeDatabaseSchema = "results",
  outcomeTableName = "event_cohorts",
  targetCohortIds = c(7),
  eventCohortIds = c(1:6)
)
```

## Output

`executeCohortPathways()` returns a list with pathway-level and
event-level results, including:

- `pathwayAnalysisStatsData`
- `pathwaysAnalysisPathsData`
- `pathwaysAnalysisEventsData`
- `pathwaycomboIds`
- `pathwayAnalysisCodesLong`
- `isCombo`
- `pathwayAnalysisCodesData`

These outputs can be used directly for reporting, QA, or downstream
visualization.

## Reproducible demo with Eunomia

For an end-to-end runnable example, see the package vignette. The
vignette uses Eunomia to create demo cohorts and run both analysis
modes.

## Graph analysis

The package includes a built-in lightweight graph representation of
pathway results based on [igraph](https://igraph.org/). This is useful
for exploratory analysis and custom visualizations without additional
dependencies.

``` r
# Build a directed igraph from executeCohortPathways() output
esg <- buildEventSequenceGraph(results, generationSet)
esg$summary

# Plot with layered layout (node size proportional to patient count,
# edge width proportional to transitions)
plot(esg)
```

The returned object contains:

- `$graph` — a directed, weighted `igraph` object with vertex/edge
  attributes
- `$sequences` — decoded pathway strings with per-pathway patient counts
- `$summary` — high-level statistics (vertices, edges, depth, unique
  events)
- `$eventNameLookup` — named vector mapping combo IDs to event names

## Plotting

For Sankey and Sunburst visualization, use the companion package
`OdysseusPathwayPlots`.

## Development notes

`README.md` is generated from `README.Rmd`. After editing this file,
regenerate with:

``` r
devtools::build_readme()
```
