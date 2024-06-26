% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabler.R
\name{tabler}
\alias{tabler}
\title{TableR}
\usage{
tabler(
  data_object = NULL,
  mode = "summarize",
  tableID = NULL,
  variables = NULL,
  cols = c("labels", "estimate", "pct"),
  dispPerc = TRUE,
  type = NULL,
  shorten = NULL,
  sort = FALSE,
  sort_bygroup = FALSE,
  pctFilter = NULL,
  pdf = FALSE,
  usCompare = NULL,
  stateCompare = NULL,
  summaryLevels = 1,
  filterAddress = NULL,
  filterRadius = NULL,
  filterByGeoType = NULL,
  filterByGeoValue = NULL,
  filterSummaryLevels = NULL,
  state = NULL,
  county = NULL,
  tract = NULL,
  block_group = NULL,
  geography = NULL,
  year = NULL,
  geosObject = NULL,
  dataset_main = "acs",
  dataset_sub = "acs5",
  dataset_last = NULL,
  censusVars = NULL,
  moe = FALSE,
  groupPerc = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{data_object}{A census data object.}

\item{mode}{Options for display calculations: "summarize", "summarizeinc","simple","bygeo","nosummary"}

\item{tableID}{Formerly known as varBase, or concept, or group: i.e., "B01001"}

\item{variables}{A vector of variables for the call.}

\item{cols}{Defaults to "labels","estimate","pct", but can be modified.}

\item{dispPerc}{Whether to display percentages. This may be set to FALSE when
using median or mean data.}

\item{type}{Parameter to filter by type ("root","summary","level_1", etc)}

\item{shorten}{Numeric value specifying the size of a slice.}

\item{sort}{Option to sort by value.}

\item{sort_bygroup}{Logical flag to specify whether to sort by variable group.}

\item{pctFilter}{A numeric value filtering the minimum percentage shown.}

\item{pdf}{Set to TRUE for latex use.}

\item{usCompare}{Pass comparison dataset offer a national compare value.}

\item{stateCompare}{Pass comparison dataset offer a state-level compare value.}

\item{summaryLevels}{How deep shaded levels will display.}

\item{filterAddress}{Address of centroid to filter census tracts/blocks.}

\item{filterRadius}{In miles, the radius for the filter.}

\item{filterByGeoType}{An irregular geo type to get a smaller overlapping set
of tracts, block_groups or other geography from. Options are currently
"metro", "place","combined_statistical_areas". E.g., Find all tracts in Chicago (place).}

\item{filterByGeoValue}{A value to find object for filtering. Either NAME or GEOID.}

\item{filterSummaryLevels}{Lowest summary level to include.}

\item{state}{A state value to filter.}

\item{county}{A county value to filter.}

\item{tract}{Input (abb. or FIPS) of tract for search.}

\item{block_group}{Input (abb. or FIPS) of block group for search.}

\item{geography}{Geography designation for capi().}

\item{year}{Year designation for capi().}

\item{geosObject}{Optional geosObject object to speed up geo processing.}

\item{dataset_main}{Selection parameters for get_census_variables (e.g. "acs")}

\item{dataset_sub}{Selection parameters for get_census_variables (e.g. "acs5")}

\item{dataset_last}{Selection parameters for get_census_variables (e.g. "cprofile")}

\item{censusVars}{Passthrough object to bypass get_census_variables}

\item{moe}{Logical parameter to specify whether margin of error should be calculated.}

\item{groupPerc}{Logical parameter to specify whether to display percentages by type in tables.}

\item{verbose}{Pass through param to create_profile() for feedback.}
}
\value{
A flextable object.
}
\description{
A censusprofiler wrapper for flextables in RDF files.
}
\examples{
\dontrun{
tabler(data_object=data,datatype="acs",dataset="acs5",mode="simple",
tableID="B02001",variables=NULL,summaryLevels=1,)
}
}
