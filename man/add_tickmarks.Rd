% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add-tickmarks.R
\name{add_tickmarks}
\alias{add_tickmarks}
\title{Add tickmarks to an existing plot TODO copying over for now to get going,
then delete.}
\usage{
add_tickmarks(
  obj_lub,
  y_tick_by,
  y_tick_start,
  y_tick_end,
  y_tick_max_number = 50,
  x_tick_extra_years,
  start_decade_ticks
)
}
\arguments{
\item{obj_lub}{obj a `pacea_index` object, which is a time series, with a date
column that is the lubridate `date` class.}

\item{y_tick_by}{increment for y-axis ticks; gets
overwritten in `add_tickmarks()` if this yields more than
`y_tick_max_number` tickmarks. If using [plot.pacea_biomass()] the default
of 1 gets automatically changed to 0.25 for `plot(hake_biomass)`. If
`y_axis_reverse` is TRUE then the negative of `y_tick_by` (which should
always be positive) is passed onto `add_tickmarks()`.}

\item{y_tick_start}{where to start y-axis tickmarks, set automatically if not
specified (may need to occasionally specify)}

\item{y_tick_end}{where to end y-axis tickmars, as for `y_tick_start`}

\item{y_tick_max_number}{maximum number of y tickmarks.}

\item{x_tick_extra_years}{number of extra years to expand around the range
of data for which to add annual tick marks (does not expand the axis); in
hindsight could have simplified this in `add_tickmarks()`, but just made
the default big here.}

\item{start_decade_ticks}{where to start tickmarks for decades (defaults to
1800 as hard to automate)}
}
\value{
adds tickmarks to an existing plot
}
\description{
Add sensible smaller (unlabelled) tickmarks to both axes of an existing
pacea temporal plot. Called from `plot.pacea_index()`,
`plot.pacea_recruitment()` etc. Is exported but unlikely to be needed externally.
}
\examples{
\dontrun{
plot.pacea_index(oni)   # see end of that function for usage
}
}
\author{
Andrew Edwards
}
