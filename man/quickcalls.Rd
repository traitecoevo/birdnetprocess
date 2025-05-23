% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quickcalls.R
\name{quickcalls}
\alias{quickcalls}
\title{quick calls}
\usage{
quickcalls(df, confidence = 0, remove.dominants = F)
}
\arguments{
\item{df}{the dataframe created by the read_birdnet_file or read_birdnet_folder function}

\item{confidence}{the minimum confidence level for the bird call identifications}

\item{remove.dominants}{removes dominant species from plot for cleaner visualisation}
}
\value{
a plot showing the number of calls per bird species + prints removed species if applicable
}
\description{
takes birdnet .txt data creates a figure representing number of recordings of
each bird, over the given confidence interval
}
\examples{
\dontrun{ quickcalls(df, 0.5) }

}
