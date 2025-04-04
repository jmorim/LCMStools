% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{getCEData}
\alias{getCEData}
\title{Get peak intensities for each collision energy in an experiment}
\usage{
getCEData(peaks, metric = "into")
}
\arguments{
\item{peaks}{An XChromatogram object that contains detected peaks}
}
\value{
A data frame of collision energy, peak intensities, and data files.
}
\description{
Returns a data frame of collision energies, peak intensities, and data files.
Assumes that one peak is detected per file.
}
\examples{
library(xcms)
ex.file = system.file('extdata', '_d/0176.d/0176.mzML', package='LCMStools')
ex.data = readSRMData(ex.file)
ex.peaks = findChromPeaks(ex.data, CentWaveParam(peakwidth=c(0.2,0.25)))
getCEData(ex.peaks)
}
