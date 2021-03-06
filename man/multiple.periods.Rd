% Generated by roxygen2 (4.0.2): do not edit by hand
\name{multiple.periods}
\alias{multiple.periods}
\title{Wrapper function for 'op.temp.body' to loop over multiple rasters}
\usage{
multiple.periods(max.rast,
min.rast,
elev,
J,
hours,
mean = TRUE,
st.dev = TRUE,
out = "mean"
...)
}
\arguments{
\item{max.rast}{A list providing the full path to all maximum temperature raster files. Should be of same length and order as 'min.rast'}

\item{min.rast}{A list providing the full path to all minimum temperature raster files. Should be of same length and order as 'max.rast'}

\item{J}{A list corresponding to the Julian day for each maximum/minimum period. Be sure that order dates matches order of file names in 'max.rast' and 'min.rast'}

\item{elev}{Elevation raster object}

\item{hours}{Vector of the hours each day to calculate operative body temperature}

\item{out}{Results can be returned as the "mean", "sd", "all" (Default = 'mean')}

\item{...}{Additional parameters to be passed to \code{\link[biophys]{op.body.temp}} function.}
}
\value{
This function will loop over all maximum and minimum raster surfaces to calculate monthly averages. Specified output can be (1) "mean": mean of all months; (2) "sd": standard deviation of all months; (3) "all": mean, standard deviation, and each monthly average
}
\description{
Wrapper function for 'op.temp.body' to loop over multiple rasters
}
\details{
This is a helper function to calculate operative body temperatures using multiple minimum and maximum temperature surfaces (e.g., monthly averages). This function directly uses \code{\link[biophys]{op.body.temp}}. Note that you should have the same numbe of minimum and maximum surfaces in each list, and they should share a common name.
}
\examples{
# TO BE WRITTEN
}
\author{
Bill Peterman <Bill.Peterman@gmail.com>
}

