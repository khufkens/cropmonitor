% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sobel.r
\name{sobel}
\alias{sobel}
\title{Sobel-Feldman operator}
\usage{
sobel(x, method = "intensity", ...)
}
\arguments{
\item{...}{Additional arguments passed to raster::overlay or, 
if method="edge", raster::focal 
(if you want a file written to disk use filename = "" argument)}

\item{x:}{A raster class object}

\item{method:}{Type of operator ("intensity", "direction", "edge")}
}
\value{
A raster class object or raster written to disk
}
\description{
An isotropic image gradient operator using a 3x3 window
}
\examples{

library(raster)
r <- brick(system.file("external/rlogo.grd", package="raster")) 
s.int <- sobal(r[[1]])
s.dir <- sobal(r[[1]], method = "direction")
s.edge <- sobal(r[[1]], method = "edge")
par(mfrow=c(2,2))
  plot(r[[1]])
  plot(s.int, main="intensity") 
  plot(s.dir, main="direction") 
  plot(s.edge, main="edge")
  
}
\references{
Sobel, I., & G. Feldman, (1969) A 3x3 Isotropic Gradient Operator for
Image Processing, presented at the Stanford Artificial 
Intelligence Project (SAIL).
}
\author{
Jeffrey S. Evans  <jeffrey_evans@tnc.org>
}
