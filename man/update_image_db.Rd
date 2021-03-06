% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_image_db.r
\name{update_image_db}
\alias{update_image_db}
\title{Batch download, resize and reorient all data
in the IFPRI database using the
automatic horizon and ROI detection methods}
\usage{
update_image_db(database = NULL, path = "~/cropmonitor",
  server = "http://cdn.wheatcam.ifpri.org/ReportImages")
}
\arguments{
\item{database:}{STATA file as provided by IFPRI}

\item{path:}{path of the IFPRI database images to process}

\item{server:}{location of the images online
including estimating the horizon and ROI, as well as indices / features}
}
\description{
Batch download, resize and reorient all data
in the IFPRI database using the
automatic horizon and ROI detection methods
}
\keyword{QA/GC}
\keyword{calculation,}
\keyword{gcc}
