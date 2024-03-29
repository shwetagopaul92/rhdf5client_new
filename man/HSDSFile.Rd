% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Source.R
\name{HSDSFile}
\alias{HSDSFile}
\title{Construct an object of type HSDSFile}
\usage{
HSDSFile(src, domain)
}
\arguments{
\item{src}{an object of type HSDSSource, the server which exposes the file}

\item{domain}{the domain string; the file's location on the server's
file system.}
}
\value{
an initialized object of type HSDSFile
}
\description{
A HSDSFile is a representation of an HDF5 file the contents of which are accessible
exposed by a HDF5 server.
}
\details{
This function is deprecated and will be defunct in the next release.
}
\examples{
src <- HSDSSource('http://hsdshdflab.hdfgroup.org')
f10x <- HSDSFile(src, '/shared/bioconductor/tenx_full.h5')
}
