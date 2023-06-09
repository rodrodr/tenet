% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-documentation.R
\docType{data}
\name{br.films}
\alias{br.films}
\title{Brazilian post-1984 Fictional Cinema}
\format{
a \link{corpus} object with the following docvars:
\itemize{
\item \code{primaryTitle} character; Main title
\item \code{Director} character; Names of the directors
\item \code{Country} character; Countries involved in the production
\item \code{imdbRating} numeric; IMDB Rating of the film
\item \code{Poster} character; URL to the Film's poster
\item \code{Decade} character; The decade when the film was released
}
}
\source{
OMDB.
}
\usage{
br.films
}
\description{
This dataset contains 260 Brazilian fictional films from 1985 to 2023.
}
\examples{
# some operations on the corpus
summary(br.films)
head(quanteda::docvars(br.films), 10)
br.films
}
\keyword{data}
