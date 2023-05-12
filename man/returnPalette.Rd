\name{returnPalette}
\alias{returnPalette}
\title{Returns Selected Color Palettes}
\description{
Returns the colors of a selected palette included in different R packages.
}
\usage{
returnPalette(palette="EdwardHopper")
}

\arguments{
\item{palette}{Name of the color palette. The default is "EdwardHopper", included in the MetBrewer package.}

}
\details{
Provides all the colors belonging to a palette included in one of the packages RColorBrewer, wesanderson, ghibli, rtist, MetBrewer, and lisa.
}
\value{
Returns a list of hexadecimal values of all colors represented in the selected palette.
}
\examples{
# Returns the colors for the Degas palette.
col <- returnPalette(palette="Degas")

# View the results
barplot(rep(10, length(col)), col=col)

# Repeats the operation for the Gauguin palette.
col <- returnPalette(palette="Gauguin")

# View the results
barplot(rep(10, length(col)), col=col)
}
