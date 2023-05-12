\name{listPalettes}
\alias{listPalettes}
\title{List of Color Palettes}
\description{
Provides a list of all palettes in selected color packages.
}
\usage{
listPalettes()
}
\details{
Provides the names of the palettes contained in the packages RColorBrewer, wesanderson, lisa, MetBrewer, ghibli, and rtist. The function is designed to help users to quickly access the names of color palettes from various packages without having to load or use their specific functions.
}
\value{
A list with all palettes grouped by each package.
}
\examples{
# generates a list
pal <- listPalettes()

# View the results
pal

# Access the EdwardHopper palette from the
# MetBrewer package
pal$MetBrewer$EdwardHopper
}
