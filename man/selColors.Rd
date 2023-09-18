\name{selColors}
\alias{selColors}
\title{Large-Number Color Palettes for Graphs}
\description{
Generates color scales with a large number of colors based on custom basic palettes or those contained in packages RColorBrewer and wesanderson.
}
\usage{

selColors(palette=c("#1B9E77",
                    "#D95F02",
                    "#7570B3",
                    "#E7298A",
                    "#66A61E",
                    "#E6AB02",
                    "#A6761D",
                    "#666666"), 
          col.n=9)
}

\arguments{
\item{palette}{The color palette included in the pal object. The default are the colors of the "Dark2" palette included in the RColorBrewer package.}

\item{col.n}{Total number of colors to be generated. The default is 9.}
}

\details{
Generates color scales with a large number of colors based on custom basic palettes or those contained in other R packages.
}
\value{
The function returns a list of n colors based on a previously defined palette.
}
\examples{
\dontrun{

# Generates a list of 20 different colors based 
# on the Dark2 palette (RColorBrewer)
selColors(col.n=20)

# Generates a list of 20 different colors based 
# on the "ZissouContinuous" palette (wesanderson)
selColors(palette=pal$cat.wesanderson.ZissouContinuous.11, 20)


# Generates a list of 20 different colors based 
# on a custom palette using red, yellow and blue
selColors(palette=c("red","yellow","blue"), col.n=20)


}

}
