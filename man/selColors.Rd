\name{selColors}
\alias{selColors}
\title{Large-Number Color Palettes for Graphs}
\description{
Generates color scales with a large number of colors based on custom basic palettes or those contained in packages RColorBrewer and wesanderson.
}
\usage{

selColors(palette="Dark2", 
          col.n=9, 
          custom.pal=NULL)
}

\arguments{
\item{palette}{A color palette included either on RColorBrewer or wesanderson. The default is "Dark2".}

\item{col.n}{Total number of colors to be generated. The default is 9.}

\item{custom.pal}{A list of basic colors to be used in the generated of the final color scale. The default is NULL.}

}
\details{
Generates color scales with a large number of colors based on custom basic palettes or those contained in packages RColorBrewer and wesanderson.
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
# on the "Zissou1" palette (wesanderson)
selColors(palette="Zissou1", 20)


# Generates a list of 20 different colors based 
# on a custom palette using red, yellow and blue
selColors(col.n=20, custom.pal=c("red","yellow","blue") )


}

}
