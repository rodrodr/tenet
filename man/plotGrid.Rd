\name{plotGrid}
\alias{plotGrid}
\title{Proportional Symbol Grid Plot}
\description{
Creates a grid of proportional symbols for representing tabular data.
}
\usage{

plotGrid(data, 
        x=NULL, 
        y=NULL, 
        size=NULL, 
        color=NULL, 
        tooltip=NULL, 
        palette=c("#E41A1C",
                  "#377EB8",
                  "#4DAF4A",
                  "#984EA3",
                  "#FF7F00",
                  "#FFFF33",
                  "#A65628",
                  "#F781BF",
                  "#999999"), 
        grid.color="grey90",
        interactive=TRUE, 
        family="Helvetica", 
        x.axis.angle=90, 
        standardize=FALSE, 
        avg.mark=TRUE, 
        axis="y", 
        alpha=TRUE, 
        width_svg = 9, 
        height_svg = 6, 
        leg.size="Size",
        leg.color="Dimension", 
        point.shape="circle")
}

\arguments{
\item{data}{A data frame containing two categorical variables and their shared frequencies.}
\item{x}{A character string specifying the name of the column in \code{data} to be used as the x-axis.}
\item{y}{A character string specifying the name of the column in \code{data} to be used as the y-axis.}
\item{size}{A character string specifying the name of the column in \code{data} to be used as the size of the symbols.}
\item{color}{A character string specifying the name of the column in \code{data} to be used as the color of the symbols.}
\item{tooltip}{A character string specifying the name of the column in \code{data} to be used as the tooltip.}
\item{palette}{A vector of colors. the default is a vector with the colors from the Set1 palette from the RColorBrewer package.}
\item{grid.color}{A character string specifying the color of the grid. The default is "grey90".}
\item{interactive}{A logical value. If \code{TRUE}, the plot is interactive. The default is \code{TRUE}.}
\item{family}{A character string specifying the font family. The default is "Helvetica".}
\item{x.axis.angle}{A numeric value specifying the angle of the x-axis labels. The default is 90.}
\item{standardize}{A logical value. If \code{TRUE}, the data is standardized. The default is \code{FALSE}.}
\item{avg.mark}{A logical value. If \code{TRUE}, the average is represented in the plot. The default is \code{TRUE}.}
\item{axis}{A character string specifying the axis to be used in the calculation of the average. The default is "y".}
\item{alpha}{A logical value. If \code{TRUE}, the transparency of the bubbles is proportional to the size. The default is \code{TRUE}.}
\item{width_svg}{A numeric value specifying the width of the svg. The default is 9.}
\item{height_svg}{A numeric value specifying the height of the svg. The default is 6.}
\item{leg.size}{A character string specifying the title of legend for the size. The default is "Size".}
\item{leg.color}{A character string specifying the title of legend for the color. The default is "Dimension".}
\item{point.shape}{A character string specifying the shape of the points. The possible values are "circle","square","triangle","diamond". The default is "circle".}
}
\details{
This function creates a proportional symbol grid for representing tabular data.  
}
\value{
The result is a ggplot2 chart or a ggiraph object (if interactive is true).
}
\examples{
\dontrun{
# Create a corpus with the inaugural speeches
# Count the keywords
library(quanteda)
cp <- corpus(spa.inaugural)

xz <- countKeywords(cp, 
                    dic.pol.es, 
                    rel.freq = T, 
                    group.var = "President",
                    quietly = TRUE)

# Aggregates the frecuencies by group
# contained in the dictionary
xx <- aggregate(list(frequency=xz$frequency), 
                by=list(groups=xz$groups,
                        level1=xz$level1,
                        level2=xz$level2), 
                sum, na.rm=T)

# Remove the zeros
xx <- xx[xx$frequency>0,]

# Sort values by level1 and level2
xx <- xx[order(xx$level1, xx$level2),]
xx$level2 <- factor(xx$level2, levels=unique(xx$level2))

# Create the non-standardized plot
plotGrid(xx, 
         x="groups", 
         y="level2", 
         size="frequency",
         palette=pal$cat.awtools.bpalette.16,
         color="level1",
         alpha=FALSE, 
         interactive=FALSE)

# Create the standardized plot
plotGrid(xx, 
         x="groups", 
         y="level2", 
         size="frequency",
         palette=pal$cat.awtools.bpalette.16,
         color="level1", 
         standardize = TRUE,
         interactive=TRUE)

}
}


