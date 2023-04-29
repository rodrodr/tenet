\name{colBright}
\alias{colBright}
\title{Color Brightness for Labels}
\description{
Evaluates the brightness of background colors and return light colors for labels when the background is dark and dark colors when the background is light.
}
\usage{
colBright(colors,...)

colBright(colors,
        limit=130,
        bright="white",
        dark="black")
}

\arguments{
\item{colors}{Background colors to be employed in plots.}
\item{limit}{Luminosity threshold to separate bright to dark colors. The default is 130.}
\item{bright}{The label color to be returned when background colors are considered dark. The default is white.}
\item{dark}{The label color to be returned when background colors are considered bright. The default is black.}
}
\details{
Calculates the ratio of the frequency of a given term compared to its appearance in the whole corpus. This function is particularly useful for selecting texts according to themes or issues. It allows users to select only those documents containing the ideas for interest.
}
\value{
The function presents two possible values to be returned. The default is the frequency ratio. The ratio of a term will be high (much higher than 1) in those cases where few documents concentrate most of its occurence. It will be zero for the texts with no matches and will be between 0 and 1 for the documents where the frequency is below average.

If the option "return.selected=TRUE", the function will return all the values of the ratio above the limit established in the "threshold" argument. The default for this last parameter is to return all values above 0, i.e., all the texts where the term were obsersed at least once.

}
\examples{
\dontrun{
# Establish a set of both dark and light colors
col <- c("white","grey20","black","orange","red3","green")

# Use the function to return label colors
colBright(col)

# Change the default colors
colBright(col, bright="yellow", dark="blue")
}
}
