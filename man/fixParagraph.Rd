\name{fixParagraph}
\alias{fixParagraph}
\title{Fix sentences from texts extracted from PDF and other text documents.}
\description{
Remove line breaks, hyphenizations, and other interruptions in paragraph flows.
}
\usage{
fixParagraph(text, 
             check.doubleline=FALSE)
}

\arguments{
\item{text}{A a character vector containing the text to be processed.}
\item{check.doubleline}{Replace values only when there are only one newline character or include also two newline characters. The default is FALSE (only one).}
}
\details{
The function fixes texts extracted from documents or formats that convert each row in a new line. In these cases, sentences and paragraphs are broken into several parts. This fragmentation hinders the analysis of texts or their organization into paragraph or sentence-based corpora. This function also allows correction of Latin texts without the need to remove accents and diacritics.
}
\value{
A character vector with the corrected text.
}
\examples{
\dontrun{
# An example text from Lewis Mumford "The Myth of the Maschine":
tx <- "From Peking Man onward, some five hundred thousand years ago, caves have
served as the womb and the tomb of human culture. All over the world, caves and
grottoes became sacred places, reserved for ceremonials and for memorials of the
dead. The paleolithic cave (top left) at La Magdeleine has two female figures at
either side of the entrance and a horse in the foreground at the right, not visible
here. The Temple of Siva at Elephanta, one of the many examples in India of
temples and statues hollowed out of a stone mountain, repeats that ancient
arrangement of symbols. But early man also appropriated caves for shelter, se-
curity, and storage: witness (top right) this later Amerindian habitation within
a cliff. (Top left) La Magdeleine (Tarn). From Sigfried Giedion, 'The Eternal
Present.' Photograph by Achille Weider. (Top right) Gila National Monument.
New Mexico. Courtesy of United States Department of the Interior, National
Park Service Photo. (Bottom) Siva Temple, Elephanta, c. Eighth century. Cour-
tesy of Museum of Fine Arts, Boston."

# Inspect the text format
cat(tx)

# Apply the function
tx <- fixParagraph(tx)

# Reinspect the text format
cat(tx)
}
}
