\name{plotLogOddsRatio}
\alias{plotLogOddsRatio}
\title{Generate a Weighted Log Odds Ratio vs. Log Frequency Plot}

\description{
  The \code{plotLogOddsRatio} function generates a weighted log odds ratio vs. log frequency plot to visualize the relationship between log odds ratios and log frequencies of tokens in a text corpus. This function uses the quanteda package for text processing and ggplot2 for plotting. It is particularly useful for analyzing the significance of token frequencies in different categories or documents within the corpus.
}

% Usage
\usage{
plotLogOddsRatio(
    corpus,
    ref.cat,
    comp.cat = NULL,
    palette = c("red3", "goldenrod1", "dodgerblue3"),
    remove.punct = TRUE,
    remove.number = TRUE,
    remove.stopwords = TRUE,
    use.stem = FALSE,
    use.bigrams = FALSE,
    label.dots = TRUE,
    gray.area = 0,
    exclude.zeros = FALSE,
    lang = "es",
    title = "Weighted log odds ratio vs. Log Frequency",
    title.text.size = 14,
    interactive = TRUE,
    return.data = FALSE
  )
}

\arguments{
  \item{corpus}{
    A text corpus.
  }
  \item{ref.cat}{
    A category or document ID used as the reference category for log odds ratio calculations.
  }
  \item{comp.cat}{
    A vector of category or document IDs for comparison (default: NULL).
  }
  \item{palette}{
    A vector of color codes specifying the node colors (default: a palette of three colors).
  }
  \item{remove.punct}{
    A logical value indicating whether to remove punctuation from tokens (default: TRUE).
  }
  \item{remove.number}{
    A logical value indicating whether to remove numbers from tokens (default: TRUE).
  }
  \item{remove.stopwords}{
    A logical value indicating whether to remove stopwords from tokens (default: TRUE).
  }
  \item{use.stem}{
    A logical value indicating whether to apply stemming to tokens (default: FALSE).
  }
  \item{use.bigrams}{
    A logical value indicating whether to consider bigrams in tokenization (default: FALSE).
  }
  \item{label.dots}{
    A logical value indicating whether to label data points with token names (default: TRUE).
  }
  \item{gray.area}{
    A numeric value specifying the threshold for the gray area in log odds ratio (default: 0).
  }
  \item{exclude.zeros}{
    A logical value indicating whether to exclude tokens with zero frequency (default: FALSE).
  }
  \item{lang}{
    A character specifying the language for text processing (default: "es" for Spanish).
  }
  \item{title}{
    A character specifying the plot title (default: "Weighted log odds ratio vs. Log Frequency").
  }
  \item{title.text.size}{
    An integer specifying the size of the plot title text (default: 14).
  }
  \item{interactive}{
    A logical value indicating whether to generate an interactive plot (default: TRUE).
  }
  \item{return.data}{
    A logical value indicating whether to return the data used for plotting (default: FALSE).
  }
}

% Details
\details{
  The \code{plotLogOddsRatio} function tokenizes the text corpus, processes tokens based on the specified options (e.g., removing punctuation, stopwords, stemming), calculates log odds ratios, and plots the log odds ratios against log frequencies. Tokens are colored based on their log odds ratios, and a gray area can be specified to highlight significant tokens. The plot can be interactive, allowing for tooltips and data exploration.
}

% Value
\value{
  The function returns either a ggiraph interactive plot (if \code{interactive} is TRUE) or a ggplot2 plot (if \code{interactive} is FALSE). When \code{return.data} is TRUE, it returns a data frame containing the plotted data.
}

% Examples
\examples{
  \dontrun{
    # Example usage:
    # Load required libraries
    library(quanteda)
    library(tenet)
    
    # Creates a corpus of inaugural speeches
    cp <- corpus(spa.inaugural)

    # Group documents by President
    ci <- corpus_group(cp, groups = President)

    # Creates the plot
    plotLogOddsRatio( corpus = ci, 
                      ref.cat = "Zapatero")                
  }
}



