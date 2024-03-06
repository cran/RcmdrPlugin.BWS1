\name{bws1CountPlot}
\alias{bws1CountPlot}


\title{Draw Relationship between Means and Standard Deviations of BWS1 Scores}

\description{
Draw a plot of the relationship between means and standard deviations 
of the BW scores by item.
}

\details{
This dialog box draws a plot of the relationship between means and
standard deviations of the BW scores by item using the function 
\code{\link[support.BWS]{plot.bws.count2}} (\code{plot} for S3 class 
\sQuote{\code{bws.count2}}).
This dialog box is activated if the data set containing BWS1 scores
generated in the \sQuote{\code{Calculate BWS1 Scores}} dialog box is
selected as the active data set.
For details on the results of this dialog box, refer to help for
\code{\link[support.BWS]{bws.count}}.

GUI elements of this dialog box are as follows.

\itemize{
\item \code{Score type}: Type of score to be drawn. Select one from
three radio buttons corresponding to \sQuote{\code{Best-minus-Worst}}, 
\sQuote{\code{Best}}, and \sQuote{\code{Worst}}.

\item \code{Position of point labels}: A position of labels for points
in the plot. Select one from four radio buttons corresponding to 
\sQuote{\code{Below}}, \sQuote{\code{Left}}, \sQuote{\code{Above}}, and
\sQuote{\code{Right}}.

\item \code{x-axis}: the x limits of the plot. The lower limit and
the upper limit are specified in the \sQuote{\code{from}} box and 
the \sQuote{\code{to}} box, respectively.

\item \code{y-axis}: the y limits of the plot. The lower limit and 
the upper limit are specified in the \sQuote{\code{from}} box and
the \sQuote{\code{to}} box, respectively.
}
}

