\name{bws1CountBarplot2}
\alias{bws1CountBarplot2}


\title{Draw Mean BWS1 Scores}

\description{
Draw a bar plot of means of the BW scores by item.
}

\details{
This dialog box draws a bar plot of means of the BW scores by item
using the function \code{\link[support.BWS]{barplot.bws.count2}} 
(\code{barplot} for S3 class \sQuote{\code{bws.count2}}). 
This dialog box is activated if the data set containing BWS1 scores
generated in the \sQuote{\code{Calculate BWS1 Scores}} dialog box is
selected as the active data set. 
For details on the results of this dialog box, refer to help for
\code{\link[support.BWS]{bws.count}}.

GUI elements of this dialog box are as follows.

\itemize{
\item \code{Score type}: Type of score to be drawn. Select one from
four radio buttons corresponding to \sQuote{\code{Best-minus-Worst (BW)}},
\sQuote{\code{Standardized BW}}, \sQuote{\code{Best}}, and 
\sQuote{\code{Worst}}.

\item \code{Error bar type}: A type of error bar added in the bar plot. 
Select one from four radio buttons corresponding to \sQuote{\code{None}},
\sQuote{\code{Standard deviation}}, \sQuote{\code{Standard error}},
and \sQuote{\code{Confidence interval}}.

\item \code{Confidence level}: A confidence level when adding 
the confidence interval on the bar plot. This option is valid only when
selecting \sQuote{\code{Confidence interval}} as the error bar type.

\item \code{Left margin of plot}: Number of lines of margin to be 
specified on the left side of the plot.
}
}

