\name{bws1Count}
\alias{bws1Count}

\title{Calculate BWS1 Scores}

\description{
Calculate various BW scores for each respondent from the data set for
BWS1 analysis.
}

\details{
This dialog box, which uses the function \code{\link[support.BWS]{bws.count}} 
in \pkg{support.BWS}, calculates various BW scores for each respondent 
from the data set for BWS1 analysis created in the 
\sQuote{\code{Create Data Set for BWS1 Analysis}} dialog box. 
This dialog box is activated if the data set for BWS1 analysis is selected 
as the active data set. For details on the results of this dialog box, 
refer to help for the function \code{\link[support.BWS]{bws.count}}. 
The resultant data frame is selected automatically as the active data set. 

GUI elements of this dialog box are as follows.

\itemize{
\item \code{Name for scores}: An R object name into which a data set
generated using this dialog box is stored. The default name 
\sQuote{\code{BWS1data}} can be changed following R's rules for naming
objects.
}
}


