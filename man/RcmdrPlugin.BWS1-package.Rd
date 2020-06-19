\docType{package}
\name{RcmdrPlugin.BWS1-package}
\alias{RcmdrPlugin.BWS1}
\alias{bws1Design}
\alias{bws1Count}
\alias{bws1CountBarplot1}
\alias{bws1CountBarplot2}
\alias{bws1CountPlot}
\alias{bws1CountSummary}
\alias{bws1Dataset}
\alias{bws1DesignOLD}
\alias{bws1Fitmodel}
\alias{bws1FitmodelSimple}
\alias{bws1Items}
\alias{bws1Questions}
\alias{bws1Response}
\alias{bws1SharePref}
\alias{bws1count2P}
\alias{bws1dataP}
\alias{clogitP}

\title{R Commander Plug-in for Case 1 (Object Case) Best--Worst Scaling}

\description{
This package adds menu items to the R Commander for implementing case 1 
(object case) best--worst scaling (BWS1) from designing choice sets to 
measuring preferences for items. BWS1 is a question-based survey method 
that constructs various combinations of items (choice sets) using the 
experimental designs, asks respondents to select the best and worst 
items in each choice set, and then measures preferences for the items by 
analyzing the responses. For details on BWS1, refer to Louviere et al. 
(2015).
}

\details{
This package is an R Commander Plug-in package for case 1 best--worst 
scaling (BWS1). It depends on R packages \pkg{\link{crossdes}} (Sailer 2005),
 \pkg{\link{support.BWS}} (Aizaki et al. 2014), 
\pkg{\link{support.CEs}} (Aizaki 2012), and \pkg{survival} (Therneau 2020; 
Therneau and Grambsch 2000) as well as the \pkg{\link{Rcmdr}} package 
(Fox 2005, 2017; Fox and Bouchet-Valat 2020). Refer to 
\pkg{\link{support.BWS}} and \dQuote{Non-Market Valuation with R} 
<\url{http://lab.agr.hokudai.ac.jp/nmvr/}> for a brief introduction to BWS1 
in R.

After successfully installing and loading the \pkg{RcmdrPlugin.BWS1}, 
the R Commander window will appear, and then you will find \dQuote{BWS1} 
added to the top-level menus in the R Commander window.

Selecting \dQuote{BWS1} displays several menu items. When selecting a 
menu item, the corresponding dialog box is open. The following dialog 
boxes are provided by \pkg{RcmdrPlugin.BWS1}:
\itemize{
   \item The dialog box for \dQuote{Generate design...} generates a 
balanced incomplete block design (BIBD) for BWS1 using 
functions \code{\link[crossdes]{find.BIB}} and \code{\link[crossdes]{isGYD}} 
in the package \pkg{\link{crossdes}}.
   \item The dialog box for \dQuote{Set item...} sets all the items used 
in your BWS1 application and stores them as a character vector using R 
basic functions.
   \item The dialog box for \dQuote{Create questions...} creates a 
series of questions for BWS1 according to the BIBD generated and item 
names set in the previous dialog boxes using the 
function \code{\link[support.BWS]{bws.questionnaire}} in the 
package \pkg{\link{support.BWS}}.
   \item The dialog box for \dQuote{Synthesize responses...} generates 
artificial responses to BWS1 questions using the 
function \code{\link[support.BWS]{bws.response}} in \pkg{\link{support.BWS}}.
   \item The dialog box for \dQuote{Create data set for analysis...} 
creates a data set for analysis from the BIBD generated in the first 
dialog box and a data set containing responses to BWS1 questions using 
the function \code{\link[support.BWS]{bws.dataset}} 
in \pkg{\link{support.BWS}}.
   \item Selecting the menu item \dQuote{Measure preferences...} gives 
options: \dQuote{Counting approach} and \dQuote{Modeling approach}. Each 
has additional sub-items (see below).
   \item The dialog box for \dQuote{Calculate scores...} calculates 
various BW scores for each respondent from the data set for analysis 
created in the previous dialog box, and then stores them into a data 
frame using the function \code{\link[support.BWS]{bws.count}} 
in \pkg{\link{support.BWS}}.
   \item Selecting \dQuote{Summarize scores} summarizes the BW scores 
calculated in the previous dialog box and displays the results in the 
Output pane of the R Commander window.
   \item The dialog box for \dQuote{Draw distributions of scores...} 
draws distributions (bar plots) of the BW scores by item using an R 
basic graphic function barplot (S3 method for class \sQuote{bws.count2}: 
\code{\link[support.BWS]{bws.count}}).
   \item The dialog box for \dQuote{Draw mean scores...} draws a bar 
plot of means of the BW scores by item using the function barplot (S3 
method for class \sQuote{bws.count2}: \code{\link[support.BWS]{bws.count}}).
   \item The dialog box for \dQuote{Draw relationship between means and 
sds of scores...} draws a plot of the relationship between means and 
standard deviations of the BW scores by item using an R basic graphic 
function plot (S3 method for class \sQuote{bws.count2}: \code{\link[support.BWS]{bws.count}}).
   \item The dialog box for \dQuote{Fit model...} implements a modeling 
analysis using the data set for analysis generated in the previous 
dialog box using the functions \code{\link[survival]{clogit}} 
in \pkg{survival} and \code{\link[support.CEs]{gofm}} 
in \pkg{\link{support.CEs}}.
   \item The dialog box for \dQuote{Fit model with simple dialog box...} 
also implements a modeling analysis using \code{\link[survival]{clogit}} 
and \code{\link[support.CEs]{gofm}}. However, the method of usage is 
easier than that in \dQuote{Fit model...}.
   \item The dialog box for \dQuote{Calculate shares of preferences...} 
calculates shares of preferences for items from the model fitted in the 
previous dialog boxes using the function \code{\link[support.BWS]{bws.sp}} 
in \pkg{\link{support.BWS}}.
}
}

\author{
Hideo Aizaki
}

\references{
Aizaki, H (2012) Basic Functions for Supporting an Implementation of 
Choice Experiments in R. \emph{Journal of Statistical Software}, Code 
Snippets, 50(2): 1--24. \url{https://doi.org/10.18637/jss.v050.c02}.

Aizaki H, Nakatani T, Sato K (2014) \emph{Stated Preference Methods 
Using R}. Chapman and Hall/CRC. \url{https://doi.org/10.1201/b17292}.

Fox J (2005) The R Commander: A Basic Statistics Graphical User 
Interface to R. \emph{Journal of Statistical Software}, 14(9): 1--42. 
\url{https://doi.org/10.18637/jss.v014.i09}.

Fox J (2017) \emph{Using the R Commander: A Point-and-Click Interface 
for R}. Chapman and Hall/CRC. \url{https://doi.org/10.1201/9781315380537}.

Fox J, Bouchet-Valat M (2020) Rcmdr: R Commander. R package version 2.6-2.
 \url{http://socserv.socsci.mcmaster.ca/jfox/Misc/Rcmdr/}.

Louviere JJ, Flynn TN, Marley AAJ (2015) \emph{Best-Worst Scaling: 
Theory, Methods and Applications}. Cambridge University Press. 
\url{https://doi.org/10.1017/CBO9781107337855}.

Sailer O (2005) crossdes: A package for design and randomization in 
crossover studies. \emph{Rnews}, 5/2: 24--27.

Therneau T (2020) A Package for Survival Analysis in R. R package 
version 3.1-12. \url{https://CRAN.R-project.org/package=survival}.

Therneau TM, Grambsch PM (2000) \emph{Modeling Survival Data: Extending 
the Cox Model}. Springer. \url{https://doi.org/10.1007/978-1-4757-3294-8}.

}

\examples{
\dontrun{
library(RcmdrPlugin.BWS1)
}
}

\keyword{
package
}

