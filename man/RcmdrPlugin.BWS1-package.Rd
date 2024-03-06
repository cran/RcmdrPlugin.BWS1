\docType{package}
\name{RcmdrPlugin.BWS1-package}
\alias{RcmdrPlugin.BWS1-package}
\alias{RcmdrPlugin.BWS1}
\alias{bws1CountSummary}
\alias{bws1Load}


\title{R Commander Plug-in for Case 1 Best--Worst Scaling}

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
This package is an R Commander Plug-in package for BWS1.
It depends on R packages \pkg{\link{crossdes}} (Sailer 2005),
 \pkg{\link{support.BWS}} (Aizaki 2021; Aizaki and Fogarty 2023; Aizaki et al. 2014), 
\pkg{\link{support.CEs}} (Aizaki 2012), and \pkg{survival} (Therneau 2021; 
Therneau and Grambsch 2000) as well as the \pkg{\link{Rcmdr}} package 
(Fox 2005, 2017ab; Fox and Bouchet-Valat 2021). Refer to 
Aizaki and Fogarty (2023) and \sQuote{Non-Market Valuation with R} 
<\url{http://lab.agr.hokudai.ac.jp/nmvr/}> for a brief introduction to BWS1 
in R.

After successfully installing and loading \pkg{RcmdrPlugin.BWS1}, 
the R Commander window will appear, and then you will find \sQuote{\code{BWS1}} 
added to the top-level menus in the R Commander window.

Selecting \sQuote{\code{BWS1}} displays several menu items. When selecting a 
menu item, the corresponding dialog box opens. The following dialog 
boxes are provided by \pkg{RcmdrPlugin.BWS1}:

\itemize{
   \item The dialog box for \sQuote{\code{Design choice sets...}} designs choice sets 
for BWS1 with an balanced incomplete block design (BIBD) using 
functions \code{\link[crossdes]{find.BIB}} and \code{\link[crossdes]{isGYD}} 
in the package \pkg{\link{crossdes}}.

   \item The dialog box for \sQuote{\code{Display questions...}} displays a 
series of questions for BWS1 according to choice sets designed in 
the previous dialog box using the function 
\code{\link[support.BWS]{bws.questionnaire}} in the package \pkg{\link{support.BWS}}.

   \item The dialog box for \sQuote{\code{Create data set for analysis...}} 
creates a data set for analysis from the BIBD generated in the first 
dialog box and a data set containing responses to BWS1 questions using 
the function \code{\link[support.BWS]{bws.dataset}} 
in \pkg{\link{support.BWS}}.

   \item Selecting the menu item \sQuote{\code{Measure preferences...}} gives 
options: \sQuote{\code{Counting approach}} and \sQuote{\code{Modeling approach}}.
Each has additional sub-items (see below).

   \item The dialog box for \sQuote{\code{Calculate scores...}} calculates 
various BW scores for each respondent from the data set for BWS1 analysis 
created in the previous dialog box, and then stores them into a data 
frame using the function \code{\link[support.BWS]{bws.count}} 
in \pkg{\link{support.BWS}}.

   \item Selecting \sQuote{\code{Summarize scores}} summarizes the BW scores 
calculated in the previous dialog box and displays the results in the 
Output pane of the R Commander window.

   \item The dialog box for \sQuote{\code{Draw distributions of scores...}} 
draws distributions (bar plots) of the BW scores by item using an R 
basic graphic function \code{barplot} (S3 method for class \sQuote{bws.count2}: 
\code{\link[support.BWS]{bws.count}}).

   \item The dialog box for \sQuote{\code{Draw mean scores...}} draws a bar 
plot of means of the BW scores by item using the function \code{barplot} (S3 
method for class \sQuote{bws.count2}: \code{\link[support.BWS]{bws.count}}).

   \item The dialog box for \sQuote{\code{Draw relationship between means and 
sds of scores...}} draws a plot of the relationship between means and 
standard deviations of the BW scores by item using an R basic graphic 
function \code{plot} (S3 method for class \sQuote{bws.count2}: 
\code{\link[support.BWS]{bws.count}}).

   \item The dialog box for \sQuote{\code{Fit model...}} implements a modeling 
analysis using the data set for BWS1 analysis generated in the previous 
dialog box using the functions \code{\link[survival]{clogit}} 
in \pkg{survival} and \code{\link[support.CEs]{gofm}} 
in \pkg{\link{support.CEs}}.

   \item The dialog box for \sQuote{\code{Calculate shares of preferences...}} 
calculates shares of preferences for items from the model fitted in the 
previous dialog boxes using the function \code{\link[support.BWS]{bws.sp}} 
in \pkg{\link{support.BWS}}.

   \item The dialog box for \sQuote{\code{Load saved objects...}} loads 
an R Data (rda) file saved in the dialog box for \sQuote{\code{Design choice sets...}} 
or \sQuote{\code{Create data set for analysis...}} using the function 
\code{\link[base]{load}} in \pkg{\link{base}}.
}
}

\section{Acknowledgments}{
This work was supported by JSPS KAKENHI Grant Number JP20K06251.
}


\author{
Hideo Aizaki
}

\references{
Aizaki H (2012) Basic Functions for Supporting an Implementation of 
Choice Experiments in R. \emph{Journal of Statistical Software}, Code 
Snippets, 50(2): 1--24. DOI: 10.18637/jss.v050.c02

Aizaki H (2021) support.BWS: Tools for Case 1 Best-Worst Scaling. 
R package version 0.4-4. \url{https://CRAN.R-project.org/package=support.BWS}.

Aizaki H, Fogarty J (2023) R packages and tutorial for case 1 best-worst scaling. 
\emph{Journal of Choice Modelling}, 46: 100394. DOI: 10.1016/j.jocm.2022.100394

Aizaki H, Nakatani T, Sato K (2014) \emph{Stated Preference Methods 
Using R}. Chapman and Hall/CRC. DOI: 10.1201/b17292

Fox J (2005) The R Commander: A Basic Statistics Graphical User 
Interface to R. \emph{Journal of Statistical Software}, 14(9): 1--42. 
DOI: 10.18637/jss.v014.i09

Fox J (2017a) \emph{Using the R Commander: A Point-and-Click Interface 
for R}. Chapman and Hall/CRC. DOI: 10.1201/9781315380537

Fox J (2017b) Writing R Commander Plug-in Packages. Version 3.4-0. 
\url{https://socialsciences.mcmaster.ca/jfox/Books/RCommander/plug-ins.html}.

Fox J, Bouchet-Valat M (2021) Rcmdr: R Commander. R package version 2.7-1.
 \url{https://socialsciences.mcmaster.ca/jfox/Misc/Rcmdr/}.

Louviere JJ, Flynn TN, Marley AAJ (2015) \emph{Best-Worst Scaling: 
Theory, Methods and Applications}. Cambridge University Press. 
DOI: 10.1017/CBO9781107337855

Sailer O (2005) crossdes: A package for design and randomization in 
crossover studies. \emph{Rnews}, 5/2: 24--27.

Therneau T (2021) survival: Survival Analysis. R package version 3.2-13. 
\url{https://CRAN.R-project.org/package=survival}.

Therneau TM, Grambsch PM (2000) \emph{Modeling Survival Data: Extending 
the Cox Model}. Springer. DOI: 10.1007/978-1-4757-3294-8

}

\examples{
if (interactive()) {
  library(RcmdrPlugin.BWS1)
}
}

\keyword{
package
}

