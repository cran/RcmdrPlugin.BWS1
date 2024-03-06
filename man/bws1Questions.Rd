\name{bws1Questions}
\alias{bws1Questions}


\title{Display BWS1 Questions}

\description{
Displays a series of questions for BWS1.}

\details{
This dialog box displays a series of questions for BWS1 according to the
choice sets designed in the \sQuote{\code{Design Choice Sets for BWS1}} dialog
box using the function
\code{\link[support.BWS]{bws.questionnaire}} in \pkg{support.BWS}. 
For details on the results of this dialog box, refer to the help content for 
\code{\link[support.BWS]{bws.questionnaire}}.

GUI elements of this dialog box are as follows.

\itemize{
\item \code{Design}: An R object name containing the BWS1 choice sets
generated in the \sQuote{\code{Design Choice Sets for BWS1}} dialog box.
The default name \sQuote{\code{BWS1design}} is displayed automatically. 
The correct name should be entered if the name of the BWS1
choice sets (\code{BWS1design}) was changed in the dialog box for BWS1
design.

\item \code{Items}: An R object name containing the BWS1 items entered in
the \sQuote{\code{Design Choice Sets for BWS1}} dialog box. 
The default name \sQuote{\code{BWS1items}} is displayed automatically.
The correct name should be entered if the name of  BWS1 items 
(\code{BWS1items}) was changed in the dialog box for BWS1 design.
}
}

