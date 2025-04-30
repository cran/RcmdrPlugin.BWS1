\name{bws1Response}
\alias{bws1Response}
\alias{bws1ResponseSet}

\title{Collect Responses to BWS1 Questions}

\description{
Collect user's responses to BWS1 questions.
}

\details{
Three types of dialog boxes are provided to collect user's responses to
BWS1 questions that are converted from the choice sets designed in 
the \sQuote{\code{Design Choice Sets for BWS1}} dialog box.

The first dialog box (\sQuote{\code{Set Options for Response Collection}})
sets three options for response collections:

\itemize{
\item \code{Design}: An R object name containing BWS1 choice sets
generated in the \sQuote{\code{Design Choice Sets for BWS1}} dialog box. 
The default name \sQuote{\code{BWS1design}} is displayed automatically.
The correct name should be entered if the name of BWS1 choice sets
(\code{BWS1design}) was changed in the dialog box for BWS1 design.

\item \code{Items}: An R object name containing the BWS1 items entered in
the \sQuote{\code{Design Choice Sets for BWS1}} dialog box. 
The default name \sQuote{\code{BWS1items}} is displayed automatically.
The correct name should be entered if the name of BWS1 items 
(\code{BWS1items}) was changed in the dialog box for BWS1 design.

\item \code{Save to file}: When the user's responses should be saved
to an external file in CSV format (\code{.csv}), check this box.
}

After confirming all the settings on the first dialog box, press
the \sQuote{\code{OK}} button to continue.

A series of dialog boxes (\sQuote{\code{Collect Responses to BWS1 Questions}})
sequentially opens to collect the user's responses to BWS1 questions.
In each dialog box, items designed in the \sQuote{\code{Design Choice Sets for BWS1}}
dialog box are presented. The user is asked to select their best and worst
items from the drop-down list boxes. After making the selection,
the user clicks the \sQuote{\code{OK}} button to open a new dialog box
corresponding to the next BWS1 question.

When all user responses to BWS1 questions have been collected, a dialog
box (\sQuote{\code{Save As}}) opens if the \sQuote{\code{Save to file}}
option was selected in the first dialog box. This saves the user's
responses to an external file in CSV format.
}
