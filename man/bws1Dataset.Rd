\name{bws1Dataset}
\alias{bws1Dataset}
\alias{resetBws1Dataset}
\alias{bws1Count2P}
\alias{bws1DataP}

\title{Create Data Set for BWS1}

\description{
Create a data set for BWS1 analysis from choice sets for BWS1 and data set 
containing responses to BWS1 questions.}

\details{
This dialog box uses the function \code{\link[support.BWS]{bws.dataset}} in 
\pkg{support.BWS} to create a data set for BWS1 analysis from the choice
sets designed in the \sQuote{\code{Design Choice Sets for BWS1}} dialog box
and a data set containing responses to BWS1 questions. 
This dialog box is activated if the data set containing responses to BWS1
questions is selected as the active data set. For details on the results of
this dialog box, refer to help for \code{\link[support.BWS]{bws.dataset}}.

GUI elements of this dialog box are as follows.

\itemize{
\item \code{Name for data set}: An R object name into which a data set
generated using this dialog box is stored. The default name 
\sQuote{\code{BWS1data}} can be changed following R's rules for naming
objects. If changed, you should take note of the new object name, which
will be used later.

\item \code{Save to file}: A check box for saving the resultant data set
to an external file in RDA format (\code{.rda}). 
If necessary, check this box.

\item \code{Design}: An R object name containing BWS1 choice sets generated
in the \sQuote{\code{Design Choice Sets for BWS1}} dialog box. 
The default name \sQuote{\code{BWS1design}} is
displayed automatically. The correct name should be entered if the name of
BWS1 choice sets (\code{BWS1design}) was changed in the dialog box for BWS1 
design.

\item \code{Items}: An R object name containing BWS1 items entered in 
the \sQuote{\code{Design Choice Sets for BWS1}} dialog box. 
The default name \sQuote{\code{BWS1items}} is displayed automatically. 
The correct name should be entered if the name of BWS1 items 
(\code{BWS1items}) was changed in the dialog box for BWS1 design.

\item \code{ID variable}: A variable name identifying respondents.
The default name \sQuote{\code{id}} can be changed according to 
the corresponding variable name in the active data set.

\item \code{Response variable format}: A format of response variable. 
Select one from two radio buttons corresponding to 
\sQuote{\code{Row number format}} and \sQuote{\code{Item number format}}.
See help for a function \code{\link[support.BWS]{bws.dataset}} for 
the definition of two formats.

\item \code{Model type}: The specific type of model used for an analysis. 
Select one from three radio buttons corresponding to 
\sQuote{\code{Maxdiff model}}, \sQuote{\code{Marginal model}}, and
\sQuote{\code{Marginal sequential model}}. 
Search help for a function \code{\link[support.BWS]{bws.dataset}} for 
details on three models.

\item \code{Number of BWS1 questions}: The number of BWS1 questions per
respondent. According to the design parameters set in 
the \sQuote{\code{Design Choice Sets for BWS1}} dialog box,
this slider is set to the appropriate number. Depending on the number,
the number of rows of the 
\sQuote{\code{Names of best- and worst-response variables}} table varies.

\item \code{Letters of best- and worst-response variables}: Types/formats
of letters used for best- and worst-variable names shown in the 
\sQuote{\code{Names of best- and worst-variable names}} table. 
Select one from three radio buttons corresponding to 
\sQuote{\code{Uppercase}}, \sQuote{\code{Lowercase}}, and 
\sQuote{\code{None}}. According to the selection, the letters of 
the best and worst variable names in the table are changed. 
When selecting \sQuote{\code{None}}, no variable names appear in the table. 
Users must type appropriate names in each cell of the table.

\item \code{Names of best- and worst-response variables}: Names of best- 
and worst-response variables stored in the active data set. These names
must follow R's rules for naming variables.
}

Please click the \sQuote{\code{Cancel}} button and open this dialog box again
if it does not work as expected.
}

