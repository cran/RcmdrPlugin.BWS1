\name{bws1Design}
\alias{bws1Design}
\alias{resetBws1Items}

\title{Design Choice Sets for BWS1}

\description{
Design choice sets for BWS1 with a balanced incomplete block design.
}

\details{
This dialog box uses the functions \code{\link[crossdes]{find.BIB}} and 
\code{\link[crossdes]{isGYD}} in package \pkg{crossdes} to
design choice sets for BWS1 with a balanced incomplete block design. 
For details on the results of this dialog box, refer to the help content 
for the functions \code{\link[crossdes]{find.BIB}} and 
\code{\link[crossdes]{isGYD}}.

GUI elements of this dialog box are as follows.

\itemize{
\item \code{Name for design}: An R object name into which choice sets 
generated using this dialog box are stored. The default name 
\sQuote{\code{BWS1design}} can be changed following R's rules for naming
objects. If changed, you should take note of the new object name, 
which will be used later.

\item \code{Name for items}: An object name for items entered in the 
\sQuote{\code{Items}} table (explained later). 
The default name \sQuote{\code{BWS1items}} can be changed following 
R's rules for naming objects. 
If changed, you should take note of the new object name, 
which will be used later.

\item \code{Save to file}: A check box for saving both the resultant design
object and the items object to a single external file in RDA 
format (\code{.rda}). If necessary, check this box.

\item \code{Number of items}: A total number of items to be evaluated in
BWS1 questions.

\item \code{Number of questions}: A total number of BWS1 questions.

\item \code{Number of items per question}: The number of items that appeared
in each BWS1 question.

\item \code{Items}: Item names. As items typed into this table will be used as variables
in the following process, item names must follow R's rules for naming
variables. Type names without skipping from \sQuote{\code{Item 1}}.

\item \code{Number of iterations}: Total number of iterations for searching
for a design that meets the design parameters assigned above. 
The default is \code{1000}.

\item \code{Seed for random number generator (optional)}: An integer value
used as a seed for generating random numbers. If reproducibility is required,
type an arbitrary value into this box.
}
}

