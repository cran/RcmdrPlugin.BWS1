\name{bws1SharePreference}
\alias{bws1SharePreference}

\title{Calculate Shares of Preferences}

\description{
Calculate shares of preferences for items from the fitted model.
}

\details{
This dialog box calculates shares of preferences for items from 
the model fitted in the \sQuote{\code{Fit Model to BWS1 Data}} 
dialog box using the function 
\code{\link[support.BWS]{bws.sp}} in \pkg{support.BWS}.
This dialog box is activated if the fitted model is selected as
the active model. For details on the results of this dialog box,
refer to help for \code{\link[support.BWS]{bws.sp}}.

GUI elements of this dialog box are as follows.

\itemize{
\item \code{Base item}: An item used as the reference (base) item
in the model. This is selected automatically according to the setting
for fitting the model in the \sQuote{\code{Fit Model to BWS1 Data}}
dialog box.

\item \code{Item names (optional)}: Item names to be calculated.

\item \code{Order}: A type of sorting order. Select one from three
radio buttons corresponding to \sQuote{\code{None}}, 
\sQuote{\code{Increasing}}, and \sQuote{\code{Decreasing}}.
}
}

