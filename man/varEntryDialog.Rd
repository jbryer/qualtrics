\name{varEntryDialog}
\alias{varEntryDialog}
\title{Creates a dialog box using tcl/tk to get input from the user.}
\usage{
  varEntryDialog(vars, labels = vars,
    show = rep(NA, length(vars)),
    fun = rep(list(as.character), length(vars)),
    title = "Variable Entry", prompt = NULL)
}
\arguments{
  \item{vars}{character list of variable names. These will
  be the element names within the returned list.}

  \item{labels}{the labels the user will see for each
  variable entry.}

  \item{show}{a list of the same length of vars specifying
  how input should be shown.  For example, use '*' for
  password entry. Is NA by default.}

  \item{fun}{list of functions that converts the user
  input.}

  \item{title}{the title of the dialog box.}

  \item{prompt}{the prompt the user will see on the dialog
  box.}
}
\value{
  a \code{\link{list}} of named values entered by the user.
}
\description{
  This function will create a tcl/tk dialog box to get user
  input. It has been written to be extensible so the R
  programmer can easily create a dialog with any number of
  varaibles with custom labels and data conversion of the
  user entered data. The function will return a list where
  the element names are \code{vars} and the value is the
  user input. By default, all entry will be converted using
  the \code{as.character} function. However, this can
  easily be altered using the \code{fun} parameter. For
  example, if integers are required, use
  \code{fun=c(as.integer, ...)}. It is also possible to
  write custom functions that can serve as a data
  validation. See the examples.
}
\details{
  Adopted from Kay Cichini:
  \url{http://thebiobucket.blogspot.com/2012/08/tcltk-gui-example-with-variable-input.html}
  See also:
  \url{http://bioinf.wehi.edu.au/~wettenhall/RTclTkExamples/OKCancelDialog.html}
}

