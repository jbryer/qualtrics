#' Creates a dialog box using tcl/tk to get input from the user.
#' 
#' This function will create a tcl/tk dialog box to get user input. It has been
#' written to be extensible so the R programmer can easily create a dialog with
#' any number of varaibles with custom labels and data conversion of the user
#' entered data. The function will return a list where the element names are
#' \code{vars} and the value is the user input. By default, all entry will be
#' converted using the \code{as.character} function. However, this can easily be
#' altered using the \code{fun} parameter. For example, if integers are required,
#' use \code{fun=c(as.integer, ...)}. It is also possible to write custom
#' functions that can serve as a data validation. See the examples.
#'
#' Adopted from Kay Cichini:
#' \url{http://thebiobucket.blogspot.com/2012/08/tcltk-gui-example-with-variable-input.html}
#' See also: 
#' \url{http://bioinf.wehi.edu.au/~wettenhall/RTclTkExamples/OKCancelDialog.html}
#' 
#' @param vars character list of variable names. These will be the element names 
#'        within the returned list.
#' @param labels the labels the user will see for each variable entry.
#' @param show a list of the same length of vars specifying how input should be shown.
#'        For example, use '*' for password entry. Is NA by default.
#' @param fun list of functions that converts the user input.
#' @param title the title of the dialog box.
#' @param prompt the prompt the user will see on the dialog box.
#' @return a \code{\link{list}} of named values entered by the user.
#' @export
varEntryDialog <- function(vars, 
						   labels = vars,
						   show = rep(NA, length(vars)),
						   fun = rep(list(as.character), length(vars)),
						   title = 'Variable Entry',
						   prompt = NULL) {
	require(tcltk)
	
	stopifnot(length(vars) == length(labels), length(labels) == length(fun))

	# Create a variable to keep track of the state of the dialog window:
	# done = 0; If the window is active
	# done = 1; If the window has been closed using the OK button
	# done = 2; If the window has been closed using the Cancel button or destroyed
	done <- tclVar(0)

	tt <- tktoplevel()
	tkwm.title(tt, title)	
	entries <- list()
	tclvars <- list()

	# Capture the event "Destroy" (e.g. Alt-F4 in Windows) and when this happens, 
	# assign 2 to done.
	tkbind(tt,"<Destroy>",function() tclvalue(done)<-2)
	
	for(i in seq_along(vars)) {
		tclvars[[i]] <- tclVar("")
		if(is.na(show[i]) | is.null(show[i])) {
			entries[[i]] <- tkentry(tt, textvariable=tclvars[[i]])
		} else {
			entries[[i]] <- tkentry(tt, textvariable=tclvars[[i]], show=show[i])			
		}
	}
	
	doneVal <- as.integer(tclvalue(done))
	results <- list()

	reset <- function() {
		for(i in seq_along(entries)) {
			tclvalue(tclvars[[i]]) <<- ""
		}
	}
	reset.but <- tkbutton(tt, text="Reset", command=reset)
	
	cancel <- function() {
		tclvalue(done) <- 2
	}
	cancel.but <- tkbutton(tt, text='Cancel', command=cancel)
	
	submit <- function() {
		for(i in seq_along(vars)) {
			tryCatch( {
				results[[vars[[i]]]] <<- fun[[i]](tclvalue(tclvars[[i]]))
				tclvalue(done) <- 1
				},
				error = function(e) { tkmessageBox(message=geterrmessage()) },
				finally = { }
			)
		}
	}
	submit.but <- tkbutton(tt, text="Submit", command=submit)
	
	if(!is.null(prompt)) {
		tkgrid(tklabel(tt,text=prompt), columnspan=3, pady=10)
	}
	
	for(i in seq_along(vars)) {
		tkgrid(tklabel(tt, text=labels[i]), entries[[i]], pady=10, padx=10, columnspan=4)
	}
	
	tkgrid(submit.but, cancel.but, reset.but, pady=10, padx=10, columnspan=3)
	tkfocus(tt)

	# Do not proceed with the following code until the variable done is non-zero.
	#   (But other processes can still run, i.e. the system is not frozen.)
	tkwait.variable(done)
	
	if(tclvalue(done) != 1) {
		results <- NULL
	}
	
	tkdestroy(tt)
	return(results)
}

if(FALSE) { #Test the dialog
	vals <- varEntryDialog(vars=c('Variable1', 'Variable2'))
	str(vals)
	#Test using password field. Note that the variable will still be stored in
	#plain text within R
	vals <- varEntryDialog(vars=c('Username', 'Password'), show=c(NA,'*'))
	str(vals) #Be warned you can see the value of the password
	vals <- varEntryDialog(vars=c('Var1', 'Var2'), 
						  labels=c('Enter an integer:', 'Enter a string:'),
						  fun=c(as.integer, as.character))
	str(vals)
	#Add a custom validation function
	vals <- varEntryDialog(vars=c('Var1'),
						 labels=c('Enter an integer between 0 and 10:'),
						 fun=c(function(x) {
						 	x <- as.integer(x)
						 	if(x >= 0 & x <= 10) {
						 		return(x)
						 	} else {
						 		stop(paste("Why didn't you follow instruction!\n",
						 			 "Please enter a number between 0 and 10."))
						 	}
						 } ))
	str(vals)
	#Return a list
	vals <- varEntryDialog(vars=c('Var1'),
						  labels=c('Enter a comma separated list of something:'),
						  fun=c(function(x) {
						  	return(strsplit(x, split=','))
						  }))
	vals$Var1
	str(vals)
}
