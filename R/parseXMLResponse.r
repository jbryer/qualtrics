#' Internal method. Parses XML response from Qualtrics.
#' 
#' For Qualtrics functions dates startDate and endDate format: YYYY-MM-DD
#' 
parseXMLResponse <- function(doc) {
	fields = xmlApply(doc, names)
	#unique(unlist(fields))
	ans = as.data.frame(replicate(length(unique(unlist(fields))), 
								  character(xmlSize(doc))), stringsAsFactors=FALSE)
	names(ans) = unique(unlist(fields))
	sapply(1:xmlSize(doc), 
		   function(i) { 
		   	c = doc[[i]] #$element
		   	ans[i, names(c)] <<- xmlSApply(c, xmlValue) 
		   })
	ans
}
