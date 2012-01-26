########## QUALTICS FUNCTIONS ############################################################
# For Qualtrics functions dates startDate and endDate format: YYYY-MM-DD
parseXMLResponse <- function(doc) {
	fields = xmlApply(doc, names)
	#unique(unlist(fields))
	ans = as.data.frame(replicate(length(unique(unlist(fields))), character(xmlSize(doc))), stringsAsFactors=FALSE)
	names(ans) = unique(unlist(fields))
	sapply(1:xmlSize(doc), 
		function(i) { 
			c = doc[[i]] #$element
			ans[i, names(c)] <<- xmlSApply(c, xmlValue) 
		})
	ans
}

#' Returns the results of a given survey.
#'
#' @param username the Qualtrics username.
#' @param password the Qualtrics password.
#' @param surveyid the Qualtrics survey id.
#' @param truncNames the maximum length of column names returned from qualtrics.
#' @param startDate beginning date range for results returned.
#' @param endDate ending date range for results returned.
#' @export
getSurveyResults <- function(username, password, surveyid, truncNames=20, startDate=NULL, endDate=NULL) {
	url = paste("http://new.qualtrics.com/Server/RestApi.php?Request=getResponseData&User=",
		username, "&Password=", password, "&SurveyID=", surveyid, "&Format=CSV", 
		ifelse(is.null(startDate), "", paste("&StartDate=", startDate, sep="")), 
		ifelse(is.null(endDate), "", paste("&EndDate=", endDate, sep="")),
		sep="")
	t = read.csv(url, skip=1)
	t$X = NULL
	n = strsplit(names(t), "....", fixed=TRUE)
	for(i in 1:ncol(t)) {
		names(t)[i] = n[[i]][length(n[[i]])]
		if(nchar(names(t)[i]) > truncNames) {
			names(t)[i] = substr(names(t)[i], 1, truncNames)
		}
	}
	t
}

#' Returns a list of surveys available for the given user.
#'
#' @param username the Qualtrics username.
#' @param password the Qualtrics password.
#' @export
getSurveys <- function(username, password) {
	url = paste("http://new.qualtrics.com/Server/RestApi.php?Request=getSurveys&User=",
		username, "&Password=", password, sep="")
	doc = xmlRoot(xmlTreeParse(url))
	parseXMLResponse(doc[[1]])
}

#' Returns the given survey.
#'
#' @param username the Qualtrics username.
#' @param password the Qualtrics password.
#' @param surveyid the Qualtrics survey id.
getSurvey <- function(username, password, surveyid) {
	url = paste("http://new.qualtrics.com/Server/RestApi.php?Request=getSurvey&User=",
		username, "&Password=", password, "&SurveyID=", surveyid, sep="")
	xmlRoot(xmlTreeParsedoc(url))
}

#' Returns the given survey name.
#'
#' @param username the Qualtrics username.
#' @param password the Qualtrics password.
#' @param surveyid the Qualtrics survey id.
getSurveyName <- function(username, password, surveyid) {
	url = paste("http://new.qualtrics.com/Server/RestApi.php?Request=getSurveyName&User=",
		username, "&Password=", password, "&SurveyID=", surveyid, sep="")
	doc = xmlRoot(xmlTreeParsedoc(url))
	fields = names(doc)
	ans = as.data.frame(replicate(length(unique(unlist(fields))), character()), stringsAsFactors=FALSE)
	names(ans) = unlist(names(fields))
	sapply(1:xmlSize(doc), 
		function(i) { 
			c = doc[[i]] #$element
			if(length(xmlValue(c)) > 0) {
				ans[1, i] <<- xmlValue(c)
			}
		})
	ans
}


