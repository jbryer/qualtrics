#' Returns the results of a given survey.
#'
#' @param username the Qualtrics username.
#' @param password the Qualtrics password.
#' @param surveyid the Qualtrics survey id.
#' @param truncNames the maximum length of column names returned from qualtrics.
#' @param startDate beginning date range for results returned.
#' @param endDate ending date range for results returned.
#' @export
getSurveyResults <- function(username, password, surveyid, 
							 truncNames=20, startDate=NULL, endDate=NULL) {
	url = paste("http://eu.qualtrics.com/Server/RestApi.php?Request=getResponseData&User=",
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
	url <- paste("http://eu.qualtrics.com/Server/RestApi.php?Request=getSurveys&User=",
		username, "&Password=", password, sep="")
	doc <- xmlRoot(xmlTreeParse(url))
	df <- parseXMLResponse(doc[['Surveys']])
	return(df)
}

#' Returns the given survey.
#'
#' @param username the Qualtrics username.
#' @param password the Qualtrics password.
#' @param surveyid the Qualtrics survey id.
#' @export
getSurvey <- function(username, password, surveyid) {
	url = paste("http://eu.qualtrics.com/Server/RestApi.php?Request=getSurvey&User=",
		username, "&Password=", password, "&SurveyID=", surveyid, sep="")
	xmlRoot(xmlTreeParse(url))
}

#' Returns the given survey name.
#'
#' @param username the Qualtrics username.
#' @param password the Qualtrics password.
#' @param surveyid the Qualtrics survey id.
#' @export
getSurveyName <- function(username, password, surveyid) {
	url = paste("http://eu.qualtrics.com/Server/RestApi.php?Request=getSurveyName&User=",
		username, "&Password=", password, "&SurveyID=", surveyid, sep="")
	doc = xmlRoot(xmlTreeParsedoc(url))
	fields = names(doc)
	ans = as.data.frame(replicate(length(unique(unlist(fields))), character()), 
						stringsAsFactors=FALSE)
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


