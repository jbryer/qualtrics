#' Returns the results of a given survey.
#'
#' @param username the Qualtrics username.
#' @param password the Qualtrics password or token.
#' @param surveyid the Qualtrics survey id.
#' @param astoken use password as token?
#' @param truncNames the maximum length of column names returned from qualtrics.
#' @param startDate beginning date range for results returned.
#' @param endDate ending date range for results returned.
#' @param rooturl root URL for API calls.
#' @export
getSurveyResults <- function(username, password, surveyid, astoken=FALSE,
							               truncNames=20, startDate=NULL, endDate=NULL,
							               rooturl="http://eu.qualtrics.com/Server/RestApi.php") {
  filter <- paste0(ifelse(is.null(startDate), "", paste0("&StartDate=", startDate)), 
                   ifelse(is.null(endDate), "", paste0("&EndDate=", endDate)))
  if(astoken == FALSE) {
    url <- paste0(rooturl, "?Request=getResponseData", "&User=", username, 
                  "&Password=", password, "&SurveyID=", surveyid, 
                  "&Format=CSV", filter)
  } else {
    url <- paste0(rooturl, "?Request=getLegacyResponseData", "&User=", username, 
                  "&Token=", password, "&SurveyID=", surveyid, 
                  "&Version=2.3", "&Format=CSV", filter)
  }
  df <- read.csv(url, skip=1)
  df$X <- NULL
  n <- strsplit(names(df), "....", fixed=TRUE)
  for(i in 1:ncol(df)) {
    names(df)[i] <- n[[i]][length(n[[i]])]
    if(nchar(names(df)[i]) > truncNames) {
      names(df)[i] <- substr(names(df)[i], 1, truncNames)
    }
  }
	return(df)
}

#' Returns a list of surveys available for the given user.
#'
#' @param username the Qualtrics username.
#' @param password the Qualtrics password or token.
#' @param astoken use password as token?
#' @param rooturl root URL for API calls.
#' @export
getSurveys <- function(username, password, astoken=FALSE,
                       rooturl="http://eu.qualtrics.com/Server/RestApi.php") {
  if(astoken == FALSE) {
    url <- paste0(rooturl, "?Request=getSurveys", "&User=", username, 
                  "&Password=", password)
    doc <- xmlRoot(xmlTreeParse(url))
    df <- parseXMLResponse(doc[['Surveys']])
  } else {
    url <- paste0(rooturl, "?Request=getSurveys", "&User=", username, 
                  "&Token=", password, "&Version=2.3", "&Format=XML")
    doc <- xmlRoot(xmlTreeParse(url))
    df <- parseXMLResponse(doc[['Result']][['Surveys']])
  }
  return(df)
}

#' Returns the given survey.
#'
#' @param username the Qualtrics username.
#' @param password the Qualtrics password.
#' @param surveyid the Qualtrics survey id.
#' @param astoken use the password as a token?
#' @param rooturl root URL for API calls.
#' @export
getSurvey <- function(username, password, surveyid, astoken=FALSE,
                      rooturl="http://eu.qualtrics.com/Server/RestApi.php") {
  if(astoken == FALSE) {
    url <- paste0(rooturl, "?Request=getSurvey", "&User=", username, 
                  "&Password=", password, "&SurveyID=", surveyid)
  } else {
    url <- paste0(rooturl, "?Request=getSurvey", "&User=", username, 
                  "&Token=", password, "&SurveyID=", surveyid,
                  "&Version=", version, "&Format=XML")
  }
	xmlRoot(xmlTreeParse(url))
}

#' Returns the given survey name.
#'
#' @param username the Qualtrics username.
#' @param password the Qualtrics password.
#' @param surveyid the Qualtrics survey id.
#' @param astoken use the password as a token?
#' @param rooturl root URL for API calls.
#' @export
getSurveyName <- function(username, password, surveyid, astoken=FALSE,
                          rooturl="http://eu.qualtrics.com/Server/RestApi.php") {
  if(astoken == FALSE) {
    url <- paste0(rooturl, "?Request=getSurveyName", "&User", username, 
                  "&Password=", password, "&SurveyID=", surveyid)
    doc <- xmlRoot(xmlTreeParse(url))
  } else {
    url <- paste0(rooturl, "?Request=getSurveyName", "&User=", username, 
                  "&Token=", password, "&SurveyID=", surveyid, 
                  "&Version=2.3", "&Format=XML")
    doc <- xmlRoot(xmlTreeParse(url))[['Result']]
  }
  fields <- names(doc)
  ans <- as.data.frame(replicate(length(unique(unlist(fields))), character()), 
                       stringsAsFactors=FALSE)
  names(ans) <- unlist(names(fields))
  sapply(1:xmlSize(doc), 
         function(i) { 
           c = doc[[i]] #$element
           if(length(xmlValue(c)) > 0) {
             ans[1, i] <<- xmlValue(c)
           }
         })
  return(ans$SurveyName)
}
