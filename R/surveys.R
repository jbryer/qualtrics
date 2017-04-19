#' Returns the results of a given survey.
#' 
#' You will need to a token id and survey id. This page on Qualtrics website
#' provides information on how to find the ids:
#' https://api.qualtrics.com/docs/parameters#finding-qualtrics-ids
#'
#' @param qualtrics.token authentication token.
#' @param surveyid the Qualtrics survey id.
#' @param qualtrics.baseurl The base URL for the Qualtrics API. See 
#'        https://api.qualtrics.com/docs/root-url for more information about
#'        how to determine your base URL.
#' @param truncNames the maximum length of column names returned from qualtrics.
#' @param startDate beginning date range for results returned.
#' @param endDate ending date range for results returned.
#' @param timeout maximum number of seconds to wait for the survey results.
#' @param ... for backwards compatibility.
#' @export
getSurveyResults <- function(qualtrics.token,
							 surveyid,
							 qualtrics.baseurl = "https://qualtrics.com",
							 truncNames = 20,
							 startDate = NULL,
							 endDate = NULL,
							 timeout = 300,
							 ...) {
	# get the url for download
	txt <- POST(
		paste0(qualtrics.baseurl, '/API/v3/responseexports/'),
		add_headers('content-type' = 'application/json',
					'x-api-token' = qualtrics.token),
		body = paste0('{"surveyId": "', surveyid, '", "format": "csv"}')
	)
	
	if (txt$status_code != 200) {
		stop(paste0('Error connecting to ', txt$url, '\nError: ', txt$status_code))
	}

	results <- content(txt)
	
	# Wait until the results file is ready
	start <- Sys.time()
	done <- FALSE
	while(!done) {
		if(as.double(difftime(Sys.time(), start, units = 'secs')) > timeout) {
			stop('Getting survey results timed out.')
		}
		txt.status <- GET(
			paste0(qualtrics.baseurl, '/API/v3/responseexports/', results$result$id),
			add_headers('x-api-token' = qualtrics.token)
		)
		done <- content(txt.status)$result$status == 'complete'
	}
	
	# download the file
	txt.file <- GET(content(txt.status)$result$file,
				    add_headers('x-api-token' = qualtrics.token))
	tmp.file <- tempfile(fileext = '.zip')
	writeBin(content(txt.file), tmp.file)
	
	# unzip the file
	file <- unzip(tmp.file, list = TRUE)[1, ]$Name
	dir <- tempdir()
	unzip(tmp.file, files = file, exdir = dir)
	
	t <- read.csv(paste0(dir, '/', file), skip = 2, stringsAsFactors = FALSE)
	
	t$X = NULL
	n = strsplit(names(t), "....", fixed = TRUE)
	for (i in 1:ncol(t)) {
		names(t)[i] = n[[i]][length(n[[i]])]
		if (nchar(names(t)[i]) > truncNames) {
			names(t)[i] = substr(names(t)[i], 1, truncNames)
		}
	}
	return(t)
}

#' Returns a list of surveys available for the given user.
#'
#' @export
getSurveys <- function(qualtrics.token,
					   qualtrics.baseurl = "https://qualtrics.com") {
	txt <- GET(
		paste0(qualtrics.baseurl, '/API/v3/surveys'),
		add_headers('x-api-token' = qualtrics.token),
		body = NULL
	)

	if (txt$status_code != 200) {
		stop(paste0('Error connecting to ', txt$url, '\nError: ', txt$status_code))
	}
	
	surveys <- data.frame(id = character(),
						  name = character(),
						  ownerId = character(),
						  lastModified = character(),
						  isActive = logical(),
						  stringsAsFactors = FALSE)
	
	# TODO: look for nextPage element to load more results
	
	for(i in seq_len(length(results[[1]]$elements))) {
		surveys <- rbind(surveys, data.frame(
			id = results[[1]]$elements[[i]]$id,
			name = results[[1]]$elements[[i]]$name,
			ownerId = results[[1]]$elements[[i]]$lastModified,
			isActive = results[[1]]$elements[[i]]$isActive
		))
	}
	
	return(surveys)
}

#' Returns the given survey.
#'
#' @param username the Qualtrics username.
#' @param password the Qualtrics password.
#' @param surveyid the Qualtrics survey id.
#' @export
getSurvey <- function(username, password, surveyid) {
	stop('Currently not supported!')
}

#' Returns the given survey name.
#'
#' @param username the Qualtrics username.
#' @param password the Qualtrics password.
#' @param surveyid the Qualtrics survey id.
#' @export
getSurveyName <- function(username, password, surveyid) {
	stop('Currently not supported!')
	# url = paste("http://eu.qualtrics.com/Server/RestApi.php?Request=getSurveyName&User=",
	# 	username, "&Password=", password, "&SurveyID=", surveyid, sep="")
	# doc = xmlRoot(xmlTreeParsedoc(url))
	# fields = names(doc)
	# ans = as.data.frame(replicate(length(unique(unlist(fields))), character()), 
	# 					stringsAsFactors=FALSE)
	# names(ans) = unlist(names(fields))
	# sapply(1:xmlSize(doc), 
	# 	function(i) { 
	# 		c = doc[[i]] #$element
	# 		if(length(xmlValue(c)) > 0) {
	# 			ans[1, i] <<- xmlValue(c)
	# 		}
	# 	})
	# ans
}


