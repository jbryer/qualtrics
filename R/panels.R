#' Returns the mailing lists in the given library.
#'
#' @param qualtrics.token authentication token.
#' @param qualtrics.baseurl The base URL for the Qualtrics API. See 
#'        https://api.qualtrics.com/docs/root-url for more information about
#'        how to determine your base URL.
#' @param verbose if TRUE, a statement will be printed when retrieving 
#'        subsequent pages.
#' @export
getMailingLists <- function(...) {
	return(getPanels(...))
}

#' Returns the survey panels in the given library.
#'
#' @param qualtrics.token authentication token.
#' @param qualtrics.baseurl The base URL for the Qualtrics API. See 
#'        https://api.qualtrics.com/docs/root-url for more information about
#'        how to determine your base URL.
#' @param verbose if TRUE, a statement will be printed when retrieving 
#'        subsequent pages.
#' @export
getPanels <- function(qualtrics.token,
					  qualtrics.baseurl = "https://qualtrics.com",
					  verbose = FALSE) {
	txt <- GET(paste0(qualtrics.baseurl, '/API/v3/mailinglists/'),
			   add_headers('content-type' = 'application/json',
			   			'x-api-token' = qualtrics.token)
	)
	if (txt$status_code != 200) {
		stop(paste0('Error connecting to ', txt$url, '\nError: ', txt$status_code))
	}

	lists <- content(txt)[[1]]
	mailinglists <- data.frame(libraryId = character(),
							   id = character(),
							   name = character(),
							   category = character(),
							   # folder = character(),
							   stringsAsFactors = FALSE)
	while(length(lists$elements) > 0) {
		for(i in seq_len(length(lists$elements))) {
			mailinglists <- rbind(mailinglists, data.frame(
				libraryId = lists$elements[[i]]$libraryId,
				id = lists$elements[[i]]$id,
				name = lists$elements[[i]]$name,
				category = lists$elements[[i]]$category,
				# folder = lists$elements[[i]]$folder,
				stringsAsFactors = FALSE
			))
		}
		if(!is.null(lists$nextPage)) {
			if(verbose) { print(lists$nextPage) }
			txt <- GET(lists$nextPage,
					   add_headers('content-type' = 'application/json',
					   			'x-api-token' = qualtrics.token) )
			if (txt$status_code != 200) {
				stop(paste0('Error connecting to ', txt$url, '\nError: ', txt$status_code))
			}
			lists <- content(txt)[[1]]
		} else {
			lists$elements <- list()
		}
	}
	return(mailinglists)
}

#' Returns the survey invitees for the given panel.
#'
#' @param username the Qualtrics username.
#' @param password the Qualtrics password.
#' @param libraryid the Qualtrics library id.
#' @param panelid the Qualtrics panel id.
#' @param embeddedData embedded data to return.
#' @export
getPanel <- function(username, password, libraryid, panelid, embeddedData=NULL) {
	#TODO: If we use the XML format, embedded data would be included. 
	#      Parameter is a comma separated list.
	url = paste("http://eu.qualtrics.com/Server/RestApi.php?Request=getPanel",
				"&User=", username, 
				"&Password=", password, 
				"&LibraryID=", libraryid, 
				"&PanelID=", panelid, 
				"&Format=CSV", 
				ifelse(is.null(embeddedData), "", 
					   paste("&EmbeddedData=", embeddedData, sep="")), 
				sep="")
	t = read.csv(url)
	t$X = NULL
	t
}

getPanelXML <- function(username, password, libraryid, panelid) {
	url = paste("http://eu.qualtrics.com/Server/RestApi.php?Request=getPanel",
				"&User=", username, 
				"&Password=", password, 
				"&LibraryID=", libraryid, 
				"&PanelID=", panelid, 
				"&Format=XML&RecipientHistory=1", 
				sep="")
	doc = xmlRoot(xmlTreeParse(url))
	doc
}

#' Returns the number of members in the panel.
#'
#' @param username the Qualtrics username.
#' @param password the Qualtrics password.
#' @param libraryid the Qualtrics library id.
#' @param panelid the Qualtrics panel id.
#' @export
getPanelMemberCount <- function(username, password, libraryid, panelid) {
	url = paste("http://eu.qualtrics.com/Server/RestApi.php?Request=getPanelMemberCount",
				"&User=", username, 
				"&Password=", password, 
				"&LibraryID=", libraryid, 
				"&PanelID=", panelid, 
				sep="")
	doc = xmlRoot(xmlTreeParse(url))
	as.numeric(xmlValue(doc[[1]]))
}

#' Creates a new panel.
#'
#' @param username the Qualtrics username.
#' @param password the Qualtrics password.
#' @param libraryId the Qualtrics library id.
#' @param name the panel name.
#' @param category the panel's category (optional).
#' @export
createPanel <- function(username, password, libraryId, name, category=NULL) {
	url = paste("http://eu.qualtrics.com/Server/RestApi.php?Request=createPanel",
				"&User=", username, 
				"&Password=", password, 
				"&LibraryID=", libraryId, 
				"&Name=", name,
				ifelse(is.null(category), "", paste("&Category=", category, sep="")),
				sep="")
	doc = xmlRoot(xmlTreeParse(url))
	as.character(xmlValue(doc[[1]]))
}

#' Adds a recipient to the given panel.
#'
#' @param username the Qualtrics username.
#' @param password the Qualtrics password.
#' @param libraryId the Qualtrics library id.
#' @param panelId the Qualtrics panel id.
#' @param firstName the recipient's first name.
#' @param lastName the recipient's last name.
#' @param email the recipient's email address.
#' @param externalDataRef Qualtrics extrenral data ref.
#' @param embeddedData embedded data for the recipient.
#' @export
addRecipient <- function(username, password, libraryId, panelId, firstName=NULL, 
			lastName=NULL, email=NULL, externalDataRef=NULL, embeddedData=NULL) {
	url = paste("http://eu.qualtrics.com/Server/RestApi.php?Request=addRecipient",
			"&User=", username, 
			"&Password=", password, 
			"&LibraryID=", libraryId, 
			"&PanelID=", panelId,
			ifelse(is.null(firstName), "", paste("&FirstName=", firstName, sep="")),
			ifelse(is.null(lastName), "", paste("&LastName=", lastName, sep="")),
			ifelse(is.null(email), "", paste("&Email=", email, sep="")),
			ifelse(is.null(externalDataRef), "", 
				   paste("&ExternalDataRef", externalDataRef, sep="")),
			sep="")
	if(!is.null(embeddedData)) {
		for(i in names(embeddedData)) {
			url = paste(url, "&ED[", i, "]=", as.character(embeddedData[1,i]), sep="")
		}
	}
	doc = xmlRoot(xmlTreeParse(url))
	as.character(xmlValue(doc[[1]]))
}

#' Returns a recipient from the panel.
#'
#' @param username the Qualtrics username.
#' @param password the Qualtrics password.
#' @param libraryId the Qualtrics library id.
#' @param recipientId the id for the recipient to return.
#' @export
getRecipient <- function(username, password, recipientId, libraryId) {
	url = paste("http://eu.qualtrics.com/Server/RestApi.php?Request=getRecipient",
				"&User=", username, 
				"&Password=", password, 
				"&RecipientID=", recipientId, 
				"&LibraryID=", libraryId, 
		sep="")
	doc = xmlRoot(xmlTreeParse(url))
	doc
}

sendSurveyToPanel <- function(username, password, surveyId, sendDate, fromEmail, 
							  fromName, subject, messageId, messageLibraryId, 
							  panelId, panelLibraryId, expirationDate=NULL, linkType=NULL) {
#todo:
}

#' Sends a survey to the given recipient.
#'
#' @param username the Qualtrics username.
#' @param password the Qualtrics password.
#' @param surveyId the Qualtrics surveyId.
#' @param sendDate the date to send the invitation.
#' @param fromEmail the email address of the sender.
#' @param fromName the name of the sender.
#' @param subject the subject of the email to send.
#' @param messageId the Qualtrics message id from the library.
#' @param messageLibraryId the Qualtrics library id containing the message to send.
#' @param panelId the Qualtrics panel id.
#' @param panelLibraryId the Qualtrics library id containing the panel.
#' @param recipientId the id of the recipient in the panel.
#' @export
sendSurveyToIndividual <- function(username, password, surveyId, sendDate, 
			fromEmail, fromName, subject, messageId, messageLibraryId, panelId, 
			panelLibraryId, recipientId) {
	url = paste("http://eu.qualtrics.com/Server/RestApi.php?Request=sendSurveyToIndividual",
				"&User=", username, 
				"&Password=", password, 
				"&SurveyID=", surveyId, 
				"&SendDate=", sendDate, 
				"&FromEmail=", fromEmail, 
				"&FromName=", fromName, 
				"&Subject=", subject, 
				"&MessageID=", messageId, 
				"&MessageLibraryID=", messageLibraryId, 
				"&PanelID=", panelId, 
				"&PanelLibraryID=", panelLibraryId,
		"&RecipientID=", recipientId,
		sep="")
	doc = xmlRoot(xmlTreeParse(url))
	doc
}

#' Sends a survey to the given recipient.
#'
#' @param username the Qualtrics username.
#' @param password the Qualtrics password.
#' @param parentEmailDistributionId see Qualtrics documentation.
#' @param sendDate the date to send the invitation.
#' @param fromEmail the email address of the sender.
#' @param fromName the name of the sender.
#' @param subject the subject of the email to send.
#' @param messageId the Qualtrics message id from the library.
#' @param libraryId the Qualtrics library id containing the message to send.
#' @export
sendReminder <- function(username, password, parentEmailDistributionId, sendDate,
				fromEmail, fromName, subject, messageId, libraryId) {
	url = paste("http://eu.qualtrics.com/Server/RestApi.php?Request=sendReminder",
				"&User=", username, 
				"&Password=", password, 
				"&ParentEmailDistributionID=", parentEmailDistributionId,
				"&SendDate=", sendDate, 
				"&FromEmail=", fromEmail, 
				"&FromName=", fromName, 
				"&Subject=", subject, 
				"&MessageID=", messageId, 
				"&LibraryID=", libraryId,
		sep="")
	doc = xmlRoot(xmlTreeParse(url))
	doc
}
