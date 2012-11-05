#' Returns the survey panels in the given library.
#'
#' @param username the Qualtrics username.
#' @param password the Qualtrics password.
#' @param libraryid the Qualtrics library id.
#' @export
getPanels <- function(username, password, libraryid) {
	url = paste("http://eu.qualtrics.com/Server/RestApi.php?Request=getPanels",
				"&User=", username, 
				"&Password=", password, 
				"&LibraryID=", libraryid, 
				sep="")
	doc = xmlRoot(xmlTreeParse(url))
	parseXMLResponse(doc[[1]])
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
