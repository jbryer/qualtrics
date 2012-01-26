######### Panel Requests #########
getPanels <- function(username, password, libraryid) {
	url = paste("http://new.qualtrics.com/Server/RestApi.php?Request=getPanels&User=",
		username, "&Password=", password, "&LibraryID=", libraryid, sep="")
	doc = xmlRoot(xmlTreeParse(url))
	parseXMLResponse(doc[[1]])
}

getPanel <- function(username, password, libraryid, panelid, embeddedData=NULL) {
	#TODO: If we use the XML format, embedded data would be included. Parameter is a comma separated list.
	url = paste("http://new.qualtrics.com/Server/RestApi.php?Request=getPanel&User=",
		username, "&Password=", password, "&LibraryID=", libraryid, "&PanelID=", panelid, "&Format=CSV", 
		ifelse(is.null(embeddedData), "", paste("&EmbeddedData=", embeddedData, sep="")), 
		sep="")
	t = read.csv(url)
	t$X = NULL
	t
}

getPanelXML <- function(username, password, libraryid, panelid) {
	url = paste("http://new.qualtrics.com/Server/RestApi.php?Request=getPanel&User=",
		username, "&Password=", password, "&LibraryID=", libraryid, "&PanelID=", panelid, "&Format=XML&RecipientHistory=1", 
		sep="")
	doc = xmlRoot(xmlTreeParse(url))
	doc
}

getPanelMemberCount <- function(username, password, libraryid, panelid) {
	url = paste("http://new.qualtrics.com/Server/RestApi.php?Request=getPanelMemberCount&User=",
		username, "&Password=", password, "&LibraryID=", libraryid, "&PanelID=", panelid, 
		sep="")
	doc = xmlRoot(xmlTreeParse(url))
	as.numeric(xmlValue(doc[[1]]))
}

createPanel <- function(username, password, libraryId, name, category=NULL) {
	url = paste("http://new.qualtrics.com/Server/RestApi.php?Request=createPanel&User=",
		username, "&Password=", password, "&LibraryID=", libraryId, "&Name=", name,
		ifelse(is.null(category), "", paste("&Category=", category, sep="")),
		sep="")
	doc = xmlRoot(xmlTreeParse(url))
	as.character(xmlValue(doc[[1]]))
}

addRecipient <- function(username, password, libraryId, panelId, firstName=NULL, lastName=NULL, email=NULL, externalDataRef=NULL, embeddedData=NULL) {
	url = paste("http://new.qualtrics.com/Server/RestApi.php?Request=addRecipient&User=",
		username, "&Password=", password, "&LibraryID=", libraryId, "&PanelID=", panelId,
		ifelse(is.null(firstName), "", paste("&FirstName=", firstName, sep="")),
		ifelse(is.null(lastName), "", paste("&LastName=", lastName, sep="")),
		ifelse(is.null(email), "", paste("&Email=", email, sep="")),
		ifelse(is.null(externalDataRef), "", paste("&ExternalDataRef", externalDataRef, sep="")),
		sep="")
	if(!is.null(embeddedData)) {
		for(i in names(embeddedData)) {
			url = paste(url, "&ED[", i, "]=", as.character(embeddedData[1,i]), sep="")
		}
	}
	doc = xmlRoot(xmlTreeParse(url))
	as.character(xmlValue(doc[[1]]))
}

getRecipient <- function(username, password, recipientId, libraryId) {
	url = paste("http://new.qualtrics.com/Server/RestApi.php?Request=getRecipient&User=",
		username, "&Password=", password, "&RecipientID=", recipientId, "&LibraryID=", libraryId, 
		sep="")
	doc = xmlRoot(xmlTreeParse(url))
	doc
}

sendSurveyToPanel <- function(username, password, surveyId, sendDate, fromEmail, fromName, subject, messageId, messageLibraryId, panelId, panelLibraryId, expirationDate=NULL, linkType=NULL) {
#todo:
}

sendSurveyToIndividual <- function(username, password, surveyId, sendDate, fromEmail, fromName, subject, messageId, messageLibraryId, panelId, panelLibraryId, recipientId) {
	url = paste("http://new.qualtrics.com/Server/RestApi.php?Request=sendSurveyToIndividual&User=",
		username, "&Password=", password, "&SurveyID=", surveyId, "&SendDate=", sendDate, 
		"&FromEmail=", fromEmail, "&FromName=", fromName, "&Subject=", subject, 
		"&MessageID=", messageId, "&MessageLibraryID=", messageLibraryId, "&PanelID=", panelId, "&PanelLibraryID=", panelLibraryId,
		"&RecipientID=", recipientId,
		sep="")
	doc = xmlRoot(xmlTreeParse(url))
	doc
}

sendReminder <- function(username, password, parentEmailDistributionId, sendDate, fromEmail, fromName, subject, messageId, libraryId) {
	url = paste("http://new.qualtrics.com/Server/RestApi.php?Request=sendReminder&User=",
		username, "&Password=", password, 
		"&ParentEmailDistributionID=", parentEmailDistributionId,
		"&SendDate=", sendDate, 
		"&FromEmail=", fromEmail, "&FromName=", fromName, "&Subject=", subject, 
		"&MessageID=", messageId, "&LibraryID=", libraryId,
		sep="")
	doc = xmlRoot(xmlTreeParse(url))
	doc
}