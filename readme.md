# Qualtrics R Package

The `qualtrics` R package provides functions to interact with the [Qualtrics](http://www.qualtrics.com) online survey tool. It requires that your account have API access. The latest development version can be installed from Github with the `devtools` package.

	> require(devtools)
	> install_github('qualtrics', 'jbryer')
	
	> ls('package:qualtrics')	 [1] "addRecipient"           "createPanel"            "getPanel"              	 [4] "getPanelMemberCount"    "getPanels"              "getRecipient"          	 [7] "getSurvey"              "getSurveyName"          "getSurveyResults"      	[10] "getSurveys"             "sendReminder"           "sendSurveyToIndividual"	[13] "varEntryDialog" 

Login credentials are required. The `varEntryDialog` provides a user dialog for entering a username and password. Note that although password entry is masked in the dialog, the password is stored in plain text within R.

	> login <- varEntryDialog(vars=c('Username', 'Password'), show=c(NA,'*'))

You can retrieve the list of survey in your account with the `getSurveys` function (note output has been truncated).

	> getSurveys(login$Username, login$Password)    	responses SurveyType               SurveyID                            SurveyName               1         117  SS SV_39RKJNe1htAdR4g  Faculty Survey of Student 	Readiness               2         400  SS SV_3rQruV2eBPG7uFS      Staff Satisfaction Survey - 2011