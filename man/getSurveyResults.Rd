\name{getSurveyResults}
\alias{getSurveyResults}
\title{Returns the results of a given survey.}
\usage{
  getSurveyResults(username, password, surveyid,
    truncNames = 20, startDate = NULL, endDate = NULL)
}
\arguments{
  \item{username}{the Qualtrics username.}

  \item{password}{the Qualtrics password.}

  \item{surveyid}{the Qualtrics survey id.}

  \item{truncNames}{the maximum length of column names
  returned from qualtrics.}

  \item{startDate}{beginning date range for results
  returned.}

  \item{endDate}{ending date range for results returned.}
}
\description{
  Returns the results of a given survey.
}

