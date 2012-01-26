require(devtools)
require(roxygen2)

setwd("~/Dropbox/Projects")

document('qualtrics')
check_doc('qualtrics')
build('qualtrics')
install('qualtrics')
check('qualtrics')
library(qualtrics)

ls('package:qualtrics')
