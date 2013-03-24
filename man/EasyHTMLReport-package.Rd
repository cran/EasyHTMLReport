\name{EasyHTMLReport-package}
\alias{EasyHTMLReport-package}
\alias{EasyHTMLReport}
\docType{package}
\title{
Easy to send HTML reports.
}
\description{
  It is a package of easy to send HTML reports. \\
  If sendmailR(<= 1.1-2) please install a modified version of me. https://github.com/yokkuns/sendmailR.
  
  $ git clone git@github.com:yokkuns/sendmailR.git \\
  $ cd sendmailR \\
  $ sudo make install \\

  or

  $ make package \\
  $ R \\
  > install.packages("sendmailR_1.1-2.tar.gz") \\
}
\details{
\tabular{ll}{
Package: \tab EasyHTMLReport\cr
Type: \tab Package\cr
Version: \tab 0.0.2\cr
Date: \tab 2013-03-24\cr
License: \tab BSD\cr
}
}
\author{
Maintainer: Yohei Sato <yokkuns@tkuljp>
}

\keyword{ package }
\seealso{
  \code{\link{easyHtmlReport}}
}

\examples{
\dontrun{
rmd.file <- "sample.Rmd"
mail.from <- "yokkuns@tkul.jp"
mail.to <- "yokkuns@tkul.jp"
mail.subject <- "Easy HTML Report"
easyHtmlReport(rmd.file,mail.from,mail.to,mail.subject)
}
}
