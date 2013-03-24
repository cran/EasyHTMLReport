\name{easyHtmlReport}
\alias{easyHtmlReport}
\title{
Easy to send HTML reports.
}
\description{
  It is a package of easy to send HTML reports. \\
  If sendmailR(<= 1.1-2) please install a modified version of
  me. https://github.com/yokkuns/sendmailR. \\
  
  $ git clone git@github.com:yokkuns/sendmailR.git \\
  $ cd sendmailR \\
  $ sudo make install \\

  or

  $ make package \\
  $ R \\
  > install.packages("sendmailR_1.1-2.tar.gz") \\
}
\usage{
easyHtmlReport(rmd.file, from, to, subject, headers = list(), control = list(), markdown.options = c("hard_wrap", "use_xhtml", "smartypants"), stylesheet = "")
}

\arguments{
  \item{rmd.file}{
    R Markdown file.
}
  \item{from}{
    mail from.
}
  \item{to}{
    mail to.
}
  \item{subject}{
    mail subject.
}
  \item{headers}{
    mail headers.
}
  \item{control}{
    mail control.
}
  \item{markdown.options}{
    markdownToHTML options.
}
  \item{stylesheet}{
    stylesheet file.
}
}

\value{
None
}

\author{
  Yohei Sato
}

\seealso{
  sendmailR::sendmail,markdown::markdownToHTML
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


