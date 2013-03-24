easyHtmlReport <-
function(rmd.file,from,to,subject,headers=list(),control=list(),
                           markdown.options=c("hard_wrap","use_xhtml","smartypants"),stylesheet=""){
  f <- rmd.file
  md.file <- paste(f,"md",sep=".")
  mail.html.file <- paste(f,".html",sep="") ## メール用

  knit(input=rmd.file,output=md.file)
  markdownToHTML(file=md.file,output=mail.html.file,
                 stylesheet=stylesheet,
                 options=markdown.options)

  html.str <- paste(readLines(mail.html.file),collapse="\n")
  html.str <- gsub("figure/","cid:",html.str)
  
  imgs <- sapply(list.files("figure"),
                 function(f){
                   mime_part(paste("figure",f,sep="/"),f)
                 })

  body <- unlist(list(list(html.str),imgs))
  headers <- list("Content-Type"="text/html; charset=\"utf-8\"")
  sendmail(from,to,subject,body,headers=headers,control=control)
}
