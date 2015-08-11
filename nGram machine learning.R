getWords <- function(txtString){
     
     
     #description <- gsub(txtString , pattern="(\")|(\\?)|(\\:)|(;)|(\\()|(\\))", replacement=" ")
     #description <- gsub(description , pattern="(\\.)|(,)|(/)|(-)", replacement=" ")
     #description <- gsub(description , pattern="[0-9]", replacement=" ")
     
     u <- unlist(strsplit(txtString, split=""))
     r <- which(u %in% c(" ", letters[1:26], LETTERS[1:26]))
     #r <- which(u %in% c(" ", letters[1:26], LETTERS[1:26],".", ":", ",", "-", "/", 0:9))
     description <- paste(u[r], collapse="")
     
     description <- gsub(description , pattern="\\s+", replacement=" ")
     description <- gsub(description , pattern="^\\s+|\\s+$", replacement=" ")
     
     description <- tolower(description)
     
     words <- unlist(strsplit(description, split=" "))
     wordLength <- nchar(words)
     r <- which(wordLength >= 1)
     #words <- words[r]
     words <- paste(words[r], collapse = " ")
     return(words)
     
}



d <- read.csv("/Volumes/NeuroQuality/ClosedCrtsRecords.csv", head=TRUE, as.is=TRUE)
names(d) <- c("pe", "comments", "code", "code1desc", "code2desc", "therapy", "asReported")

r <- grep(pattern = "SWELLING|BURNING SENSATION", d$code1desc)

d1 <- d[r,]
r <- which(!duplicated(d1, MARGIN = 1))
d1 <- d1[r,]


lWords <- lapply(d1$comments, getWords)





