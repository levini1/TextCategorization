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

lWords[[10]]

charVals <- unlist(strsplit(lWords[[2]], split = ""))

txt <- "12345678901234567890"

charVals <- unlist(strsplit("12345678901234567890", split = ""))

# vectorLength TotalNumberOfCharacters - NumberOfCharacters + 1
nCharacters <- 6
endPoints <- nCharacters:(sum(nchar(charVals)))
startPoints <- endPoints - nCharacters + 1
data.frame(startPoints, endPoints)
sum(nchar(charVals)) - nCharacters + 1

numberOfCharactersToGroup <- nCharacters

getNGram <- function(txt, numberOfCharactersToGroup){
     endPoints <- numberOfCharactersToGroup:(nchar(txt))
     startPoints <- endPoints - numberOfCharactersToGroup + 1
     
     indexPositions <- mapply(seq, from = startPoints, to = endPoints)
     m <- matrix(charVals[indexPositions], ncol=numberOfCharactersToGroup, byrow = TRUE)
     return(apply(m, 1, paste, collapse = ""))
     
}








