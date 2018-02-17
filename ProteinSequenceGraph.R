
# Load Libs ---------------------------------------------------------------

library(ggplot2)
library(plyr)
library(UniProt.ws)


# Functions ---------------------------------------------------------------

# sequence2Matrix <- function( sequence ){
#   
#   dim <- ceiling(sqrt(nchar(sequence)))
#   diff <- dim * dim - nchar(sequence)
#   sequence <- paste0(sequence, paste0(rep("-", diff), collapse = ""), collapse = "")
#   sequenceVector <- unlist(strsplit(sequence, split = ""))
#   sequenceMatrix <- matrix( sequenceVector, ncol = dim,  nrow = dim, byrow = TRUE)
#   
#   # dim <- ceiling(nchar(sequence)/30)
#   # diff <- dim * 30 - nchar(sequence)
#   # sequence <- paste0(sequence, paste0(rep("-", diff), collapse = ""), collapse = "")
#   # sequenceVector <- unlist(strsplit(sequence, split = ""))
#   # sequenceMatrix <- matrix( sequenceVector, ncol = 30,  nrow = dim, byrow = TRUE) 
#   return(sequenceMatrix)
# }

sequence2Dataframe <- function( sequence ){
  
  dim <- ceiling(sqrt(nchar(sequence)))
  diff <- dim * dim - nchar(sequence)
  sequence <- paste0(sequence, paste0(rep("-", diff), collapse = ""), collapse = "")
  sequenceVector <- unlist(strsplit(sequence, split = ""))
  x <- rep(1:dim, dim)
  y <- sort(rep(1:dim, dim)) 
  
  
  # dim <- ceiling(nchar(sequence)/30)
  # diff <- dim * 30 - nchar(sequence)
  # sequence <- paste0(sequence, paste0(rep("-", diff), collapse = ""), collapse = "")
  # sequenceVector <- unlist(strsplit(sequence, split = ""))
  # sequenceMatrix <- matrix( sequenceVector, ncol = 30,  nrow = dim, byrow = TRUE) 
  return( data.frame( row = y, col = x, AA = sequenceVector, position = 1:(dim*dim) ) )
}

fetchSequence <- function( accession = "P02671" ){
  
  up <- UniProt.ws(taxId=9606)
  
  #keytypes(up)
  
  sequence <- select(up, accession, "SEQUENCE", "UNIPROTKB" )[1,2]

  return(sequence)  
}

# fetchSequence <- function( accession = "P02671" ){
#   
#   up <- UniProt.ws(taxId=9606)
#   
#   #keytypes(up)
#   
#   test <- select(up, accession, "PHOSSITE", "UNIPROTKB" )
#   
#   return(sequence)  
# }


# Example -----------------------------------------------------------------

accession <- "P02671"

sequence <- fetchSequence(accession) 

#d <- sequence2Matrix(d)

sequenceDframe <- sequence2Dataframe(sequence)

p <- ggplot(sequenceDframe, aes(col, row, label = AA)) + geom_text()
p <- p + scale_y_continuous(trans = "reverse")
p <- p + ggtitle(accession)

psites <- c(37,88,3,678)
psiteDframe <- sequenceDframe[ sequenceDframe$position %in% psites  ,]

p <- p + geom_text(data =  psiteDframe,  aes(col, row, label = AA), inherit.aes = FALSE, colour = "red")

peptides <- data.frame(peptideSequence = c("Test","Test"), start = c(67,360), stop = c(80, 375))
peptides <- ddply(peptides, .(), mutate, positions = start:stop )
peptidePosition <- apply(peptides,2,1:2)
peptidesDframe <- sequenceDframe[ sequenceDframe$position %in% psites  ,]
 
p




