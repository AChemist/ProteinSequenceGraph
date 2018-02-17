
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

p <- ggplot()
p <- p + ggtitle(accession)
p <- p + scale_y_continuous(trans = "reverse")

cleavageSites <- sequenceDframe[ sequenceDframe$position %in% gregexpr(pattern = "[RK][^P]", sequence )[[1]],]
p <- p + geom_tile(data =  cleavageSites,  aes(col, row), inherit.aes = FALSE, fill = "green")
p

p <- p + geom_text( data = sequenceDframe, aes(col, row, label = AA)) 
p


# psites <- c(37,88,3,678)
# psiteDframe <- sequenceDframe[ sequenceDframe$position %in% psites  ,]
# 
# p <- p + geom_text(data =  psiteDframe,  aes(col, row, label = AA), inherit.aes = FALSE, colour = "red")

peptides <- data.frame(peptideSequence = c("GLIDEVNQDFTNR","Test2"), start = c(72,360), stop = c(84, 375))
peptides <- ddply(peptides, .(peptideSequence), transform, positions = start:stop )
peptidesDframe <- sequenceDframe[ sequenceDframe$position %in% peptides$positions  ,]
 
p <- p + geom_text(data =  peptidesDframe,  aes(col, row, label = AA), inherit.aes = FALSE, colour = "green")
p <- p + geom_text(data =  cleavageSites,  aes(col, row, label = AA), inherit.aes = FALSE, colour = "black")
p


cleavageSites <- sequenceDframe[ sequenceDframe$position %in% gregexpr(pattern = "[RK][^P]", sequence )[[1]],]
p <- p + geom_text(data =  cleavageSites,  aes(col, row, label = AA), inherit.aes = FALSE, colour = "red")
p



regmatches(sequence,gregexpr(pattern = "[N][G]", sequence ))
regmatches(sequence,gregexpr(pattern = "[RK][EQ]", sequence ))



