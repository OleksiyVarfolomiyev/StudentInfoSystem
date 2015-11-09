dataRead <- function(fileUrl = "https://docs.google.com/spreadsheets/d/1f9IFGVNPdTsvFF7H1gjI1aRtlzOY96URMg_STNONVFQ/pub?output=xlsx",
                     fileName = "./data/MATH111-105_Roster.xlsx", 
                     sheetName, nStudents, nCols) {
  
  library(plyr)
  library(xlsx)
  
  ## Download Data
  if (!file.exists("data")) dir.create("data")
  
  fileName = "./data/MATH_111-105_Roster.xlsx"
  download.file(fileUrl, destfile = fileName, method = "curl")
  
  dateDownloaded <- date()
  dateDownloaded 
  
  ## Read Data
  dat = read.xlsx2(fileName, endRow = nStudents+2, colIndex = seq(1,nCols), sheetName = sheetName, stringsAsFactors = F)
  
  # Remove '%'
  dat <- as.data.frame(sapply(dat, gsub, pattern = "%", replacement=""), stringsAsFactors = F)
  
  # Convert to integer, except Name column
  temp = dat$Name
  dat <- as.data.frame(sapply(dat, as.integer))
  dat$Name <- temp
  dat[is.na(dat)] <- 0
  
  if(sheetName == "MATH111-105"){

    # Rename columns
    dat <- rename(dat,c("C2" = "Exam2", "C1" = "Exam1", "C3" = "Exam3", "F" = "Final", "M" = "MATLAB"))
  
    # Add Rank column
    dat$Rank = which(dat$ID==dat$ID)
    dat[nrow(dat), ]$Rank = ""
    
    # Add column 'Top', Every student is in top N%
    dat$Top <- paste(dat$Top, '%')
    dat[nrow(dat), ]$Top <- ''
  } # end of if sheetName == MATH111-105
   else 
     if (sheetName == "Quizzes") {
      # delete empty columnts with the default names names X.*
      colsDesired <- !grepl( "X", colnames(dat) )
      #colDesired <- !grepl( "X", colnames(datQuizzes) )
      dat <- dat[, colsDesired]
     }
  
  dat
}