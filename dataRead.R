dataRead <- function(nStudents=37, sheetName, fileUrl = "https://docs.google.com/spreadsheets/d/1tI5lHVKL5v8YvD4pEgmb-2rQ3ZJwzFuGk1hdeI4X2sA/pub?output=xlsx",
                     fileName = "./data/MATH111-105_Roster.xlsx") {
  
  library(plyr)
  library(xlsx)
  
  ## Download Data
  if (!file.exists("data")) dir.create("data")
  
  fileName = "./data/MATH_111-105_Roster.xlsx"
  fileUrl <- "https://docs.google.com/spreadsheets/d/1f9IFGVNPdTsvFF7H1gjI1aRtlzOY96URMg_STNONVFQ/pub?output=xlsx"
  download.file(fileUrl, destfile = fileName, method = "curl")
  
  dateDownloaded <- date()
  dateDownloaded 
  
  ## Read Data
  nCols <- 20
  
  if(sheetName == "MATH111-105"){
    
  dat = read.xlsx2(fileName, endRow = nStudents+2, colIndex = seq(1,nCols), sheetName = "MATH111-105", stringsAsFactors = F)
  
  ## Process data 
  # Rename columns
  dat <- rename(dat,c("C2" = "Exam2", "C1" = "Exam1", "C3" = "Exam3", "F" = "Final", "M" = "MATLAB"))
  
  # Remove '%'
  dat <- as.data.frame(sapply(dat, gsub, pattern="%", replacement=""), stringsAsFactors = F)
  
  # Add Rank column
  dat$Rank = which(dat$ID==dat$ID)
  dat[nrow(dat), ]$Rank = ""
  
  # Convert to integer
  temp = dat$Name
  dat <- as.data.frame(sapply(dat, as.integer))
  #dat$Name = as.character(dat$Name)
  dat$Name <- temp
  dat$Top <- paste(dat$Top, '%')
  dat[nrow(dat), ]$Top <- ''
  } # end of if sheetName == MATH111-105
  else 
    if (sheetName == "Quizzes") {
      dat = read.xlsx2(fileName, endRow = nStudents+2, sheetName = "Quizzes", stringsAsFactors = F)
      # Remove '%'
      dat <- as.data.frame(sapply(dat, gsub, pattern = "%", replacement = ""), stringsAsFactors = F)
  
      # Convert to integer
      temp = dat$Name
      dat <- as.data.frame(sapply(dat, as.integer))
      dat$Name <- temp
      dat[is.na(dat)] <- 0
    }
  
  dat
}