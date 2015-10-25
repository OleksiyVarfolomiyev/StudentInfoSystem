# Author: Alex (Oleksiy) Varfolomiyev
# Watch LIVE Presentation here http://rpubs.com/AlexVarfolomiyev/StudentInformationSystem

library(shiny)
library(ggplot2)
library(reshape2)
library(plyr)
library(xlsx)

# setwd to the source file dir
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
#dat <<- dat

# Read data

  if (!file.exists("data")) dir.create("data")
  
  # Download Data
  
  fileUrl <- "https://docs.google.com/spreadsheets/d/1tI5lHVKL5v8YvD4pEgmb-2rQ3ZJwzFuGk1hdeI4X2sA/pub?output=xlsx"
  fileName = "./data/MATH111-105_Roster.xlsx"
  download.file(fileUrl, destfile = fileName, method = "curl")
  
  dateDownloaded <- date()
  dateDownloaded 
  
  #nCols <- 10
  #colIdx = 1:nCols
  nStudents <- 38
  #rowIdx = 1:nStudents
  
  # Read Data
  dat = read.xlsx2(fileName, sheetName="MATH111-105", stringsAsFactors = F)
  
  # delete empty columnts with names X.
  colDesired <- !grepl( "X.", colnames(dat) )
  dat <- dat[ 0:nStudents+1, colDesired]
  
  # Read Quiz Data
  #datQuizzes = read.xlsx2(fileName, colIndex = c(1,20), sheetName="Quizzes", stringsAsFactors = F)
  #datQuizzes[datQuizzes==""] <- NA
  
  # delete empty columnts with the default names names X.*
  #colDesired <- !grepl( "X.", colnames(datQuizzes) )
  # select nStudents+1 rows
  #colDesired <- !grepl( "X.", colnames(datQuizzes) )
  #datQuizzes <- datQuizzes[ 0:nStudents+1, colDesired]
  
  # Clean data 
  dat$Quiz <- gsub("%", "", dat$Quiz)
  dat$HW <- gsub("%", "", dat$HW)
  dat$OnlineHW <- gsub("%", "", dat$OnlineHW)
  dat$ExamAve <- gsub("%", "", dat$ExamAve)
  dat$CurvedScore <- gsub("%", "", dat$CurvedScore)
  dat$CurvedScore <- gsub("%", "", dat$CurvedScore)
  dat$ExamAve <- gsub("%", "", dat$ExamAve)
  dat$AssignmentsAve <- gsub("%", "", dat$AssignmentsAve)
  dat$M <- gsub("%", "", dat$M)
  dat$C2 <- gsub("%", "", dat$C2)
  dat$CurvedScore <- gsub("%", "", dat$CurvedScore)
  
  #datQuizzes$Q9 <- gsub("%", "", datQuizzes$Q9)
  #  dat <- as.data.frame(sapply(dat, function(x) gsub("%", "", x)))
  dat$CurvedScore = as.integer(dat$CurvedScore)
  dat$C2 = as.integer(dat$C2)
  dat$ExamAve = as.integer(dat$ExamAve)
  dat$CurvedScore <- as.integer(dat$CurvedScore)
  dat$Quiz = as.integer(dat$Quiz)
  dat$HW = as.integer(dat$HW)
  dat$OnlineHW = as.integer(dat$OnlineHW)
  dat$ExamAve = as.integer(dat$ExamAve)
  dat$M = as.integer(dat$M)
  dat$AssignmentsAve = as.integer(dat$AssignmentsAve)
  #datQuizzes= na.omit(datQuizzes)
  #datQuizzes$Q9 = as.integer(datQuizzes$Q9)
  
  dat$Rank = which(dat$ID==dat$ID)
  dat[nrow(dat), ]$Rank = ""

  # ggplot colors
  n=3
  hues = seq(15, 375, length=n+1)
  ggplotCol <- hcl(h=hues, l=65, c=100)[1:n]
############################################  
shinyServer(
  function(input, output) {
    
#     output$histRecentQuiz <- renderPlot({
#       ggplot(datQuizzes[1:nStudents, ], aes(Q9)) + 
#         geom_histogram(aes(fill=..count..), colour = "black", 
#                        breaks = seq(min(datQuizzes$Q9), max(datQuizzes$Q9), by = 11)) + 
#         geom_density(colour = "black") + 
#         geom_vline(xintercept = datQuizzes[datQuizzes$ID == {input$studentID}, ]$Q9, colour = ggplotCol[2]) +
#         geom_vline(xintercept = mean(datQuizzes$Q9), colour = ggplotCol[1]) +
#         xlab("Quiz") + 
#         ylab("# of students with the Score in the range")+
#         ggtitle("Quiz 9 Histogram")+
#         theme_bw()
#     })
    
    output$histRecentExam <- renderPlot({
      ggplot(dat[1:nStudents, ], aes(C2)) + 
        geom_histogram(aes(fill=..count..), colour = "black", 
                       breaks = seq(min(dat$C2), max(dat$C2), by = 11)) + 
        geom_density(colour = "black") + 
        geom_vline(xintercept = dat[dat$ID == {input$studentID}, ]$C2, colour = ggplotCol[2]) +
        geom_vline(xintercept = mean(dat$C2), colour = ggplotCol[1]) +
        xlab("Common Exam 2") + 
        ylab("# of students with the Score in the range")+
        ggtitle("Common Exam 2 Histogram")+
        theme_bw()
    })
    output$tabStudent <- renderTable({

      rank = which(dat$CurvedScore == dat[dat$ID == {input$studentID}, ]$CurvedScore)
      if(length(rank)>1) {
        rank = gsub(", ", "-", toString( c(rank[1], rank[length(rank)]) ))
        dat[dat$ID == {input$studentID}, ]$Rank <- rank
      }
      dat[dat$ID == {input$studentID} | dat$ID == '0', c("Name", "Rank", "Top", "CurvedScore", 
                                                         "ExamAve", "AssignmentsAve", "C2", "Quiz", "HW", "OnlineHW", "M")]
      
       })
    
    # check if it's a valid ID
#    if(dat$ID == {input$studentID}){
      output$plotScores <- renderPlot({
        data <- dat[dat$ID == {input$studentID} | dat$ID == '0', c("Name", "CurvedScore", 
                                                                   "ExamAve", "AssignmentsAve", "C2", "Quiz", "HW", "OnlineHW", "M")]
        data = melt(data)
        
        ggplot(data, aes(x=variable, y = value, group = factor(variable))) +
        geom_point(aes(color=factor(Name)), size = 5)+
        scale_fill_discrete(name ="Assignment")+
      #  scale_fill_brewer()+
        xlab("Assignment")+
        ylab("Score")+
        ggtitle("Scores")+
        labs(colour = "Name") +
        theme_bw()
    })
    
    output$histCurvedScore <- renderPlot({
      ggplot(dat[1:nStudents, ], aes(CurvedScore)) + 
        geom_histogram(aes(fill=..count..), colour = "black", breaks = seq(min(dat$CurvedScore), max(dat$CurvedScore), by = 7)) + 
        geom_density(colour = "black") + 
        geom_vline(xintercept = dat[dat$ID == {input$studentID}, ]$CurvedScore, colour = ggplotCol[2]) +
        geom_vline(xintercept = mean(dat$CurvedScore), colour = ggplotCol[1]) +
        xlab("CurvedScore") + 
        ylab("# of students with the CurvedScore in the range")+
        ggtitle("CurvedScore Histogram")+
        theme_bw()
    })

    output$histExamAve <- renderPlot({
      ggplot(dat[1:nStudents, ], aes(ExamAve)) + 
        geom_histogram(aes(fill=..count..), colour = "black", breaks = seq(min(dat$ExamAve), max(dat$ExamAve), by = 9) ) + 
        scale_fill_gradient("Count", low = "gold4", high = "gold")+
        geom_vline(xintercept = dat[dat$ID == {input$studentID}, ]$ExamAve, colour = ggplotCol[2]) +
        geom_vline(xintercept = mean(dat$ExamAve), colour = ggplotCol[1]) +
        xlab("ExamAve Score") + 
        ylab("# of students with the score in the range")+
        ggtitle("ExamAve Score Histogram")+
        theme_bw()
    })   
    
    output$histQuiz <- renderPlot({
      ggplot(dat[1:nStudents, ], aes(Quiz)) + 
        geom_histogram(aes(fill=..count..), colour = "black", breaks=seq(min(dat$Quiz), max(dat$Quiz), by = 9) ) + 
        scale_fill_gradient("Count", low = "coral3", high = "coral")+
        geom_vline(xintercept = dat[dat$ID == {input$studentID}, ]$Quiz, colour = ggplotCol[2]) +
        geom_vline(xintercept = mean(dat$Quiz), colour = ggplotCol[1]) +
        xlab("Quiz Score") + 
        ylab("# of students with the score in the range")+
        ggtitle("Ave Quiz Score Histogram")+
        theme_bw()
    })
    
    output$histOnlineHW <- renderPlot({
      ggplot(dat[1:nStudents, ], aes(OnlineHW)) + 
        geom_histogram(aes(fill=..count..), colour = "black", breaks=seq(min(dat$OnlineHW), max(dat$OnlineHW), by = 9) ) + 
        scale_fill_gradient("Count", low = "deepskyblue4", high = "deepskyblue")+
        geom_vline(xintercept = as.integer(dat[dat$ID == {input$studentID}, ]$OnlineHW), colour = ggplotCol[2]) +
        geom_vline(xintercept = mean(dat$OnlineHW), colour = ggplotCol[1]) +
        xlab("OnlineHW Score") + 
        ylab("# of students with the score in the range")+
        ggtitle("Ave OnlineHW Score Histogram")+
        theme_bw()
    })
    
    output$histHW <- renderPlot({
      ggplot(dat[1:nStudents, ], aes(HW)) + 
        geom_histogram(aes(fill=..count..), colour = "black", breaks=seq(min(dat$HW), max(dat$HW), by = 11) ) + 
        scale_fill_gradient("Count", low = "darkolivegreen4", high = "darkolivegreen1")+
        geom_vline(xintercept = as.integer(dat[dat$ID == {input$studentID}, ]$HW), colour = ggplotCol[2]) +
        geom_vline(xintercept = mean(dat$HW), colour = ggplotCol[1]) +
        xlab("HW Score") + 
        ylab("# of students with the score in the range")+
        ggtitle("Ave HW Score Histogram")+
        theme_bw()
    })
    
    output$histM <- renderPlot({
      ggplot(dat[1:nStudents, ], aes(M)) + 
        geom_histogram(aes(fill=..count..), colour = "black", breaks=seq(min(dat$M), max(dat$M), by = 20) ) + 
        scale_fill_gradient("Count", low = "deeppink4", high = "deeppink1")+
        geom_vline(xintercept = as.integer(dat[dat$ID == {input$studentID}, ]$M), colour = ggplotCol[2]) +
        geom_vline(xintercept = mean(dat$M), colour = ggplotCol[1]) +
        xlab("MATLAB Assignment Score") + 
        ylab("# of students with the score in the range")+
        ggtitle("Ave MATLAB Assignment Score Histogram")+
        theme_bw()
    })
#    }# if valid ID
  }
)
