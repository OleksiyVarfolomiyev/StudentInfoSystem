# Author: Alex (Oleksiy) Varfolomiyev
# Watch LIVE Presentation here http://rpubs.com/AlexVarfolomiyev/StudentInformationSystem

library(shiny)
library(ggplot2)
library(reshape2)
library(plyr)
library(xlsx)

#dat <<- dat

## Download Data
if (!file.exists("data")) dir.create("data")

fileName = "./data/MATH_111-105_Roster.xlsx"
download.file(fileUrl, destfile = fileName, method = "curl")

dateDownloaded <- date()
dateDownloaded 

## Read Data
nCols <- 20
nStudents <- 38
dat = read.xlsx2(fileName, endRow = nStudents+2, colIndex = seq(1,nCols), sheetName = "MATH111-105", stringsAsFactors = F)

## Quiz 10
datQuizzes = read.xlsx2(fileName, endRow = nStudents+2, sheetName = "Quizzes", stringsAsFactors = F)
# Remove '%'
datQuizzes <- as.data.frame(sapply(datQuizzes, gsub, pattern="%", replacement=""), stringsAsFactors = F)

# Convert to integer
temp = datQuizzes$Name
datQuizzes <- as.data.frame(sapply(datQuizzes, as.integer))
datQuizzes$Name <- temp
datQuizzes[is.na(datQuizzes)] <- 0
##


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

# Init ggplot colors
  n = 3
  hues = seq(15, 375, length = n + 1)
  ggplotCol <- hcl(h = hues, l = 65, c = 100)[1:n]
  
############################################  

shinyServer(
  function(input, output) {
    
    # Recent Quiz Result
    output$recentQuizScore <- renderText({ 
      paste(datQuizzes[datQuizzes$ID == {input$studentID}, ]$Name, " Quiz 10 Score: ", 
            datQuizzes[datQuizzes$ID == {input$studentID}, ]$Q10, "%")
    })
    
    # Recent Quiz Histogram
    output$histRecentQuiz <- renderPlot({
          ggplot(datQuizzes[1:nStudents, ], aes(Q10)) + 
            geom_histogram(aes(fill=..count..), colour = "black", 
                           breaks = seq(min(datQuizzes$Q10), max(datQuizzes$Q10), by = 25)) + 
            geom_density(colour = "black") + 
            geom_vline(xintercept = datQuizzes[datQuizzes$ID == {input$studentID}, ]$Q10, 
                       colour = ggplotCol[2], show_guide = T) +
            geom_vline(xintercept = mean(datQuizzes$Q10), colour = ggplotCol[1]) +
            xlab("Quiz") + 
            ylab("# of students with the Score in the range")+
            ggtitle("Quiz 10 Histogram")+
            theme_bw()
        })
        
    # Scores Table
    output$tabScores <- renderTable({

      rank = which(dat$CurvedScore == dat[dat$ID == {input$studentID}, ]$CurvedScore)
      if(length(rank)>1) {
        rank = gsub(", ", "-", toString( c(rank[1], rank[length(rank)]) ))
        dat[dat$ID == {input$studentID}, ]$Rank <- rank
      }

      dat[dat$ID == 0 | dat$ID == {input$studentID}, 
          c("Name", "Score", "CurvedScore", "Rank", "Top", "ExamAve", "Assignments", "Exam1", "Exam2", "Quiz", "HW", "OnlineHW", "MATLAB"), drop = F]
          })
    
    # Scores Point Plot
    output$plotScores <- renderPlot({
        data <- dat[which(dat$ID == {input$studentID} | dat$ID == '0'), 
          c("Name", "Score", "CurvedScore", "ExamAve", "Assignments", "Exam1", "Exam2", "Quiz", "HW", "OnlineHW", "MATLAB"), drop = F]
        
        data = melt(data, value.name = "Scores", variable.name = "Names")
        
        ggplot(data, aes(x = Names, y = Scores, group = factor(Names), color = factor(Names), size = Scores)) +
        geom_point(aes( shape = factor(Name), lw = 3)) +
        scale_shape(solid = FALSE) +
        scale_size(range = c(5,10)) +
        geom_text(aes(label = Scores, color = factor(Names))) +
        #scale_fill_discrete(name = "Assignment") +
      #  scale_fill_brewer()+
        #xlab("Assignment")+
        ylab("Score")+
        ggtitle("Scores")+
        labs(colour = "Name") +
        theme_bw()
    })
    
    # Scores Box Plot
    output$boxplotScores <- renderPlot({
        data <- dat[1:nStudents, 
                    c("Score", "CurvedScore", "ExamAve", "Assignments", "Exam1", "Exam2", "Quiz", "HW", "OnlineHW", "MATLAB"), drop = F]
        data = stack(data)
        
        ggplot(data, aes(x = factor(ind), y = values)) +
          geom_boxplot( aes(fill = (ind)) ) +
          scale_fill_discrete(name = "Assignment") +
          #  scale_fill_brewer()+
          xlab("Variable") +
          ylab("Score") +
          ggtitle("Boxplot Scores") +
          labs(colour = "Name") +
          theme_bw()
      })
    
    # Grade Modeler
  #  sliderValues <- reactive({
  #    Value <- as.character( dat[dat$ID == input$studentID, "Score"] +
  #      input$sliderAssignmentsAve  + 0.2* input$sliderExam3  + 0.3* input$sliderFinal )
     # Value <- as.character(cut(as.numeric(Value), 
      #                 c(0, 60, 65, 72, 77, 83, 88, Inf), right = FALSE, labels = c("F", "D", "C", "C+", "B", "B+", "A")))
  #  })
    
    output$projectedGrade <- renderText({
      as.character( dat[dat$ID == {input$studentID}, "Score"] +
                                             {input$sliderAssignmentsAve}  + 0.2* {input$sliderExam3}  + 0.3* {input$sliderFinal} )
    })
      #renderText({
      #sliderValues()
    #})
    
    output$projectedLetterGrade <- renderText({
      as.character(cut(as.numeric(as.character( dat[dat$ID == {input$studentID}, "Score"] +
      {input$sliderAssignmentsAve}  + 0.2* {input$sliderExam3}  + 0.3* {input$sliderFinal} )), 
                  c(0, 60, 65, 72, 77, 83, 88, Inf), right = FALSE, labels = c("F", "D", "C", "C+", "B", "B+", "A")))
    })  
    ############
    
    # CurvedScore Histogram
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
     
    # Exam Average Histogram
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
    
    # Quiz Average Histogram
    output$histQuiz <- renderPlot({
      ggplot(dat[1:nStudents, ], aes(Quiz)) + 
        geom_histogram(aes(fill=..count..), colour = "black", breaks=seq(min(dat$Quiz), max(dat$Quiz), by = 9) ) + 
        scale_fill_gradient("Count", low = "coral3", high = "coral") +
        geom_vline(xintercept = dat[dat$ID == {input$studentID}, ]$Quiz, colour = ggplotCol[2]) +
        geom_vline(xintercept = mean(dat$Quiz), colour = ggplotCol[1]) +
        xlab("Quiz Score") + 
        ylab("# of students with the score in the range")+
        ggtitle("Ave Quiz Score Histogram")+
        theme_bw()
    })
    
    # OnlineHW Average Histogram
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
    
    # HW Average Histogram
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
    
    # MATLAB Assignment Average Histogram
    output$histMATLAB <- renderPlot({
      ggplot(dat[1:nStudents, ], aes(MATLAB)) + 
        geom_histogram(aes(fill=..count..), colour = "black", breaks=seq(min(dat$MATLAB), max(dat$MATLAB), by = 20) ) + 
        scale_fill_gradient("Count", low = "deeppink4", high = "deeppink1")+
        geom_vline(xintercept = as.integer(dat[dat$ID == {input$studentID}, ]$MATLAB), colour = ggplotCol[2]) +
        geom_vline(xintercept = mean(dat$MATLAB), colour = ggplotCol[1]) +
        xlab("MATLAB Assignment Score") + 
        ylab("# of students with the score in the range")+
        ggtitle("Ave MATLAB Assignment Score Histogram")+
        theme_bw()
    })
  }
)
