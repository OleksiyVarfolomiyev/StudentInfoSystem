# Author: Alex (Oleksiy) Varfolomiyev
# Watch LIVE Presentation here http://rpubs.com/AlexVarfolomiyev/StudentInformationSystem

library(shiny)
library(ggplot2)
library(reshape2)
#library(plyr)
#library(xlsx)
library(gridExtra)

source('dataRead.R', local = T)
source('multiplot.R', local = T)

nStudents <- 37
nCols <- 20
# Read Data
#dat <<- dat
dat = dataRead(nStudents = nStudents, nCols = 20, sheetName = "MATH111-105")
datQuizzes = dataRead(nStudents = nStudents, nCols = 26, sheetName = "Quizzes")

# delete empty columnts with the default names names X.*
colsDesired <- !grepl( "X.", colnames(datQuizzes) )
#colDesired <- !grepl( "X.", colnames(datQuizzes) )
datQuizzes <- datQuizzes[, colsDesired]

# Init ggplot colors
  n = 3
  hues = seq(15, 375, length = n + 1)
  ggplotCol <- hcl(h = hues, l = 65, c = 100)[1:n]

############################################  

shinyServer(
  function(input, output) {
    
    output$plotQuizTrend <- renderPlot({
      data <- datQuizzes[which(datQuizzes$ID == {input$studentID} | datQuizzes$ID == '0'), 
                         2:ncol(datQuizzes)-2, drop = F]
      # Remove ID column
      data = data[,-1]
      
      data = melt(data, value.name = "Scores", variable.name = "Names")
      ggplot(data, aes(x = Names, y = Scores, group = Name,
                       color = factor(Name))) +
        geom_line(aes(  lw = 3)) +
        # scale_shape(solid = FALSE) +
        scale_size(range = c(5,10)) +
        geom_text(aes(label = Scores, color = factor(Name), size = Scores)) +
        #scale_fill_discrete(name = "Assignment") +
        #  scale_fill_brewer()+
        xlab("Quiz")+
        ylab("Score")+
        ggtitle("Quiz Trend")+
        labs(colour = "Name") +
        theme_bw()
    })
    
    # Recent Quiz Result
    output$recentQuizScore <- renderText({ 
      paste(datQuizzes[datQuizzes$ID == {input$studentID}, ]$Name, " Quiz 11 Score: ", 
            datQuizzes[datQuizzes$ID == {input$studentID}, ]$Q11, "%")
    })
    
    # Recent Quiz Histogram
    output$histRecentQuiz <- renderPlot({
          ggplot(datQuizzes[1:nStudents, ], aes(Q11)) + 
            geom_histogram(aes(fill=..count..), colour = "black", 
                           breaks = seq(min(datQuizzes$Q11), max(datQuizzes$Q11), by = 12.5)) + 
            geom_density(colour = "black") + 
            geom_vline(xintercept = datQuizzes[datQuizzes$ID == {input$studentID}, ]$Q11, 
                       colour = ggplotCol[2], show_guide = T) +
            geom_vline(xintercept = mean(datQuizzes$Q11), colour = ggplotCol[1]) +
            xlab("Quiz") + 
            ylab("# of students with the Score in the range")+
            ggtitle("Quiz 11 Histogram")+
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
          c("Name", "Score", "CurvedScore", "Rank", "Top", "ExamAve", "Assignments", 
            "Exam1", "Exam2", "Quiz", "HW", "OnlineHW", "MATLAB"), drop = F]
          })
    
    # Scores Point Plot
    output$plotScores <- renderPlot({
        data <- dat[which(dat$ID == {input$studentID} | dat$ID == '0'), 
          c("Name", "Score", "CurvedScore", "ExamAve", "Assignments", 
            "Exam1", "Exam2", "Quiz", "HW", "OnlineHW", "MATLAB"), drop = F]
        
        data = melt(data, value.name = "Scores", variable.name = "Names")
        
        ggplot(data, aes(x = Names, y = Scores, group = factor(Names), 
                         color = factor(Names), size = Scores)) +
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
                    c("Score", "CurvedScore", "ExamAve", "Assignments", 
                      "Exam1", "Exam2", "Quiz", "HW", "OnlineHW", "MATLAB"), drop = F]
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
    
    # CurvedScore Histogram    
    output$histCurvedScore <- renderPlot({
        ggplot(dat[1:nStudents, ], aes(CurvedScore)) + 
        geom_histogram(aes(fill=..count..), colour = "black", 
                       breaks = seq(min(dat$CurvedScore), max(dat$CurvedScore), by = 12)) + 
        scale_fill_gradient("Count", low = "palegreen4", high = "palegreen")+
        geom_density(colour = "black") + 
        geom_vline(xintercept = dat[dat$ID == {input$studentID}, ]$CurvedScore, colour = ggplotCol[2]) +
        geom_vline(xintercept = mean(dat$CurvedScore), colour = ggplotCol[1]) +
        xlab("CurvedScore") + 
        ylab("# of students with the CurvedScore in the range")+
        ggtitle("CurvedScore Histogram")+
        theme_bw()
    })
    
    # Grade Modeler
    output$projectedGrade <- renderText({
      as.character( dat[dat$ID == {input$studentID}, "Score"] +
                                  {input$sliderAssignmentsAve}  + 
                                  0.2* {input$sliderExam3}  + 
                                  0.3* {input$sliderFinal} )
    })

    output$projectedLetterGrade <- renderText({
      as.character(cut(as.numeric(as.character( dat[dat$ID == {input$studentID}, "Score"] +
      {input$sliderAssignmentsAve}  + 0.2* {input$sliderExam3}  + 0.3* {input$sliderFinal} )), 
      c(0, 60, 65, 72, 77, 83, 88, Inf), right = FALSE, 
      labels = c("F", "D", "C", "C+", "B", "B+", "A")))
    })  

    # Plot Scores Histograms
    output$histScores <- renderPlot({
    # Exam Average Histogram
    histExamAve <- 
      ggplot(dat[1:nStudents, ], aes(ExamAve)) + 
        geom_histogram(aes(fill=..count..), colour = "black", 
                       breaks = seq(min(dat$ExamAve), max(dat$ExamAve), by = 12) ) + 
        scale_fill_gradient("Count", low = "mediumorchid4", high = "mediumorchid")+
        geom_vline(xintercept = dat[dat$ID == {input$studentID}, ]$ExamAve, 
                   colour = ggplotCol[2]) +
        geom_vline(xintercept = mean(dat$ExamAve), colour = ggplotCol[1]) +
        xlab("ExamAve Score") + 
        ylab("# of students with the score in the range")+
        ggtitle("ExamAve Score Histogram")+
        theme_bw()
    
    histAssignmentsCurvedAve <- 
      ggplot(dat[1:nStudents, ], aes(CurvedAssignments)) + 
      geom_histogram(aes(fill=..count..), colour = "black", 
                     breaks = seq(min(dat$CurvedAssignments), max(dat$CurvedAssignments), by = 12) ) + 
      scale_fill_gradient("Count", low = "gold4", high = "gold")+
      geom_vline(xintercept = dat[dat$ID == {input$studentID}, ]$CurvedAssignments, 
                 colour = ggplotCol[2]) +
      geom_vline(xintercept = mean(dat$CurvedAssignments), colour = ggplotCol[1]) +
      xlab("Assignments Score") + 
      ylab("# of students with the score in the range")+
      ggtitle("Assignments Score Histogram")+
      theme_bw()
    
    multiplot( histExamAve, histAssignmentsCurvedAve, cols = 2)
    
    })
    
    # Plot Assignments Histograms    
    output$histAssignments <- renderPlot({
      histQuiz <- 
       ggplot(dat[1:nStudents, ], aes(Quiz)) + 
         geom_histogram(aes(fill=..count..), colour = "black", breaks = 
                          seq(min(dat$Quiz), max(dat$Quiz), by = 12) ) + 
         scale_fill_gradient("Count", low = "coral3", high = "coral") +
         geom_vline(xintercept = dat[dat$ID == {input$studentID}, ]$Quiz, 
                    colour = ggplotCol[2]) +
         geom_vline(xintercept = mean(dat$Quiz), colour = ggplotCol[1]) +
         xlab("Quiz Score") + 
         ylab("Frequency")+
         ggtitle("Ave Quiz Score Histogram")+
         theme_bw()
     
     histHW <- 
       ggplot(dat[1:nStudents, ], aes(HW)) + 
       geom_histogram(aes(fill=..count..), colour = "black", breaks = 
                        seq(min(dat$HW), max(dat$HW), by = 12) ) + 
       scale_fill_gradient("Count", low = "darkolivegreen4", high = "darkolivegreen1") +
       geom_vline(xintercept = as.integer(dat[dat$ID == {input$studentID}, ]$HW), 
                  colour = ggplotCol[2]) +
       geom_vline(xintercept = mean(dat$HW), colour = ggplotCol[1]) +
       xlab("HW Score") + 
       ylab("Frequency")+
       ggtitle("Ave HW Score Histogram")+
       theme_bw()
     
     histOnlineHW <-
       ggplot(dat[1:nStudents, ], aes(OnlineHW)) + 
         geom_histogram(aes(fill=..count..), colour = "black", breaks = 
                          seq(min(dat$OnlineHW), max(dat$OnlineHW), by = 11) ) + 
         scale_fill_gradient("Count", low = "deepskyblue4", high = "deepskyblue") +
         geom_vline(xintercept = as.integer(dat[dat$ID == {input$studentID}, ]$OnlineHW), 
                    colour = ggplotCol[2]) +
         geom_vline(xintercept = mean(dat$OnlineHW), colour = ggplotCol[1]) +
         xlab("OnlineHW Score") + 
         ylab("Frequency")+
         ggtitle("Ave OnlineHW Score Histogram")+
         theme_bw()
     
     # MATLAB Assignment Average Histogram
     histMATLAB <- 
       ggplot(dat[1:nStudents, ], aes(MATLAB)) + 
       geom_histogram(aes(fill=..count..), colour = "black", breaks = 
                        seq(min(dat$MATLAB), max(dat$MATLAB), by = 20) ) + 
       scale_fill_gradient("Count", low = "deeppink4", high = "deeppink1") +
       geom_vline(xintercept = as.integer(dat[dat$ID == {input$studentID}, ]$MATLAB), 
                  colour = ggplotCol[2]) +
       geom_vline(xintercept = mean(dat$MATLAB), colour = ggplotCol[1]) +
       xlab("MATLAB Assignment Score") + 
       ylab("Frequency")+
       ggtitle("Ave MATLAB Assignment Score Histogram")+
       theme_bw()
     
     multiplot(histQuiz, histHW, histOnlineHW, histMATLAB, cols = 2)
     
   })
    
   
  }
)
