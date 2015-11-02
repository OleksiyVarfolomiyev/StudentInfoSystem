# Author: Alex (Oleksiy) Varfolomiyev
# Watch LIVE Presentation here http://rpubs.com/AlexVarfolomiyev/StudentInformationSystem

shinyUI(
 fluidPage(
  pageWithSidebar(
    
    headerPanel("MATH 111-105"),

    wellPanel(
      textInput('studentID', 'Enter Student ID (8-digit)')
     # submitButton('Login')
    ),
    
    
#    mainPanel(
  
    conditionalPanel("input.studentID >10000000",
      #wellPanel(
        sidebarPanel(
        h2('Grade Modeler'),
        h4('Use sliders to model the Projected Final Grade'),
        
                       sliderInput("sliderAssignmentsAve", label = h3("Projected Assignments Ave Grade"), 
                                   min = 0, max = 7, value = 4, step = 1),
                       
                       sliderInput("sliderExam3", label = h3("Projected Exam 3 Grade"), 
                                   min = 0, max = 100, value = 65),
                       
                       sliderInput("sliderFinal", label = h3("Projected Final Exam Grade"), 
                                   min = 0, max = 100, value = 65),
                       h2('Projected Final Grade'),
                       textOutput('projectedGrade'),
                       h2('Projected Final Letter Grade'),
                       textOutput('projectedLetterGrade'),
      #),
      
      #wellPanel(               
        h3(paste('Quiz 10')),
#     verbatimTextOutput("StudentName"),
        textOutput('recentQuizScore'),
 #    h4(paste('Added: Boxplot Scores, Top')),
        plotOutput('histRecentQuiz')
      #)
),
mainPanel(
      wellPanel(
        h4('Scores Summary'),
        tableOutput("tabScores"),
        plotOutput('plotScores'),
        plotOutput('boxplotScores')
      )
),
      wellPanel(
        h1('Scores Histograms'),
        plotOutput('histCurvedScore'),
        plotOutput('histExamAve'),
        plotOutput('histQuiz'),
        plotOutput('histHW'),
        plotOutput('histOnlineHW'),
        plotOutput('histMATLAB')
      )

    )# conditionalPanel

  )# pageWithSidebar
 )# fluidPage
)# shinyUI

