# Author: Alex (Oleksiy) Varfolomiyev
# Watch LIVE Presentation here http://rpubs.com/AlexVarfolomiyev/StudentInformationSystem

shinyUI(
 fluidPage(
  pageWithSidebar(
    
    headerPanel("Student Information System MATH 111-105"),

#    wellPanel(
#      textInput('studentID', 'Enter Student ID (8-digit)')
     # submitButton('Login')
#    ),
    
#conditionalPanel("input.studentID >10000000| input.studentID == 0",
     
      sidebarPanel(
        textInput('studentID', 'Enter Student ID (8-digit)'),
        h3(paste('Quiz 10')),
        textOutput('recentQuizScore'),
        plotOutput('histRecentQuiz'),
        
        h2('Grade Modeler'),
        h4('Use sliders to model Projected Final Grade'),
        
        sliderInput("sliderAssignmentsAve", label = h3("Projected Assignments Ave Grade"), 
                    min = 0, max = 7, value = 4, step = 1),
                       
        sliderInput("sliderExam3", label = h3("Projected Exam 3 Grade"), 
                    min = 0, max = 100, value = 65),
                       
        sliderInput("sliderFinal", label = h3("Projected Final Exam Grade"), 
                    min = 0, max = 100, value = 65),
        
        h2('Projected Final Grade'),
        textOutput('projectedGrade'),
                       
        h2('Projected Final Letter Grade'),
        textOutput('projectedLetterGrade')
    ), # sidebarPanel
##########
    mainPanel(
      wellPanel(
        h2('Scores Summary'),
        tableOutput("tabScores"),
        plotOutput('plotScores'),
        plotOutput('boxplotScores'),
       
       h2('Scores Histograms'),
       plotOutput('histScores'), 
       
       h2('Assignments Histograms'),
       plotOutput('histAssignments')
      )
    ) # mainPanel
#  )# conditionalPanel

  )# pageWithSidebar
 )# fluidPage
)# shinyUI

