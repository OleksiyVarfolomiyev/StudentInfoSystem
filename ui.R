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
        wellPanel(
          textInput('studentID', 'Enter Student ID (8-digit)')
        ),
        
        wellPanel(
          h3(paste('Quiz 11')),
          textOutput('recentQuizScore'),
          plotOutput('histRecentQuiz')
        ),
        
        wellPanel(
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
        ),
        wellPanel(
          h2('CurvedScore'),
          plotOutput('histCurvedScore')
        )
        
    ), # sidebarPanel
##########
    mainPanel(
      wellPanel(
        h2('Scores Summary'),
        tableOutput("tabScores"),
        plotOutput('plotScores'),
        plotOutput('boxplotScores')
      ),
      
      wellPanel(
       h2('Scores Histograms'),
       plotOutput('histScores') 
      ),
      
      wellPanel(
       h2('Assignments Histograms'),
       plotOutput('histAssignments')
      ),
      
      wellPanel(
        h2('Trends'),
        plotOutput('plotQuizTrend')
      )
    ) # mainPanel
#  )# conditionalPanel

  )# pageWithSidebar
 )# fluidPage
)# shinyUI

