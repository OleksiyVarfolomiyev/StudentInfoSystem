# Author: Alex (Oleksiy) Varfolomiyev
# Watch LIVE Presentation here http://rpubs.com/AlexVarfolomiyev/StudentInformationSystem

shinyUI(fluidPage(
  pageWithSidebar(
    
    headerPanel("MATH 111-105"),

    wellPanel(
      textInput('studentID', 'Student ID (8-digit)'),
      submitButton('Login')
    ),
  
#    mainPanel(
  
#    verbatimTextOutput("Score"),
    conditionalPanel("input.studentID != 0",

    h4(paste('Updated: Quiz 10')),
 #   verbatimTextOutput("StudentName"),
    textOutput('recentQuizScore'),
    #h4(paste('Added: Boxplot Scores, Top')),

      plotOutput('histRecentQuiz'),
    
      tableOutput("tabScores"),
      plotOutput('plotScores'),
      plotOutput('boxplotScores'),
    
      plotOutput('histCurvedScore'),
      plotOutput('histExamAve'),
      plotOutput('histQuiz'),
      plotOutput('histHW'),
      plotOutput('histOnlineHW'),
      plotOutput('histMATLAB')
      
    )
  )
))

