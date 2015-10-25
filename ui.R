# Author: Alex (Oleksiy) Varfolomiyev
# Watch LIVE Presentation here http://rpubs.com/AlexVarfolomiyev/StudentInformationSystem

shinyUI(
  pageWithSidebar(
    
    headerPanel("MATH 111-105"),
  
    wellPanel(
      textInput('studentID', 'Student ID'),
      submitButton('Login')
    ),
  
#    mainPanel(
  
#    h4('Score'),
#    verbatimTextOutput("Score"),
    conditionalPanel("input.studentID != 0",
      plotOutput('histRecentExam'),
      tableOutput("tabStudent"),
      plotOutput('plotScores'),
      plotOutput('histCurvedScore'),
      plotOutput('histExamAve'),
      plotOutput('histQuiz'),
      plotOutput('histHW'),
      plotOutput('histOnlineHW'),
      plotOutput('histM')
      
    )
  )
)

