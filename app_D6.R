library(shiny)
library(shinydashboard)
library(shinyWidgets)
#The user interface
header <- dashboardHeader(title = "Cayley Graph for Group D6 (Hexagon)",
                          titleWidth = 600
)
sidebar <- dashboardSidebar(disable = TRUE)



#Functions that implement the mathematics
source("D6_cayley.R")


# dashboard functions

body <- dashboardBody(
  column(
    width = 3,
    h3("Multiplication", style=kahua.s),
    h4("Defining relations:", style=kahua),
    div(HTML(paste(Leerraum(4),CD6.manifestWord("rrrrrr")," = I<br>",Leerraum(7),CD6.manifestWord("ff")," = I<br>",Leerraum(3),CD6.manifestWord("frfr")," = I",sep="")),style=kahua.s),
    h4("Rewrite rules:", style=kahua),                 
    p(HTML(paste(Leerraum(7),CD6.manifestWord("ff")," &#8658; I<br>",
                 Leerraum(6),CD6.manifestWord("rf")," &#8658; ",CD6.manifestWord("frrrrr"),"<br>",
                 Leerraum(3),CD6.manifestWord("rrf")," &#8658; ",CD6.manifestWord("frrrr"),"<br>",
                 Leerraum(4),CD6.manifestWord("rrrf")," &#8658; ",CD6.manifestWord("frrr"),"<br>",
                 CD6.manifestWord("rrrrf")," &#8658; ",CD6.manifestWord("frr"),"<br>",
                 Leerraum(4),CD6.manifestWord("rrrrr")," &#8658; I",
                 sep="")),style=kahua.s),
    actionBttn("btnright","Select right operand (red)",style="jelly",color="danger"),
    uiOutput("right"),
    actionBttn("btnleft","Select left operand (green)",style="jelly",color="success"),
    uiOutput("left"),
    actionBttn("btncalc","Calculate the product (purple)",style="jelly",color="royal"),
    uiOutput("prod"),
    uiOutput("message")
  ),
  column(
    width = 6,
    h2("The Cayley Graph", align="center", style = kahua.s),
    div(HTML(paste("Labeling rule: at most one ",CD6.manifestWord("f"),"<br>",
                   "Labeling rule: no ", CD6.manifestWord("f")," appear after ",CD6.manifestWord("r"),"<br>",
                   # "Labeling rule: three ",CD6.manifestWord("f")," appear only as ",CD6.manifestWord("frrfrrrf"),
                   sep="")),style=kahua),
    plotOutput("cayley",height = 768, click = "plot_click"),
    #        uiOutput("message")
    
  ),
  column(
    width = 3,
    h3("Permutations", align="center", style = kahua.s),
    selectInput("chooser",helpText("Choose",span("r", style = "color:royalblue; font-weight: bold"),span(": Order 5",style="font-size:initial"),style=kahua),c("(123456)", '165432')),
    selectInput("choosef",helpText("Choose",span("f", style = "color:darkorange; font-weight: bold"),span(": Order 2",style="font-size:initial"),style=kahua),c("(12)(36)(45)", "(26)(35)", '(13)(46)', '(15)(24)', '(16)(25)(34)', '(14)(23)(56)')),
    actionBttn("makeperm","Make Permutations",style="jelly")
  )
  
)


ui <- dashboardPage(header, sidebar, body)



#Variables that are shared among server functions
D6DF <- CD6.makeDataFrame()
oldrightop <- rightop <- "" # we're going to store the previous value for the right operand
oldleftop <- leftop <- ""   # as well as both old and new values for the left operand as well
product <- ""
rperm <- "(123456)" # our default r 
fperm <- "(12)(34)(56)" # our default f
#chooseRight <- TRUE
opchoice <- "" # I replaced the chooseRight boolean with a flag that can be "L", "R", or "".
# This way you can't start accidentally marking vertices without having pushed a button
# It also lets us reset the colors of vertices once we're done looking at the product
# or when we change one of the factors

#Functions that read the input and modify the output and input
server <- function(session, input, output) {
  #Initialization
  output$cayley <- renderPlot(CD6.drawGraph(D6DF)) 
  
  #Set a flag so we know how to use the next mouse click
  observeEvent(input$btnleft,{
    output$message <- renderUI(h3("Click on a vertex"))
    opchoice <<- "L"
  })
  observeEvent(input$btnright,{
    output$message <- renderUI(h3("Click on a vertex"))
    opchoice <<- "R"
  })
  #Use the mouse click to select a vertex
  observeEvent(input$plot_click,{
    i <- CD6.findClosestVertex(input$plot_click$x,input$plot_click$y)
    if (opchoice == "R"){
      oldrightop <<- rightop
      rightop <<- D6DF$sana[i]
      output$right <- renderUI(h3(HTML(CD6.manifestWord(rightop))))
      oldi <- which(D6DF$sana == oldrightop)
      D6DF <<- CD6.markVertex(D6DF,oldi,"lightgray") # first reset the old right operand
      D6DF <<- CD6.markVertex(D6DF,product,"lightgray") # also reset the product
      D6DF <<- CD6.markVertex(D6DF,i,"orangered") # color the vertex for the new right operand
      
      output$message <- renderUI("")
    }
    if (opchoice=="L"){
      oldleftop <<- leftop # remember the previous left operand
      leftop <<- D6DF[i,]$sana # find the new left operand
      output$left <- renderUI(h3(HTML(CD6.manifestWord(leftop)))) # tell user what generator word applies
      oldi <- which(D6DF$sana == oldleftop) # we are going need to figure out what to recolor
      D6DF <<- CD6.markVertex(D6DF,oldi,"lightgray")
      D6DF <<- CD6.markVertex(D6DF,product,"lightgray")
      D6DF <<- CD6.markVertex(D6DF,i,"darkturquoise")
      output$message <- renderUI("")
    }
    #Redraw the graph to show the selected vertex
    output$cayley <- renderPlot(CD6.drawGraph(D6DF))
  })									  
  
  #Multiply the selected group elements
  observeEvent(input$btncalc,{
    output$message <- renderUI("")
    product <<- CD6.multiply(leftop,rightop)
    msg <- paste0("The product is ",CD6.manifestWord(leftop),CD6.manifestWord(rightop)," which simplifies to ",CD6.manifestWord(product),".")
    output$prod <- renderUI(h3(HTML(msg)))
    D6DF <<- CD6.markVertex(D6DF,product,"slateblue")
    #Redraw the graph to show the result
    output$cayley <- renderPlot(CD6.drawGraph(D6DF))
  })
  
  
  #This depends on three inputs but responds only to the button
  observeEvent(input$makeperm,{
    D6DF <<- CD6.makePerms(D6DF,r=input$chooser,f=input$choosef) # consider radio buttons instead of drop-down menu
    output$cayley <- renderPlot(CD6.drawGraph(D6DF,permlabel=TRUE))    
  })							  
}

#Run the app
shinyApp(ui = ui, server = server)
