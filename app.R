#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Otis's Pokemon Game"),
    uiOutput("display_screen"),
    br(),
    br(),
    hr(),
    conditionalPanel("input.play==0",fluidRow(actionButton("play",label="Play Game")))

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$trainer_display = renderUI({
    div(
    h2("Trainer"),
    img(src = paste0("trainers/",input$trainer,".jpg"),height="60%", width="60%")
    )
    
  })
  
  output$bag_display = renderUI({

    div(
      h2("Bag"),
      img(src = paste0("bags/",input$bag,".jpg"),height="60%", width="60%")
    )

  })
  
  output$pokeball_display = renderUI({
    div(
    h2("Pokeball"),
    img(src = paste0("balls/",input$pokeball,".jpg"),height="60%", width="60%")
    )
    
  })
  
  output$buddy_display = renderUI({
    div(
      h2("Buddy"),
      img(src = paste0("buddies/",input$buddy,".jpg"),height="60%", width="60%")
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  

  output$display_screen=renderUI({
    
    if(input$play==0){
    
    ###
    # Select Screen
    ###


    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        width = 2,
        selectInput("trainer", label = h3("Pick Trainer"), 
                    choices = list("Cool Guy" = "coolguy",
                                   "Paaonm" = "paaonm"), 
                    selected = "nosword"),
        selectInput("buddy", label = h3("Pick Buddy"), 
                    choices = list(
                      "Gom" = "gom",
                      "Momom" = "momom",
                      "Tmotnot" = "tmotnot",
                      "Ots" = "ots",
                      "Opm" = "opm",
                      "Tis" = "tis",
                      "Tvtv" = "tvtv"
                    ), 
                    selected = "nosword"),
        selectInput("bag", label = h3("Pick Pokebag"),
                    choices = list("Bulbasaur" = "bulb",
                                   "Chansey" = "chansey",
                                   "Normal Type" = "normal"
                    ),
                    selected = "bulb"),
        selectInput("pokeball", label = h3("Pick Pokeball"), 
                    choices = list("LOL" = "lol",
                                   "XOXO" = "xoxo",
                                   "AOX" = "aox",
                                   "NTI" = "nti",
                                   "NMO" = "nmo",
                                   "Gigantamax Ball" = "gmb"
                                   
                    ), 
                    selected = "lol"),
        br(),
        hr(),
        br()
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        div(
          br(),
          br(),
          fluidRow(
            column(
              uiOutput("trainer_display"),
              width=9,offset = 0, style='padding: 0px 0px;'
            ),
            column(
              uiOutput("bag_display"),
              width=3,offset = 0, style='padding: 0px 0px;margin:-2em'
            )),
          br(),
          hr(),
          br(),
          fluidRow(
            column(
              uiOutput("buddy_display"),
              width=9,offset = 0, style='padding: 0px 0px;margin:-2em'
            ),
            column(
              uiOutput("pokeball_display"),
              width=3,offset = 0, style='padding: 0px 0px;margin:-2em'
            )
          )
        )
      )
    )
      
    # }else{
    #   
    #   ###
    #   # Game Screen
    #   ###
    #   
    #   h3(paste("You have chosen:",input$trainer,input$buddy,input$bag,input$pokeball))
    #   
    #   
    # }
    }else{
      
      div(
      h3(paste("You have chosen:",input$trainer,input$buddy,input$bag,input$pokeball)),
      uiOutput("pokemon_encounter"),
      actionButton("go","Go"),
      verbatimTextOutput("pokedex_display")
      )
      
      
    }
    
  })
  
  
  pokedex=reactiveValues(thelist=list())
  themons=gsub("\\.jpg","",list.files("www/pokemon"))
  themons=themons[themons!="oclti"]
  thepokemon<<-sample(themons,2)
  
  observeEvent(input$go,{
    
    if(input$go>1){
      
      if(runif(1)>.99){
        thepokemon<<-c(thepokemon[2],"oclti")
        }else{
          thepokemon<<-c(thepokemon[2],sample(themons,1))
        }
    }
    
    output$pokemon_encounter = renderUI({
      
      div(
        h3(paste("Wow! Its",thepokemon[2],if(thepokemon[2]=="oclti"){"WOW!!!!!  It's LEGENDARY!!!!!!!!!!"}else{""})),
        img(src = paste0("pokemon/",thepokemon[2],".jpg"),height="10%", width="10%"),
        selectInput("what_to_do", label = "What do you want to do?", 
                    choices = list("Catch It" = "catch", "Run" = "run"), 
                    selected = "catch"),
        
      )
    })
    
    
      req(input$what_to_do)
      
      if(input$what_to_do=="catch"){
        
        if(thepokemon[1] %in% names(pokedex$thelist)){
          pokedex$thelist[[thepokemon[1]]]=pokedex$thelist[[thepokemon[1]]]+1
        }else{
          pokedex$thelist[[thepokemon[1]]]=1
        }
        
      }
    })
  
  output$pokedex_display=renderPrint({
    
    if(length(pokedex$thelist)>0){
      df = as.data.frame(pokedex$thelist)
      rownames(df)=NULL
      df
    }
    
  })
  
  
      


}

# Run the application 
shinyApp(ui = ui, server = server)
