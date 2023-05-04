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
    img(src = paste0("trainers/",input$trainer,".jpg"),height="30%", width="30%")
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
              width=9
            ),
            column(
              uiOutput("bag_display"),
              width=3
            )),
          br(),
          br(),
          br(),          
          br(),
          br(),
          br(),
          fluidRow(
            column(
              uiOutput("buddy_display"),
              width=9
            ),
            column(
              uiOutput("pokeball_display"),
              width=3
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
      hr(),
      fluidRow(
        h3("Map"),
        img(src = "map.jpg",height="30%", width="30%"),
        selectInput("region", label = h3("Select Region"), 
                    choices = list(
                      # "Dragon" = "dragon",
                      # "Hawaii" = "hawaii",
                      "Water" = "water",
                      # "Sky" = "sky",
                      "Grass" = "grass",
                      "Egypt" = "egypt",
                      # "Poison" = "poison",
                      # "Fire" = "fire",
                      "Dark" = "dark",
                      "Rock" = "rock",
                      "Ice" = "ice",
                      "Random"="random"
                    ), 
                    selected = "water")
      ),
      uiOutput("pokemon_encounter"),
      actionButton("go","Go"),
      uiOutput("the_pokedex_ui")
      
      )
      
      
    }
    
  })
  
 
  pokedex=reactiveValues(thelist=list())
  legendaries <- gsub("\\.jpg","",list.files(paste0("www/pokemon/legendary")))
  
  themons=reactive({
    
    themons=gsub("\\.jpg","",list.files(paste0("www/pokemon/",input$region)))
  # themons=themons[themons!="oclti"]
    themons
  })
  
  observeEvent(input$go,{
    
    current_region=input$region
    
    if(input$go==1){
      thepokemon<<-sample(themons(),2) #get an initial sample
    }else{
      
      if(runif(1)>.99){
        thepokemon<<-c(thepokemon[2],sample(legendaries,1))
        }else{
          thepokemon<<-c(thepokemon[2],sample(themons(),1))
        }
    }
    
    output$pokemon_encounter = renderUI({
      
      div(
        h3(paste("You are in the ",current_region,"region.")),
        h3(paste0("Wow! Its ",gsub("_"," ",thepokemon[2]),"! ",if(thepokemon[2] %in% legendaries){"WOW!!!!!  It's LEGENDARY!!!!!!!!!!"}else{""})),
        img(src = paste0("pokemon/",if(thepokemon[2] %in% legendaries){"legendary"}else{current_region},"/",thepokemon[2],".jpg"),height="10%", width="10%"),
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
  
  output$the_pokedex_ui=renderUI({
    
    if(length(pokedex$thelist)>0){
    div(
      hr(),
      h3("Pokedex"),
      verbatimTextOutput("pokedex_display")
    )
    }
  })
  
      


}

# Run the application 
shinyApp(ui = ui, server = server)
