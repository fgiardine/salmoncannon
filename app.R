library(shiny)
library(dplyr)
library(ggplot2)
library(gganimate)
library(gifski)
library(ggimage)
#change to whatever time you want


data <- data.frame(time = c(0:5))
ui <- fluidPage(
  titlePanel("Fish Cannon"),
  sidebarLayout(
    sidebarPanel(selectInput("fishInput", "Fish Species", choices = c("Shad", "Salmon")),
                 selectInput("damInput", "Dam Size:", choices = c("Small", "Medium", "Large")),
                 sliderInput("angle",
                             "Initial angle:",
                             min = 0,
                             max = 90,
                             value = 1),
                 sliderInput("height",
                             "Height above water:",
                             #change to height possibilities you want
                             min = 0,
                             max = 150,
                             value = 1)),
    mainPanel(plotOutput("plot1"),
              #add any more plots here
              imageOutput("position"),
              br(),
              br(),
              tableOutput("results"))
  )
)
server <- function(input, output) {
  output$plot1 <- renderPlot({
    data <- data %>% 
      mutate(mass = case_when(
        #change fish weight, if add fish make sure to add to input
        input$fishInput == "Salmon" ~ 3,
        input$fishInput == "Shad" ~ 1),
        image = case_when(
          #this is just the image location url, no need to download :)
          input$fishInput == "Salmon" ~ "https://webstockreview.net/images/salmon-clipart-salmon-alaskan-1.png",
          input$fishInput == "Shad" ~ "https://lh3.googleusercontent.com/proxy/T3qjG1rOYyMwkk82Ei95GO8MOcoSwThg9Gvx9dPSLibhyVH6cTvHmD4dBKt-XmUR2eaITqVVPhohvzkcHEJYJHvU71W4EV0lCz4tTmYDkdXQ2HSyjL9UA8bwNnSoK48"),
        init_velocity = case_when(
          #change to appropriate velocity
          input$damInput == "Small" ~ 10,
          input$damInput == "Medium" ~ 16,
          input$damInput == "Large" ~ 22),
        #change to actual formula
        vel_y= init_velocity*sin(input$angle*3.14/180)-9.81*time,
        vel_x= init_velocity*cos(input$angle*3.14/180),
        vel_fin_x= sqrt(2*9.81*input$height+(init_velocity*cos(input$angle*3.14/180))^2),
        vel_fin_y= sqrt(2*9.81*input$height+(init_velocity*sin(input$angle*3.14/180))^2),
        vel = sqrt(vel_x^2 + vel_y^2),
        vel_fin = sqrt(vel_fin_x^2 + vel_fin_y^2),
        position = 1,
        #change
        x = position,
        y = position)
    
    ggplot(data, aes(x = time, y = vel_y)) + geom_point()
  })
  output$position <- renderImage({
    data <- data %>%
      mutate(mass = case_when(
        input$fishInput == "Salmon" ~ 3,
        input$fishInput == "Shad" ~ 1),
        image = case_when(
          input$fishInput == "Salmon" ~ "https://webstockreview.net/images/salmon-clipart-salmon-alaskan-1.png",
          input$fishInput == "Shad" ~ "https://tpwd.texas.gov/fishboat/fish/images/inland_species/gizzardshad.jpg"),
        velocity = case_when(
          input$damInput == "Small" ~ mass * 1,
          input$damInput == "Medium" ~ mass * 2,
          input$damInput == "Large" ~ mass * 3),
        position = mass * velocity * input$angle * input$height * time,
        x = position,
        y = position)
    outfile <- tempfile(fileext='.gif')
    animation <- ggplot(data, aes(x = time, y = position)) + geom_image(aes(image=image), size=.1) +
      labs(title = 'Position after {frame_time} seconds') +
      transition_time(time) +
      ease_aes('linear')
    anim_save("outfile.gif", animate(animation)) 
    list(src = "outfile.gif",
         contentType = 'image/gif'
    )}, 
  )}
shinyApp(ui = ui, server = server)
