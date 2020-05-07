library(shiny)
library(dplyr)
library(ggplot2)
library(gganimate)
library(gifski)
library(ggimage)
library(formattable)
library(htmlTable)
library(magrittr)
#change to whatever time you want


data <- data.frame(time = seq(from = 0, to = 3, by = 0.01))
data2<-data.frame(time = seq(from = 0, to = 3, by = 0.01))


ui <- fluidPage(
  titlePanel("The Fish Cannon: Calculating the Exit Projectile"),
  
  sidebarLayout(
    sidebarPanel(tags$img(src='bro.png', height=190),
                 br(),
      selectInput("fishInput", "Fish Species", choices = c("American shad",
                                                                      "Chinook Salmon",
                                                                      "Rainbow trout",
                                                                      "Lake Sturgeon",
                                                                      "Sockeye Salmon",
                                                                      "Pink Salmon",
                                                                      "Atlantic Salmon",
                                                                      "Pacific Salmon")),
                 sliderInput("damInput", "Velocity in cm/s",
                             min = 100,
                             max = 900,
                             value = 0.1),
                 sliderInput("angle",
                             "Initial angle (degrees):",
                             min = -90,
                             max = 90,
                             value = 1),
                 sliderInput("height",
                             "Height of tube above water (cm):",
                             #change to height possibilities you want
                             min = 1,
                             max = 300,
                             value = 1)),
    mainPanel( h3("Example of the Fish Tube and the Exit"),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/6Gd-DCGku5U?start=75" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      
      h2("Velocity of Fish After Exit (m/s)"),
              plotOutput("plot1"),
      br(),
              h3("X and Y position of Fish after Exit (m)"),
              plotOutput("position"),
      br(),
              h3("Force on Fish Hitting the Water (N)"),
              h6("For a fish with a speed after they hit the water close to the speed of them in the air, they will experience a very small force. For a fish with a slower submersion this will also be very small. Here users can use the velocity of the fish after the water and the submergence time to predict the force on the fish."),
              tableOutput("results"),
              br(),
              h3("The G-Force of this Impact (g's)"),
              tableOutput("accel"))
  )
)
server <- function(input, output) {
  
  
  
  output$plot1 <- renderPlot({
    data <- data %>% 
      mutate(mass = case_when(
        #change fish weight, if add fish make sure to add to input
        input$fishInput == "Chinook Salmon" ~ 18,
        input$fishInput == "American shad" ~ 5.5,
        input$fishInput == "Rainbow trout" ~ 9,
        input$fishInput == "Lake Sturgeon" ~ 6.4,
        input$fishInput == "Sockeye Salmon" ~ 4.2,
        input$fishInput == "Pink Salmon" ~ 3.7,
        input$fishInput == "Atlantic Salmon" ~ 5.4,
        input$fishInput == "Pacific Salmon" ~ 9.1),
        image = case_when(
          #this is just the image location url, no need to download :)
          input$fishInput == "Salmon" ~ "https://webstockreview.net/images/salmon-clipart-salmon-alaskan-1.png",
          input$fishInput == "Shad" ~ "https://lh3.googleusercontent.com/proxy/T3qjG1rOYyMwkk82Ei95GO8MOcoSwThg9Gvx9dPSLibhyVH6cTvHmD4dBKt-XmUR2eaITqVVPhohvzkcHEJYJHvU71W4EV0lCz4tTmYDkdXQ2HSyjL9UA8bwNnSoK48"),
    
        #change to actual formula
        vel_y= input$damInput/100*sin(input$angle*3.14/180)-9.81*time,
        vel_x= input$damInput/100*cos(input$angle*3.14/180),
        vel_fin_x= input$damInput/100*cos(input$angle*3.14/180),
        vel_fin_y= sqrt(2*9.81*input$height/100+(input$damInput/100*sin(input$angle*3.14/180))^2),
        vel = sqrt(vel_x^2 + vel_y^2),
        vel_fin = sqrt(vel_fin_x^2 + vel_fin_y^2),
        position_x =  vel_x * time,
        position_y = input$height/100 + input$damInput/100*sin(input$angle*3.14/180)*time - 0.5*9.81*time^2)%>%
      filter(position_y>0)
    
    ggplot(data, aes(x = time, y = vel)) + geom_point()+ ggtitle("Velocity before impact") +
      xlab("Time") +
      ylab("Velocity (m/s)")
  })
  output$position <- renderPlot({
    data <- data %>% 
      mutate(mass = case_when(
        #change fish weight, if add fish make sure to add to input
        input$fishInput == "Chinook Salmon" ~ 18,
        input$fishInput == "American shad" ~ 5.5,
        input$fishInput == "Rainbow trout" ~ 9,
        input$fishInput == "Lake Sturgeon" ~ 6.4,
        input$fishInput == "Sockeye Salmon" ~ 4.2,
        input$fishInput == "Pink Salmon" ~ 3.7,
        input$fishInput == "Atlantic Salmon" ~ 5.4,
        input$fishInput == "Pacific Salmon" ~ 9.1),
        image = case_when(
          #this is just the image location url, no need to download :)
          input$fishInput == "Salmon" ~ "https://webstockreview.net/images/salmon-clipart-salmon-alaskan-1.png",
          input$fishInput == "Shad" ~ "https://lh3.googleusercontent.com/proxy/T3qjG1rOYyMwkk82Ei95GO8MOcoSwThg9Gvx9dPSLibhyVH6cTvHmD4dBKt-XmUR2eaITqVVPhohvzkcHEJYJHvU71W4EV0lCz4tTmYDkdXQ2HSyjL9UA8bwNnSoK48"),
        #change to actual formula
        vel_y= input$damInput/100*sin(input$angle*3.14/180)-9.81*time,
        vel_x= input$damInput/100*cos(input$angle*3.14/180),
        vel_fin_x= input$damInput/100*cos(input$angle*3.14/180),
        vel_fin_y= sqrt(2*9.81*input$height/100+(input$damInput/100*sin(input$angle*3.14/180))^2),
        vel = sqrt(vel_x^2 + vel_y^2),
        vel_fin = sqrt(vel_fin_x^2 + vel_fin_y^2),
        position_x =  vel_x * time,
        position_y = input$height/100 + input$damInput/100*sin(input$angle*3.14/180)*time - 0.5*9.81*time^2)%>%
      filter(position_y>0)
    ggplot(data, aes(x = time)) + geom_line(aes(y = position_x, colour = "X (m)")) +
      geom_line(aes(y = position_y, colour = "Y (m)")) +
      scale_colour_manual("", 
                          breaks = c("X (m)", "Y (m)"),
                          values = c("cyan4", "magenta")) + 
      ggtitle("X and Y displacement of fish (m)") +
    xlab("Time (s)") +
    ylab("Position (m)")
  })
  
  output$results <- renderTable({
    data3 <- data.frame(dummy = 1:10)%>% 
      mutate(mass = case_when(
        #change fish weight, if add fish make sure to add to input
        input$fishInput == "Chinook Salmon" ~ 18,
        input$fishInput == "American shad" ~ 5.5,
        input$fishInput == "Rainbow trout" ~ 9,
        input$fishInput == "Lake Sturgeon" ~ 6.4,
        input$fishInput == "Sockeye Salmon" ~ 4.2,
        input$fishInput == "Pink Salmon" ~ 3.7,
        input$fishInput == "Atlantic Salmon" ~ 5.4,
        input$fishInput == "Pacific Salmon" ~ 9.1),
        image = case_when(
          #this is just the image location url, no need to download :)
          input$fishInput == "Salmon" ~ "https://webstockreview.net/images/salmon-clipart-salmon-alaskan-1.png",
          input$fishInput == "Shad" ~ "https://lh3.googleusercontent.com/proxy/T3qjG1rOYyMwkk82Ei95GO8MOcoSwThg9Gvx9dPSLibhyVH6cTvHmD4dBKt-XmUR2eaITqVVPhohvzkcHEJYJHvU71W4EV0lCz4tTmYDkdXQ2HSyjL9UA8bwNnSoK48"),
        vel_fin_x= input$damInput/100*cos(input$angle*3.14/180),
        vel_fin_y= sqrt((2*9.81)*input$height/100+(input$damInput/100*sin(input$angle*3.14/180))^2),
        vel_fin = sqrt(vel_fin_x^2 + vel_fin_y^2),
        water_vel = seq(0,max(vel_fin),length.out=10),
        delta = seq(1/0.01,1/1.15,length.out=10),
        impact = mass*(water_vel - vel_fin))
    
    
    force3 = abs(data3$impact%o%data3$delta)
    accel = force3/9.81/data3$mass
    x<-data.frame("vel" = seq(0,max(data3$vel_fin),length.out=10),"l" = c("|","|","|","|","|","|","|","|","|","|"),force3)%>%rename("Velocity of Fish In Water" = "vel",
                                                                                   "dt = 0.01s" = "X1",
                                                                                   "dt = 0.14s" = "X2",
                                                                                   "dt = 0.26s" = "X3",
                                                                                   "dt = 0.39s" = "X4",
                                                                                   "dt = 0.52s" = "X5",
                                                                                   "dt = 0.64s" = "X6",
                                                                                   "dt = 0.77s" = "X7",
                                                                                   "dt = 0.90s" = "X8",
                                                                                   "dt = 1.02s" = "X9",
                                                                                   "dt = 1.15s" = "X10")
      
   
  })
  


output$accel <- renderTable({
  data3 <- data.frame(dummy = 1:10)%>% 
    mutate(mass = case_when(
      #change fish weight, if add fish make sure to add to input
      input$fishInput == "Chinook Salmon" ~ 18,
      input$fishInput == "American shad" ~ 5.5,
      input$fishInput == "Rainbow trout" ~ 9,
      input$fishInput == "Lake Sturgeon" ~ 6.4,
      input$fishInput == "Sockeye Salmon" ~ 4.2,
      input$fishInput == "Pink Salmon" ~ 3.7,
      input$fishInput == "Atlantic Salmon" ~ 5.4,
      input$fishInput == "Pacific Salmon" ~ 9.1),
      image = case_when(
        #this is just the image location url, no need to download :)
        input$fishInput == "Salmon" ~ "https://webstockreview.net/images/salmon-clipart-salmon-alaskan-1.png",
        input$fishInput == "Shad" ~ "https://lh3.googleusercontent.com/proxy/T3qjG1rOYyMwkk82Ei95GO8MOcoSwThg9Gvx9dPSLibhyVH6cTvHmD4dBKt-XmUR2eaITqVVPhohvzkcHEJYJHvU71W4EV0lCz4tTmYDkdXQ2HSyjL9UA8bwNnSoK48"),
      vel_fin_x= input$damInput/100*cos(input$angle*3.14/180),
      vel_fin_y= sqrt((2*9.81)*input$height/100+(input$damInput/100*sin(input$angle*3.14/180))^2),
      vel_fin = sqrt(vel_fin_x^2 + vel_fin_y^2),
      water_vel = seq(0,max(vel_fin),length.out=10),
      delta = seq(1/0.01,1/1.15,length.out=10),
      impact = mass*(water_vel - vel_fin))
  
  
  force3 = abs(data3$impact%o%data3$delta)
  accel = force3/9.81/data3$mass
  x<-data.frame("vel" = seq(0,max(data3$vel_fin),length.out=10),"l" = c("|","|","|","|","|","|","|","|","|","|"), accel)%>%rename("Velocity of Fish In Water" = "vel",
                                                                                 "dt = 0.01s" = "X1",
                                                                                 "dt = 0.14s" = "X2",
                                                                                 "dt = 0.26s" = "X3",
                                                                                 "dt = 0.39s" = "X4",
                                                                                 "dt = 0.52s" = "X5",
                                                                                 "dt = 0.64s" = "X6",
                                                                                 "dt = 0.77s" = "X7",
                                                                                 "dt = 0.90s" = "X8",
                                                                                 "dt = 1.02s" = "X9",
                                                                                 "dt = 1.15s" = "X10")
  
  
})}


  # output$position <- renderImage({
  #   data <- data %>%
  #     mutate(mass = case_when(
  #       input$fishInput == "Salmon" ~ 3,
  #       input$fishInput == "Shad" ~ 1),
  #       image = case_when(
  #         input$fishInput == "Salmon" ~ "https://webstockreview.net/images/salmon-clipart-salmon-alaskan-1.png",
  #         input$fishInput == "Shad" ~ "https://tpwd.texas.gov/fishboat/fish/images/inland_species/gizzardshad.jpg"),
  #       init_velocity = case_when(
  #         input$damInput == "Small" ~ 10,
  #         input$damInput == "Medium" ~ 16,
  #         input$damInput == "Large" ~ 22),
  #       vel_y= init_velocity*sin(input$angle*3.14/180)-9.81*time,
  #       vel_x= init_velocity*cos(input$angle*3.14/180),
  #       vel_fin_x= sqrt(2*9.81*input$height+(init_velocity*cos(input$angle*3.14/180))^2),
  #       vel_fin_y= sqrt(2*9.81*input$height+(init_velocity*sin(input$angle*3.14/180))^2),
  #       vel = sqrt(vel_x^2 + vel_y^2),
  #       vel_fin = sqrt(vel_fin_x^2 + vel_fin_y^2),
  #       position_x =  vel_x * time,
  #       position_y = input$height + init_velocity*sin(input$angle*3.14/180)*time - 0.5*9.81*time^2)%>%filter(position_y>0)
  #   outfile <- tempfile(fileext='.gif')
  #   animation <- ggplot(data, aes(x = time, y = position_y)) + geom_image(aes(image=image), size=.1) +
  #     labs(title = 'Position after {frame_time} seconds') +
  #     transition_time(time) +
  #     ease_aes('linear')
  #   anim_save("outfile.gif", animate(animation)) 
  #   list(src = "outfile.gif",
  #        contentType = 'image/gif' 
  #   )} 
   
shinyApp(ui = ui, server = server)
