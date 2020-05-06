library(shiny)
library(dplyr)
library(ggplot2)
library(gganimate)
library(gifski)
library(ggimage)
#change to whatever time you want


data <- data.frame(time = seq(from = 0, to = 3, by = 0.05))
data2<-data.frame(water_vel = seq(from = 0, to = 7, by = 0.5))
data3<-data.frame(delta = seq(from = 0, to = 1, by = 1/14))
ui <- fluidPage(
  titlePanel("Fish Cannon"),
  sidebarLayout(
    sidebarPanel(selectInput("fishInput", "Fish Species", choices = c("Shad", "Salmon")),
                 selectInput("damInput", "Dam Size:", choices = c("Small", "Medium", "Large")),
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
    mainPanel(plotOutput("plot1"),
              #add any more plots here
              plotOutput("position"),
              textOutput("results"),
              plotOutput("force"))
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
        vel_fin_x= init_velocity*cos(input$angle*3.14/180),
        vel_fin_y= sqrt(2*9.81*input$height/100+(init_velocity*sin(input$angle*3.14/180))^2),
        vel = sqrt(vel_x^2 + vel_y^2),
        vel_fin = sqrt(vel_fin_x^2 + vel_fin_y^2),
        position_x =  vel_x * time,
        position_y = input$height/100 + init_velocity*sin(input$angle*3.14/180)*time - 0.5*9.81*time^2)%>%
      filter(position_y>0)
    
    ggplot(data, aes(x = time, y = vel)) + geom_point()+ ggtitle("Velocity before impact") +
      xlab("Time") +
      ylab("Velocity (m/s)")
  })
  output$position <- renderPlot({
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
        vel_fin_x= init_velocity*cos(input$angle*3.14/180),
        vel_fin_y= sqrt(2*9.81*input$height/100+(init_velocity*sin(input$angle*3.14/180))^2),
        vel = sqrt(vel_x^2 + vel_y^2),
        vel_fin = sqrt(vel_fin_x^2 + vel_fin_y^2),
        position_x =  vel_x * time,
        position_y = input$height/100 + init_velocity*sin(input$angle*3.14/180)*time - 0.5*9.81*time^2)%>%
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
  
  output$results <- renderText({
    data2 <- data2 %>% 
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
        vel_fin_x= init_velocity*cos(input$angle*3.14/180),
        vel_fin_y= sqrt(2*9.81*input$height/100+(init_velocity*sin(input$angle*3.14/180))^2),
        vel_fin = sqrt(vel_fin_x^2 + vel_fin_y^2),
        impact = mass*(water_vel - vel_fin))
    # ggplot(data2) + geom_point(aes(x = input$fishInput, y = impact, colour = "X (m)"))+
    #   scale_colour_manual("", 
    #                       breaks = c("X (m)"),
    #                       values = c("magenta"))  
    paste("Impulse (N s) ranges from min = ", round(min(data2$impact),2), "to max = ", round(max(data2$impact),2), "with a mean of",round(mean(data2$impact),2))
  })
  
 output$force <- renderPlot({
    data2 <- data2 %>% 
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
        vel_fin_x= init_velocity*cos(input$angle*3.14/180),
        vel_fin_y= sqrt(2*9.81*input$height/100+(init_velocity*sin(input$angle*3.14/180))^2),
        vel_fin = sqrt(vel_fin_x^2 + vel_fin_y^2),
        impact = mass*(water_vel - vel_fin),
    delta = seq(from = 0, to = 1, by = 1/14),
    force = impact/delta)
    ggplot(data2) + geom_point(aes(x = input$fishInput, y = force, colour = "X (m)")) +
      scale_colour_manual("", 
                          breaks = c("X (m)"),
                          values = c("magenta")) 
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
