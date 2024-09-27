library(shiny)
library(ggplot2)
library(dplyr)

# Setting theme for ggplot
theme_set(theme_bw())

# Constants for watts to photons conversion
planck =  6.62607015*10^-34
lightspeed = 299792458
Avogadro = 6.02214076*10^23

ui <- fluidPage(
  titlePanel("Spectra Analysis App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("light_spectrum", "Select the light emission spectrum"),
      fileInput("detector1_spectrum", "Select the first light detector"),
      fileInput("detector2_spectrum", "Select the second light detector"),
      checkboxInput("power_units_checkbox", "Light spectrum is in power units?", value = TRUE),
      numericInput("input_value", "Input the amount of photons shown by the first detector:", value = 50)
    ),
    mainPanel(
      column(6,
             plotOutput("plot_detector1_spectrum", height = 300, width = 450),
             plotOutput("plot_detector2_spectrum", height = 300, width = 450),
             plotOutput("plot_light_spectrum", height = 300, width = 450)
      ),
      column(6,
             plotOutput("plot_detector1_right", height = 300, width = 450),
             plotOutput("plot_detector2_right", height = 300, width = 450),
             uiOutput("reactive_sentence")
      )
    )
  )
)

server <- function(input, output, session) {
  
  light_data <- reactive({
    req(input$light_spectrum)
    data <- read.csv(input$light_spectrum$datapath)
    return(data)
  })
  
  detector1_data<- reactive({
    req(input$detector1_spectrum)
    data <- read.csv(input$detector1_spectrum$datapath)
    return(data)
  })
  
  detector2_data<- reactive({
    req(input$detector2_spectrum)
    data <- read.csv(input$detector2_spectrum$datapath)
    return(data)
  })
  
  # Aggregating light spectrum data, and converting to photons if checkbox checked
  divided_light_spectrum <- reactive({
    req(light_data)
    data <- light_data()
    
    data[,1] <- round(data[,1])
    data <- aggregate(data[,2], by=list(data[,1]), FUN = mean)
    
    # Check if the checkbox is checked
    if (input$power_units_checkbox) {
      for(i in 1:length(data[,2])){
        data[,2][i] <- data[,2][i] * (data[,1][i] * 10^-9) * 1000 / (lightspeed*planck*Avogadro)
      }
      data[,2] <- data[,2]/max(data[,2])
    }
    
    return(data)
  })
  
  #Imports the first detector
  detectorA <- reactive({
    req(detector1_data)
    data <- detector1_data()
    
    #if y values > 5, divide by 100 (avoids percentage vs unit ratios problems)
    if(max(data[,2] > 5)){
      data[,2] <- data[,2] /100
    }
    
    return(data)
  })
  #Imports the second detector
  detectorB <- reactive({
    req(detector2_data)
    data <- detector2_data()
    
    #if y values > 5, divide by 100 (avoids percentage vs unit ratios problems)
    if(max(data[,2] > 5)){
      data[,2] <- data[,2] /100
    }
    
    return(data)
  })
  # Multiplying the first detector response by the light spectrum
  detectorAxlamp <- reactive({
    Adect <- detectorA()
    lamp <- divided_light_spectrum()
    
    col_A <- names(Adect)[1]
    col_B <- names(lamp)[1]
    # Merging the data to have the same x values, and filling the NAs by 0s
    temp <- merge(Adect, lamp, by.x = col_A, by.y = col_B, all = TRUE)
    temp[,2] <- dplyr::coalesce(temp[,2],0)
    temp[,3] <- dplyr::coalesce(temp[,3],0)
    # We only take col 1 (nm) and col 4 (mutiplied values)
    temp <- mutate(temp, mult = temp[,2] * temp[,3])
    data <- data.frame(temp[,1], temp[,4])
    
    return(data)
  })
  
  detectorBxlamp <- reactive({
    Bdect <- detectorB()
    lamp <- divided_light_spectrum()
    
    col_A <- names(Bdect)[1]
    col_B <- names(lamp)[1]
    # Merging the data to have the same x values, and filling the NAs by 0s
    temp <- merge(Bdect, lamp, by.x = col_A, by.y = col_B, all = TRUE)
    temp[,2] <- dplyr::coalesce(temp[,2],0)
    temp[,3] <- dplyr::coalesce(temp[,3],0)
    # We only take col 1 (nm) and col 4 (mutiplied values)
    temp <- mutate(temp, mult = temp[,2] * temp[,3])
    data <- data.frame(temp[,1], temp[,4])
    
    return(data)
  })
  # Calculating the aeras under curve (AUC) with the trapezoidal method
  # rect can work with just data[,2][i] if we consider the nm intervals to be 1
  lamp_AUC <- reactive({
    Lamp <- divided_light_spectrum()
    AUCval = 0
    rect = 0
    trig = 0
    
    for(i in 1:(length(Lamp[,2])-1)){
      rect = Lamp[,2][i]
      trig = (Lamp[,2][i+1] - Lamp[,2][i]) /2
      AUCval <- AUCval + rect + trig
    }
    print("Lamp AUC")
    print(AUCval)
    return(AUCval)
  })
  
  detectorA_AUC <- reactive({
    DetectorA <- detectorAxlamp()
    AUCval = 0
    rect = 0
    trig = 0
    
    for(i in 1:(length(DetectorA[,2])-1)){
      rect = DetectorA[,2][i]
      trig = (DetectorA[,2][i+1] - DetectorA[,2][i]) /2
      AUCval <- AUCval + rect + trig
    }
    print("Detector A AUC")
    print(AUCval)
    return(AUCval)
    
  })
  
  detectorB_AUC <- reactive({
    DetectorB <- detectorBxlamp()
    AUCval = 0
    rect = 0
    trig = 0
    
    for(i in 1:(length(DetectorB[,2])-1)){
      rect = DetectorB[,2][i]
      trig = (DetectorB[,2][i+1] - DetectorB[,2][i]) /2
      AUCval <- AUCval + rect + trig
    }
    print("Detector B AUC")
    print(AUCval)
    return(AUCval)
    
  })
  
  
  # Generating plots
  output$plot_light_spectrum <- renderPlot({
    data <- divided_light_spectrum()
    ggplot(data, aes(x = data[, 1], y = data[, 2]),size = 1) +
      geom_line() +
      labs(title = "Light Emission Spectrum", y = "Response", x = "Wavelength (nm)")
  })
  
  output$plot_detector1_spectrum <- renderPlot({
    data <- detectorA()
    #This allows the x axis to be the same for everyone, taking the bounds of the light emission spectrum
    light_data <- divided_light_spectrum()
    x1 <- min(light_data[,1])
    x2 <- max(light_data[,1])
    
    ggplot(data, aes(x = data[, 1], y = data[, 2]),size = 1) +
      geom_line() +
      labs(title = "First Light Detector Spectrum", y = "Response", x = "Wavelength (nm)") +
      xlim(x1,x2)
  })
  
  output$plot_detector2_spectrum <- renderPlot({
    data <- detectorB()
    
    light_data <- divided_light_spectrum()
    x1 <- min(light_data[,1])
    x2 <- max(light_data[,1])
    
    ggplot(data, aes(x = data[, 1], y = data[, 2]),size = 1) +
      geom_line() +
      labs(title = "Second Light Detector Spectrum", y = "Response", x = "Wavelength (nm)") +
      xlim(x1,x2)
  })
  
  output$plot_detector1_right <- renderPlot({
    data <- detectorAxlamp()
    data2 <- divided_light_spectrum()
    
    ggplot() +
      geom_line(data = data, aes(x = data[, 1], y = data[, 2]),size = 1) +
      geom_line(data = data2, aes(x = data2[,1], y = data2[,2]), color = "darkred", size = 1, alpha = 0.3) +
      labs(title = "Light spectrum x First detector", y = "Response", x = "Wavelength (nm)")
  })
  
  output$plot_detector2_right <- renderPlot({
    data <- detectorBxlamp()
    data2 <- divided_light_spectrum()
    
    ggplot() +
      geom_line(data = data, aes(x = data[, 1], y = data[, 2]),size = 1) +
      geom_line(data = data2, aes(x = data2[,1], y = data2[,2]), color = "darkred",size = 1, alpha = 0.3) +
      labs(title = "Light spectrum x Second detector", y = "Response", x = "Wavelength (nm)")
  })
  
  # Reactive sentence to show light conversions
  output$reactive_sentence <- renderText({
    # Check if the checkbox is checked
    if (input$power_units_checkbox) {
      unit <- " µmoles of photons/m²/s (µE)"
    }
    else{
      unit <- " W/m²"
    }
    
    
    
    LampAUC <- lamp_AUC()
    Detector1AUC <- detectorA_AUC()
    Detector2AUC <- detectorB_AUC()
    
    ratio1 <- round(LampAUC/Detector1AUC,2)
    ratio2 <- round(Detector2AUC/Detector1AUC,2)
    
    input_value <- input$input_value
    sentence <- paste("If the first detector shows", input_value, unit,", then the true spectrum contains", input_value * ratio1, unit, " and the second detector must show", input_value * ratio2, unit," to have the same amount of light.")
    HTML(sentence)
  })
}


shinyApp(ui, server)
