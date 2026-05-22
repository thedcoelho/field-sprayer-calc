library(shiny)
library(bslib)

convert_to_gallons <- function(rate, unit){
  if(is.na(rate) || rate == 0) return(0)
  if(unit == "fl oz/ac") return(rate / 128)
  if(unit == "pt/ac") return(rate / 8)
  if(unit == "qt/ac") return(rate / 4)
  if(unit == "gal/ac") return(rate)
  return(0)
}

ui <- page_navbar(
  
  title = "🌱 Sprayer Calibration & Tank Mix Calculator",
  
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E7D32"
  ),
  
  nav_panel(
    "Sprayer Setup",
    
    layout_sidebar(
      
      sidebar = sidebar(
        
        h4("Boom Setup"),
        numericInput("nozzles", "Number of nozzles", 12, min = 1),
        numericInput("spacing", "Nozzle spacing (inches)", 20, min = 1),
        numericInput("boom_height", "Boom height (inches)", 20, min = 1),
        
        hr(),
        
        h4("Field Information"),
        numericInput("area_length", "Field/plot length (ft)", 500, min = 1),
        numericInput("area_width", "Field/plot width (ft)", 200, min = 1),
        
        hr(),
        
        h4("Application"),
        numericInput("gpa", "Target GPA", 15, min = 1),
        numericInput("tank_size", "Tank size (gallons)", 25, min = 1),
        selectInput("speed", "Travel Speed (MPH)", c(3:10), selected = 5),
        
        hr(),
        
        h4("Calibration"),
        numericInput("collected_oz", "Collected ounces from ONE nozzle", value = NA)
      ),
      
      layout_columns(
        value_box("Boom Width", textOutput("boom_width_box"), showcase = "🌾", theme = "primary"),
        value_box("Field Area", textOutput("area_box"), showcase = "📐", theme = "info"),
        value_box("Total Spray", textOutput("spray_box"), showcase = "💧", theme = "success"),
        value_box("Water Needed", textOutput("water_box"), showcase = "🚰", theme = "warning")
      ),
      
      layout_columns(
        card(
          card_header("🚜 Calibration"),
          h3(textOutput("calibration_main")),
          p("Collect spray from ONE nozzle during this time."),
          hr(),
          verbatimTextOutput("calibration_details")
        ),
        
        card(
          card_header("📋 Sprayer Summary"),
          verbatimTextOutput("sprayer_summary")
        )
      )
    )
  ),
  
  nav_panel(
    "Tank Mix",
    
    layout_sidebar(
      
      sidebar = sidebar(
        
        h4("Product 1"),
        textInput("prod1", "Product 1 name", ""),
        numericInput("rate1", "Rate Product 1", 0, min = 0),
        selectInput("unit1", "Unit Product 1", c("fl oz/ac", "pt/ac", "qt/ac", "gal/ac")),
        
        hr(),
        
        h4("Product 2"),
        textInput("prod2", "Product 2 name", ""),
        numericInput("rate2", "Rate Product 2", 0, min = 0),
        selectInput("unit2", "Unit Product 2", c("fl oz/ac", "pt/ac", "qt/ac", "gal/ac")),
        
        hr(),
        
        h4("Product 3"),
        textInput("prod3", "Product 3 name", ""),
        numericInput("rate3", "Rate Product 3", 0, min = 0),
        selectInput("unit3", "Unit Product 3", c("fl oz/ac", "pt/ac", "qt/ac", "gal/ac")),
        
        hr(),
        
        h4("Product 4"),
        textInput("prod4", "Product 4 name", ""),
        numericInput("rate4", "Rate Product 4", 0, min = 0),
        selectInput("unit4", "Unit Product 4", c("fl oz/ac", "pt/ac", "qt/ac", "gal/ac")),
        
        hr(),
        
        h4("Product 5"),
        textInput("prod5", "Product 5 name", ""),
        numericInput("rate5", "Rate Product 5", 0, min = 0),
        selectInput("unit5", "Unit Product 5", c("fl oz/ac", "pt/ac", "qt/ac", "gal/ac"))
      ),
      
      layout_columns(
        value_box("Tank Covers", textOutput("tank_acres_box"), showcase = "🚜", theme = "primary"),
        value_box("Product Volume", textOutput("product_volume_box"), showcase = "🧪", theme = "info"),
        value_box("Water Needed", textOutput("water_box2"), showcase = "🚰", theme = "warning"),
        value_box("Tanks Needed", textOutput("tanks_needed_box"), showcase = "🔁", theme = "success")
      ),
      
      card(
        card_header("🧪 Tank Mix Results"),
        h3(textOutput("tank_main")),
        hr(),
        verbatimTextOutput("tank_details")
      )
    )
  ),
  
  nav_panel(
    "Full Report",
    
    card(
      card_header("📋 Full Report"),
      verbatimTextOutput("full_report")
    )
  )
)

server <- function(input, output) {
  
  calc <- reactive({
    
    boom_width_ft <- (input$nozzles * input$spacing) / 12
    
    area_sqft <- input$area_length * input$area_width
    acres <- area_sqft / 43560
    
    total_gallons <- acres * input$gpa
    tanks_needed <- total_gallons / input$tank_size
    
    calibration_distance <- 4080 / input$spacing
    speed_ft_sec <- as.numeric(input$speed) * 1.467
    travel_time <- calibration_distance / speed_ft_sec
    
    actual_gpa <- input$collected_oz
    
    nozzle_gpm <- ifelse(
      is.na(actual_gpa),
      NA,
      (actual_gpa * as.numeric(input$speed) * input$spacing) / 5940
    )
    
    tank_acres <- input$tank_size / input$gpa
    
    prod1_total <- convert_to_gallons(input$rate1, input$unit1) * tank_acres
    prod2_total <- convert_to_gallons(input$rate2, input$unit2) * tank_acres
    prod3_total <- convert_to_gallons(input$rate3, input$unit3) * tank_acres
    prod4_total <- convert_to_gallons(input$rate4, input$unit4) * tank_acres
    prod5_total <- convert_to_gallons(input$rate5, input$unit5) * tank_acres
    
    total_product_volume <- prod1_total + prod2_total + prod3_total + prod4_total + prod5_total
    
    water_needed <- input$tank_size - total_product_volume
    
    list(
      boom_width_ft = boom_width_ft,
      area_sqft = area_sqft,
      acres = acres,
      total_gallons = total_gallons,
      tanks_needed = tanks_needed,
      calibration_distance = calibration_distance,
      travel_time = travel_time,
      actual_gpa = actual_gpa,
      nozzle_gpm = nozzle_gpm,
      tank_acres = tank_acres,
      prod1_total = prod1_total,
      prod2_total = prod2_total,
      prod3_total = prod3_total,
      prod4_total = prod4_total,
      prod5_total = prod5_total,
      total_product_volume = total_product_volume,
      water_needed = water_needed
    )
  })
  
  output$boom_width_box <- renderText({
    paste0(round(calc()$boom_width_ft, 1), " ft")
  })
  
  output$area_box <- renderText({
    paste0(round(calc()$acres, 3), " ac")
  })
  
  output$spray_box <- renderText({
    paste0(round(calc()$total_gallons, 2), " gal")
  })
  
  output$water_box <- renderText({
    paste0(round(calc()$water_needed, 2), " gal")
  })
  
  output$water_box2 <- renderText({
    paste0(round(calc()$water_needed, 2), " gal")
  })
  
  output$tank_acres_box <- renderText({
    paste0(round(calc()$tank_acres, 2), " ac")
  })
  
  output$product_volume_box <- renderText({
    paste0(round(calc()$total_product_volume, 3), " gal")
  })
  
  output$tanks_needed_box <- renderText({
    paste0(round(calc()$tanks_needed, 2))
  })
  
  output$calibration_main <- renderText({
    paste0(
      "Drive ",
      round(calc()$calibration_distance, 1),
      " ft in ",
      round(calc()$travel_time, 1),
      " seconds"
    )
  })
  
  output$calibration_details <- renderText({
    if(is.na(calc()$actual_gpa)){
      paste0(
        "Travel Speed: ", input$speed, " MPH\n",
        "Calibration Distance: ", round(calc()$calibration_distance, 1), " ft\n",
        "Required Time: ", round(calc()$travel_time, 1), " seconds\n\n",
        "Enter collected ounces after calibration."
      )
    } else {
      paste0(
        "Collected Volume: ", round(calc()$actual_gpa, 2), " oz\n",
        "Estimated Actual GPA: ", round(calc()$actual_gpa, 2), "\n",
        "Estimated Nozzle Flow: ", round(calc()$nozzle_gpm, 3), " GPM"
      )
    }
  })
  
  output$sprayer_summary <- renderText({
    paste0(
      "Boom width: ", round(calc()$boom_width_ft, 2), " ft\n",
      "Area: ", round(calc()$area_sqft, 2), " ft²\n",
      "Area: ", round(calc()$acres, 3), " acres\n",
      "Target GPA: ", input$gpa, "\n",
      "Total spray needed: ", round(calc()$total_gallons, 2), " gal\n",
      "Tank size: ", input$tank_size, " gal"
    )
  })
  
  output$tank_main <- renderText({
    paste0("Tank covers ", round(calc()$tank_acres, 2), " acres")
  })
  
  output$tank_details <- renderText({
    paste0(
      ifelse(input$prod1 == "", "Product 1", input$prod1), ": ",
      round(calc()$prod1_total, 3), " gal/tank\n",
      
      ifelse(input$prod2 == "", "Product 2", input$prod2), ": ",
      round(calc()$prod2_total, 3), " gal/tank\n",
      
      ifelse(input$prod3 == "", "Product 3", input$prod3), ": ",
      round(calc()$prod3_total, 3), " gal/tank\n",
      
      ifelse(input$prod4 == "", "Product 4", input$prod4), ": ",
      round(calc()$prod4_total, 3), " gal/tank\n",
      
      ifelse(input$prod5 == "", "Product 5", input$prod5), ": ",
      round(calc()$prod5_total, 3), " gal/tank\n\n",
      
      "Total Product Volume: ",
      round(calc()$total_product_volume, 3), " gallons\n",
      
      "Add Water: ",
      round(calc()$water_needed, 3), " gallons"
    )
  })
  
  output$full_report <- renderText({
    paste0(
      "BOOM INFORMATION\n",
      "Boom Width: ", round(calc()$boom_width_ft, 2), " ft\n",
      "Boom Height: ", input$boom_height, " inches\n\n",
      
      "FIELD INFORMATION\n",
      "Area: ", round(calc()$area_sqft, 2), " ft²\n",
      "Area: ", round(calc()$acres, 3), " acres\n\n",
      
      "SPRAY APPLICATION\n",
      "Target GPA: ", input$gpa, "\n",
      "Total Spray Needed: ", round(calc()$total_gallons, 2), " gallons\n",
      "Tank Size: ", input$tank_size, " gallons\n",
      "Estimated Tanks Needed: ", round(calc()$tanks_needed, 2), "\n\n",
      
      "CALIBRATION\n",
      "Travel Speed: ", input$speed, " MPH\n",
      "Calibration Distance: ", round(calc()$calibration_distance, 1), " ft\n",
      "Travel Time: ", round(calc()$travel_time, 1), " seconds\n\n",
      
      "TANK MIX\n",
      "Tank Covers: ", round(calc()$tank_acres, 2), " acres\n",
      "Total Product Volume: ", round(calc()$total_product_volume, 3), " gallons\n",
      "Water Needed: ", round(calc()$water_needed, 3), " gallons\n"
    )
  })
}

shinyApp(ui, server)

