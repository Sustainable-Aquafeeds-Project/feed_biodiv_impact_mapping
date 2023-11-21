library(tidyverse)
library(here)
library(data.table)
library(terra)
library(rnaturalearth)
library(sf)
library(khroma)
library(scales)
library(fields)
library(patchwork)
library(cowplot)
library(glue)

source(here("src/directories.R"))
source(here("src/spatial.R"))

countries_shp <- ne_countries(scale = 110, returnclass = "sf") |> st_transform(crs(moll_template))
YlOrBr <- color("YlOrBr")

d_brown <- "#515256"
m_brown <- "#B27B54"
l_brown <- "#BC995F"
green <- "#A8B072"
yellow<- "#EFCE71"
l_yellow <- "#F7F6C1"

jv_pal <- c(l_yellow, yellow, green, l_brown, m_brown, d_brown)

continuous_pal <-  colorRampPalette(jv_pal, space="Lab", bias = 3.5)(10000)

light_gray <- "#F8F9FA"
final_palette <- c(light_gray, continuous_pal)

palette_red <- c(final_palette, "#B90000")

light_gray <- "#F8F9FA"
final_palette <- c(light_gray, continuous_pal)

# Define your file paths
impact_maps_dir <- file.path(rdsi_dir, "biodiversity_impacts/output")

# Define your global variables
allocations <- c("economic", "ge", "mass")
calc_types <- c("mean", "sd", "nspp")
taxa_types <- c('sponges','arthropods','fish','cephalopods','corals','molluscs','Reptiles and amphibians','echinoderms','Marine plant','elasmobranchs','Marine mammal','Bird','Terrestrial mammal')
ingredient_types <- c('forage fish_fish meal','forage fish_fish oil','trimmings fish_fish meal, cut offs','trimmings fish_fish oil, cut offs','Maize_corn gluten meal','Pulses_faba beans','Soybean_soybean meal','Wheat_wheat','Wheat_wheat gluten','CropsNES_coconut oil','CropsNES_linseed oil','Pulses_guar meal','Pulses_pea flour','Pulses_pea protein concentrate','Rapeseed_canola oil','Soybean_soy oil','Soybean_soy protein concentrate','Sunflower_sunflower meal')
mat_types <- c('forage fish', 'trimmings fish', "Maize", "Pulses", "Soybean", "Wheat", "CropsNES", "Rapeseed", "Sunflower")
plot_types <- c("delta", "normal")

# Create the Shiny UI
ui <- fluidPage(
  navbarPage(
    "Biodiversity Impact Maps",
    tabPanel("Global Maps", 
             selectInput("allocation_type", "Allocation Type", choices = allocations),
             selectInput("calc_type", "Calculation Type", choices = calc_types),
             selectInput("plot_type", "Plot type", choices = plot_types),
             plotOutput("global_map")
    ),
    tabPanel("Maps by Taxa",    
             selectInput("allocation_type_2", "Allocation Type", choices = allocations),
             selectInput("calc_type_2", "Calculation Type", choices = calc_types),
             selectInput("taxa_type_2", "Taxon", choices = taxa_types),
             selectInput("plot_type_2", "Plot type", choices = plot_types),
             plotOutput("taxa_map")),
    tabPanel("Maps by Ingredient", 
             selectInput("allocation_type_3", "Allocation Type", choices = allocations),
             selectInput("calc_type_3", "Calculation Type", choices = calc_types),
             selectInput("ingredient_type_3", "Ingredient type", choices = ingredient_types),
             selectInput("plot_type_3", "Plot type", choices = plot_types),
             plotOutput("ingredient_map")),
    tabPanel("Maps by Raw Material", 
             selectInput("allocation_type_4", "Allocation Type", choices = allocations),
             selectInput("calc_type_4", "Calculation Type", choices = calc_types),
             selectInput("raw_material_type", "Raw Material type", choices = mat_types),
             selectInput("plot_type_4", "Plot type", choices = plot_types),
             plotOutput("material_map"))
  )
)

# Create the Shiny server
server <- function(input, output) {
  # Function to generate global maps
  generateGlobalMap <- function(allocation_type, calc_type, plot_type) {
    # Your code for global maps generation

    # allocation_type = "economic"
    # calc_type = "mean"
    
    if(plot_type == "normal"){
    
    all_df <- fread(glue(file.path(impact_maps_dir, "csvs/impact_maps_across_taxon_ingredient/{allocation_type}_{calc_type}.rds.gz")))
    
       ggplot() +
      geom_tile(data = all_df, aes(x= long, y = lat, fill = value)) +
      facet_grid(fcr_type~diet) +
         # scale_fill_gradientn(colors = YlOrBr(9), na.value = "white") +  # Set the color for missing values (NA) to white
         scale_fill_gradientn(colors = final_palette, na.value = "white") +
         #  scale_fill_viridis(direction = -1) + 
         geom_sf(data = countries_shp, fill = NA, colour = "grey") + 
         theme_minimal() +
         theme(
           axis.title = element_blank(),
           axis.text = element_blank(),
           axis.ticks = element_blank()) 
     # labs(title = glue("Allocation type: {allocation_type}; Impact calculation: {calc_type}"))
    }else{
  
      delta_df <- fread(glue(file.path(impact_maps_dir, "csvs/impact_maps_across_taxon_ingredient/delta/{allocation_type}_{calc_type}.rds.gz")))
      
      if(str_detect(calc_type, "nspp")){
        limit <- c(min(delta_df$delta), max(delta_df$delta))
        
      }else{
        limit <- c(quantile(delta_df$delta, 0.05), quantile(delta_df$delta, 0.9))
        
      }
      
      ggplot() + 
        geom_tile(data = delta_df %>% filter(delta != 0) , aes(x= long, y = lat, fill = delta)) +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,  limit = limit, na.value = "white", oob=scales::squish) + # this will make sure anything above the scales limit will be a solid color
        geom_sf(data = countries_shp, fill = NA, colour = "grey") + 
        facet_wrap(~fcr_type) + 
        theme_minimal() +
        theme(
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
      
}
    
  } 
    
  
  # Function to generate maps by taxa
  generateTaxaMap <- function(allocation_type_2, calc_type_2, taxa_type_2, plot_type_2) {
    # Your code for maps by taxa generation

    
     # allocation_type = "economic"
     #  taxa_type = "sponges"
     # calc_type = "mean"
    
    
    if(plot_type_2 == "normal"){
    all_df <- fread(glue(file.path(impact_maps_dir, "csvs/impact_maps_by_taxon_across_ingredient/{allocation_type_2}_{taxa_type_2}_{calc_type_2}.rds.gz")))


    ## create maps in separate plots, force common scale between them
    
    ggplot() + 
      geom_tile(data = all_df, aes(x= long, y = lat, fill = value)) + 
      facet_grid(fcr_type~diet) + 
      # scale_fill_gradientn(colors = YlOrBr(9), na.value = "white") +  # Set the color for missing values (NA) to white
      scale_fill_gradientn(colors = final_palette, na.value = "white") +
      #  scale_fill_viridis(direction = -1) + 
      geom_sf(data = countries_shp, fill = NA, colour = "grey") + 
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
    }else{
      
      
      delta_df <- fread(glue(file.path(impact_maps_dir, "csvs/impact_maps_by_taxon_across_ingredient/delta/{allocation_type_2}_{taxa_type_2}_{calc_type_2}.rds.gz")))
      
      
      if(str_detect(calc_type_2, "nspp")){
        limit <- c(min(delta_df$delta), max(delta_df$delta))
        
      }else{
        limit <- c(quantile(delta_df$delta, 0.05), quantile(delta_df$delta, 0.9))
      }
      
      ## create maps in separate plots, force common scale between them
      ggplot() + 
        geom_tile(data = delta_df %>% filter(delta != 0) , aes(x= long, y = lat, fill = delta)) +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,  limit = limit, na.value = "white", oob=scales::squish) + # squish makes sure anything above the scales limit will be a solid red or blue... better to visualize this way since the numbers are so small
        geom_sf(data = countries_shp, fill = NA, colour = "grey") + 
        facet_wrap(~fcr_type) + 
        theme_minimal() +
        theme(
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
    }
    
    
  }
  
  # Function to generate maps by ingredient
  generateIngredientMap <- function(allocation_type_3, calc_type_3, ingredient_type_3, plot_type_3) {
    # Your code for maps by ingredient generation

    # ingredient_type_3 = "Soybean_soybean meal"
    # allocation_type_3 = "economic"
    # calc_type_3 = "mean"
    # plot_type_3 = "delta"
    
    if(plot_type_3 == "normal"){
    all_df <- fread(glue(file.path(impact_maps_dir, "csvs/impact_maps_across_taxon_by_ingredient/{allocation_type_3}_{ingredient_type_3}_{calc_type_3}.rds.gz")))
    

    ## create maps in separate plots, force common scale between them
    
    ggplot() + 
      geom_tile(data = all_df, aes(x= long, y = lat, fill = value)) + 
      facet_grid(fcr_type~diet) + 
      # scale_fill_gradientn(colors = YlOrBr(9), na.value = "white") +  # Set the color for missing values (NA) to white
   scale_fill_gradientn(colors = final_palette, na.value = "white") +
   #  scale_fill_viridis(direction = -1) + 
              geom_sf(data = countries_shp, fill = NA, colour = "grey") + 
  theme_minimal() +
      theme(
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
    
    }else{
      delta_df <- fread(glue(file.path(impact_maps_dir, "csvs/impact_maps_across_taxon_by_ingredient/delta/{allocation_type_3}_{ingredient_type_3}_{calc_type_3}.rds.gz")))
     
       if(str_detect(calc_type_3, "nspp")){
        limit <- c(min(delta_df$delta), max(delta_df$delta))
        
      }else{
        limit <- c(quantile(delta_df$delta, 0.05), quantile(delta_df$delta, 0.9))
      }
      
      ## create maps in separate plots, force common scale between them
      ggplot() + 
        geom_tile(data = delta_df %>% filter(delta != 0) , aes(x= long, y = lat, fill = delta)) +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,  limit = limit, na.value = "white", oob=scales::squish) +
        geom_sf(data = countries_shp, fill = NA, colour = "grey") + 
        facet_wrap(~fcr_type) + 
        theme_minimal() +
        theme(
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
      
    }
          
  }
  
  # Function to generate maps by raw material
  generateMaterialMap <- function(allocation_type_4, calc_type_4, raw_material_type, plot_type_4) {
    # Your code for maps by ingredient generation

    # raw_material_type = "Maize"
    # allocation_type_4 = "economic"
    # calc_type_4 = "mean"
    # plot_type_4 = "delta"

    
    if(plot_type_4 == "normal"){
    
    all_df <- fread(glue(file.path(impact_maps_dir, "csvs/impact_maps_across_taxon_by_raw_material/{allocation_type_4}_{raw_material_type}_{calc_type_4}.rds.gz")))
    
    
    ## create maps in separate plots, force common scale between them
    
    ggplot() + 
      geom_tile(data = all_df, aes(x= long, y = lat, fill = value)) + 
      facet_grid(fcr_type~diet) + 
      # scale_fill_gradientn(colors = YlOrBr(9), na.value = "white") +  # Set the color for missing values (NA) to white
      scale_fill_gradientn(colors = final_palette, na.value = "white") +
      #  scale_fill_viridis(direction = -1) + 
      geom_sf(data = countries_shp, fill = NA, colour = "grey") + 
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
    }else{
      
      delta_df <- fread(glue(file.path(impact_maps_dir, "csvs/impact_maps_across_taxon_by_raw_material/delta/{allocation_type_4}_{raw_material_type}_{calc_type_4}.rds.gz"))) %>%
        filter(!is.na(delta))
      
      if(str_detect(calc_type_4, "nspp")){
        limit <- c(min(delta_df$delta), max(delta_df$delta))
        
      }else{
        limit <- c(quantile(delta_df$delta, 0.05), quantile(delta_df$delta, 0.9))
      }
      
      ## create maps in separate plots, force common scale between them
      ggplot() + 
        geom_tile(data = delta_df %>% filter(delta != 0) , aes(x= long, y = lat, fill = delta)) +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,  limit = limit, na.value = "white", oob=scales::squish) +
        geom_sf(data = countries_shp, fill = NA, colour = "grey") + 
        facet_wrap(~fcr_type) + 
        theme_minimal() +
        theme(
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) 
      
    }
  }
  
  # Render global map
  output$global_map <- renderPlot({
    allocation_type <- input$allocation_type
    calc_type <- input$calc_type
    plot_type <- input$plot_type
    generateGlobalMap(allocation_type, calc_type, plot_type)
  })
  
  # Render maps by taxa
  output$taxa_map <- renderPlot({
    allocation_type <- input$allocation_type_2
    calc_type <- input$calc_type_2
    taxa_type <- input$taxa_type_2
    plot_type <- input$plot_type_2
    generateTaxaMap(allocation_type, calc_type, taxa_type, plot_type)
  })
  
  # Render maps by ingredient
  output$ingredient_map <- renderPlot({
    allocation_type <- input$allocation_type_3
    calc_type <- input$calc_type_3
    ingredient_type <- input$ingredient_type_3
    plot_type <- input$plot_type_3
    generateIngredientMap(allocation_type, calc_type, ingredient_type, plot_type)
  })
  
  # Render maps by raw material
  output$material_map <- renderPlot({
    allocation_type <- input$allocation_type_4
    calc_type <- input$calc_type_4
    raw_material_type <- input$raw_material_type    
    plot_type <- input$plot_type_4
    generateMaterialMap(allocation_type, calc_type, raw_material_type, plot_type)
  })
}

# Run the Shiny app
shinyApp(ui, server)
