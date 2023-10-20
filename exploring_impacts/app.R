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
image(volcano, asp=1, col=continuous_pal)

light_gray <- "#F8F9FA"
final_palette <- c(light_gray, continuous_pal)
image(volcano, asp=1, col=final_palette)

palette_red <- c(final_palette, "#B90000")
image(volcano, asp=1, col=palette_red)

# jv_pal_new <- jv_pal[7000:length(continuous_pal)]
# image(volcano, asp=1, col=jv_pal_new)

light_gray <- "#F8F9FA"
final_palette <- c(light_gray, continuous_pal)
scales::show_col(final_palette)

# Define your file paths
impact_maps_dir <- file.path(rdsi_dir, "biodiversity_impacts/output")

# Define your global variables
allocations <- c("economic", "ge", "mass")
calc_types <- c("mean", "sd", "nspp")
taxa_types <- c('sponges','arthropods','fish','cephalopods','corals','molluscs','Reptiles and amphibians','echinoderms','Marine plant','elasmobranchs','Marine mammal','Bird','Terrestrial mammal')
ingredient_types <- c('forage fish_fish meal','forage fish_fish oil','trimmings fish_fish meal, cut offs','trimmings fish_fish oil, cut offs','Maize_corn gluten meal','Pulses_faba beans','Soybean_soybean meal','Wheat_wheat','Wheat_wheat gluten','CropsNES_coconut oil','CropsNES_linseed oil','Pulses_guar meal','Pulses_pea flour','Pulses_pea protein concentrate','Rapeseed_canola oil','Soybean_soy oil','Soybean_soy protein concentrate','Sunflower_sunflower meal')

tabs <- c(
  "impact_maps_across_taxon_ingredient",
  "impact_maps_across_taxon_by_ingredient",
  "impact_maps_by_taxon_across_ingredient"
)

# Create the Shiny UI
ui <- fluidPage(
  navbarPage(
    "Biodiversity Impact Maps",
    tabPanel("Global Maps", 
             selectInput("allocation_type", "Allocation Type", choices = allocations),
             selectInput("calc_type", "Calculation Type", choices = calc_types),
             plotOutput("global_map")
    ),
    tabPanel("Maps by Taxa",    
             selectInput("allocation_type_2", "Allocation Type", choices = allocations),
             selectInput("calc_type_2", "Calculation Type", choices = calc_types),
             selectInput("taxa_type_2", "Taxon", choices = taxa_types),
             plotOutput("taxa_map")),
    tabPanel("Maps by Ingredient", 
             selectInput("allocation_type_3", "Allocation Type", choices = allocations),
             selectInput("calc_type_3", "Calculation Type", choices = calc_types),
             selectInput("ingredient_type_3", "Ingredient type", choices = ingredient_types),
             plotOutput("ingredient_map"))
  )
)

# Create the Shiny server
server <- function(input, output) {
  # Function to generate global maps
  generateGlobalMap <- function(allocation_type, calc_type) {
    # Your code for global maps generation

    # allocation_type = "economic"
    # calc_type = "mean"
    
    all_df <- fread(glue(file.path(impact_maps_dir, "csvs/impact_maps_across_taxon_ingredient/{allocation_type}_{calc_type}.rds.gz")))
    
       ggplot() +
      geom_tile(data = all_df, aes(x= long, y = lat, fill = value)) +
      facet_wrap(~diet) +
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
     
     # plant <- ggplot() +
     #   geom_sf(data = countries_shp, colour = "white") +
     #   geom_raster(data = all_df %>% filter(diet == "plant-dominant"), aes(x= long, y = lat, fill = value)) +
     #   scale_fill_gradientn(
     #     colors = YlOrBr(8),  # Exclude "grey" from the color palette
     #     limits = c(0, max(all_df$value)  # Set the limits to include values from 0 to the maximum value
     #     )) +
     #   theme_minimal() +
     #   theme(
     #     axis.title = element_blank(),
     #     axis.text = element_blank(),
     #     axis.ticks = element_blank()) +
     #   labs(title = glue("Allocation type: {allocation_type}; Impact calculation: {calc_type}; Feed formulation: plant dominant"))
     # 
     # fish <- ggplot() +
     #   geom_sf(data = countries_shp, colour = "white") +
     #   geom_raster(data = all_df %>% filter(diet == "fish-dominant"), aes(x= long, y = lat, fill = value)) +
     #   scale_fill_gradientn(
     #     colors = YlOrBr(8),  # Exclude "grey" from the color palette
     #     limits = c(0, max(all_df$value)  # Set the limits to include values from 0 to the maximum value
     #     )) +
     #   theme_minimal() +
     #   theme(
     #     axis.title = element_blank(),
     #     axis.text = element_blank(),
     #     axis.ticks = element_blank()) +
     #   labs(title = glue("Allocation type: {allocation_type}; Impact calculation: {calc_type}; Feed formulation: fish dominant"))
     # 
     # plant+fish
    
  } 
    
  
  # Function to generate maps by taxa
  generateTaxaMap <- function(allocation_type_2, calc_type_2, taxa_type_2) {
    # Your code for maps by taxa generation

    
     # allocation_type = "economic"
     #  taxa_type = "sponges"
     # calc_type = "mean"
    
    all_df <- fread(glue(file.path(impact_maps_dir, "csvs/impact_maps_by_taxon_across_ingredient/{allocation_type_2}_{taxa_type_2}_{calc_type_2}.rds.gz")))


    ## create maps in separate plots, force common scale between them
    
    ggplot() + 
      geom_tile(data = all_df, aes(x= long, y = lat, fill = value)) + 
      facet_wrap(~diet) + 
      # scale_fill_gradientn(colors = YlOrBr(9), na.value = "white") +  # Set the color for missing values (NA) to white
      scale_fill_gradientn(colors = final_palette, na.value = "white") +
      #  scale_fill_viridis(direction = -1) + 
      geom_sf(data = countries_shp, fill = NA, colour = "grey") + 
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
  
    
    
  }
  
  # Function to generate maps by ingredient
  generateIngredientMap <- function(allocation_type_3, calc_type_3, ingredient_type_3) {
    # Your code for maps by ingredient generation

    # ingredient_type = "Soybean_soybean meal"
    # allocation_type = "economic"
    # calc_type = "mean"
    
    
    all_df <- fread(glue(file.path(impact_maps_dir, "csvs/impact_maps_across_taxon_by_ingredient/{allocation_type_3}_{ingredient_type_3}_{calc_type_3}.rds.gz")))
    

    ## create maps in separate plots, force common scale between them
    
    ggplot() + 
      geom_tile(data = all_df, aes(x= long, y = lat, fill = value)) + 
      facet_wrap(~diet) + 
      # scale_fill_gradientn(colors = YlOrBr(9), na.value = "white") +  # Set the color for missing values (NA) to white
   scale_fill_gradientn(colors = final_palette, na.value = "white") +
   #  scale_fill_viridis(direction = -1) + 
              geom_sf(data = countries_shp, fill = NA, colour = "grey") + 
  theme_minimal() +
      theme(
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
          
  }
  
  # Render global map
  output$global_map <- renderPlot({
    allocation_type <- input$allocation_type
    calc_type <- input$calc_type
    generateGlobalMap(allocation_type, calc_type)
  })
  
  # Render maps by taxa
  output$taxa_map <- renderPlot({
    allocation_type <- input$allocation_type_2
    calc_type <- input$calc_type_2
    taxa_type <- input$taxa_type_2
    generateTaxaMap(allocation_type, calc_type, taxa_type)
  })
  
  # Render maps by ingredient
  output$ingredient_map <- renderPlot({
    allocation_type <- input$allocation_type_3
    calc_type <- input$calc_type_3
    ingredient_type <- input$ingredient_type_3
    generateIngredientMap(allocation_type, calc_type, ingredient_type)
  })
}

# Run the Shiny app
shinyApp(ui, server)
