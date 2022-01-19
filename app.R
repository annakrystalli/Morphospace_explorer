library(RColorBrewer) # # # Install these packages
library(shiny)
library(geomorph)
library(ggvis)
library(dplyr)
library(rgl)

options(rgl.printRglwidget = TRUE)

source("R/morphospace_plot.R")
source("R/3d_beak.R")
source("R/taxo_select.R")

# Plotting pars:
so <- readRDS("data/PlottingPars_SideOn.rds")
td <- readRDS("data/PlottingPars_TopDown.rds")

# Read in data:
coords <- readRDS("data/Filtered_spp_gpa_2015_11_13.rds")$median.symm.coords
sliders <- read.csv("data/sliders.csv", header = F)
scores <- read.csv("data/scores.csv", strings = F)

# create taxonomic subset select choices
familyNames <- sort(scores$Family)
genusNames <- sort(scores$Genus)
speciesNames <- sort(setNames(scores$Species, gsub("_", " ", scores$Species))) # %>% as.list()

# process coordinate data ready for 3d plotting
data_3d <- data_3d_get(coords)

# Plotting cols:
#cols <- c(brewer.pal(9, name="Set1")[9], brewer.pal(9, name="Set1")[4], brewer.pal(9, name="Set1")[2], brewer.pal(9, name="Set1")[1], brewer.pal(9, name="Set1")[5], brewer.pal(9, name="Set1")[3])
cols <- c(RColorBrewer::brewer.pal(9, name="Blues"))

# colors
#fill_pal <- setNames(MetBrewer::met.brewer(name="Veronese", n = 20)[c(13, 20)], c("family", "genus"))
fill_pal <- setNames(MetBrewer::met.brewer(name="Signac", n = 20)[c(5, 15)], c("family", "genus"))
fill_grey <- RColorBrewer::brewer.pal(9, name="Set1")[9]

stroke_pal <- MetBrewer::met.brewer(name="Peru1", n = 8)[c(1, 4, 3, 7, 6, 2, 5, 8)]
stroke_grey <- RColorBrewer::brewer.pal(8, name="Dark2")[8]




# Create reative values object:
values <- reactiveValues()
values$clickspp <- NULL
values$last_selection <- NULL
values$selected <- NULL
values$plot3d <- NULL
values$plot_v_axis <- NULL
values$plot_h_axis <- NULL
values$plot_centre <- NULL
values$highlight_fam <- NULL
values$highlight_gen <- NULL


clickFunc <- function(x) {
    if(is.null(x)) return(NULL)
    values$clickspp <- x$Spp
    paste0("<b>", scores$English[scores$Spp==x$Spp], "</b><br><i>", 
           gsub("_"," ",x$Spp), "</i><br><a href='https://www.google.com/search?q=", 
           gsub("_"," ",x$Spp), "' target='_blank'>Search this species</a>") # Add tbm=isch& for image search
}
############################################################

##########
# SERVER #
##########

server <- function(input, output, clientData, session) {
    
    observe({
        updateSliderInput(session, "xval", label = "X value:", 
                          min=round(min(scores[,input$xaxis]),2), 
                          max=round(max(scores[,input$xaxis]),2), value=0)
        updateSliderInput(session, "yval", label = "Y value:", 
                          min=round(min(scores[,input$yaxis]),2), 
                          max=round(max(scores[,input$yaxis]),2), value=0)
    })
    
    
    observeEvent(input$fam1, {
        
        values$highlight_fam <- if(input$fam1 == "Search here..."){NULL}else{input$fam1}
        values$highlight_gen <- NULL
        
        updateSelectInput(session, "gen1", label = "Select genus", 
                          c("Search here...", 
                            get_genus_options(scores, values$highlight_fam)), 
                          "Search here...")
        updateSelectInput(session, "spp1", label = "Select species", 
                          c("Search here...", 
                            get_spp_options(scores, values$highlight_fam, values$highlight_gen)), 
                          "Search here...")
    }, ignoreInit = TRUE)
    
    observeEvent(input$gen1, {
        values$highlight_gen <- if(input$gen1 == "Search here..."){NULL}else{input$gen1}
        updateSelectInput(session, "spp1", label="Select species", 
                          c("Search here...", 
                            get_spp_options(scores, req(values$highlight_fam), 
                                            req(values$highlight_gen))), 
                          "Search here...")
    }, ignoreInit = TRUE)
    
    observeEvent(c(input$spp1, values$clickspp), {
        species_inputs <- c(input$spp1[input$spp1 != "Search here..."], 
                            values$clickspp)
        
        if(length(species_inputs) > 0){
            select_diff <- species_inputs[!species_inputs %in% values$selected]
            if(length(select_diff) > 0){
                values$last_selection <- select_diff
                values$selected <- c(values$selected, values$last_selection)
            }
        }
    })
    
    # observeEvent(c(input$xval, input$yval, input$xaxis, input$yaxis) , {
    #     values$plot_v_axis <- data.frame(X = c(input$xval, input$xval), 
    #                                      Y = c(min(scores[,input$yaxis]), max(scores[,input$yaxis])))
    #     values$plot_h_axis <- data.frame(X = c(min(scores[,input$xaxis]), max(scores[,input$xaxis])), 
    #                                      Y = c(input$yval, input$yval))
    #     values$plot_centre <- data.frame(X = c(input$xval),Y = c(input$yval))
    # }, ignoreNULL = FALSE)
    # 
    open_3d_ref_beak <- eventReactive(c(input$xval, input$yval), {
        
        rgl::close3d()
        rgl::open3d(useNULL = TRUE)
        rgl::par3d(so)
        
        plot_ref_beak(data_3d, xaxis = input$xaxis, yaxis = input$yaxis, xval = input$xval, 
                      yval = input$yval, sliders = sliders, colour = "black")
        
    })
    
    re_add_selected_3d_beaks <- eventReactive(c(input$xval, input$yval), {
        
        if(!is.null(values$selected)){
            for(i in seq_along(req(values$selected))){
                plot_selected_beak(data_3d, selected_species = values$selected[i],
                                   sliders = sliders, colour = stroke_pal[i %% 8])
            }
        }
        
    })
    
    add_selected_3d_beak <- eventReactive(values$last_selection, {
        
        plot_selected_beak(data_3d, selected_species = req(values$last_selection),
                           sliders = sliders, colour = stroke_pal[length(req(values$selected)) %% 8])
    })
    
    get_morphoplot_fill_params <- reactive({
        morphoplot_fill_params_fn(scores, values$highlight_fam, 
                                  values$highlight_gen,
                                  fill_grey, fill_pal)
    })
    
    get_morphoplot_stroke_params <- reactive({
        morphoplot_stroke_params_fn(scores, values$selected,
                                    stroke_grey,
                                    stroke_pal)
    })
    
    # Plot the morphospace:
    vis <- reactive({
        
        # create plot dataframes
        # vertical PCA axis data
        values$plot_v_axis <- data.frame(X = c(input$xval, input$xval),
                                         Y = c(min(scores[,input$yaxis]), max(scores[,input$yaxis])))
        # horizontal PCA axis data
        values$plot_h_axis <- data.frame(X = c(min(scores[,input$xaxis]), max(scores[,input$xaxis])),
                                         Y = c(input$yval, input$yval))
        # PCA axis centre data
        values$plot_centre <- data.frame(X = c(input$xval), Y = c(input$yval))
        
        
        fill_params <- get_morphoplot_fill_params()
        stroke_params <- get_morphoplot_stroke_params()
        
        plot_morphospace(scores, xaxis = input$xaxis, yaxis = input$yaxis,
                         plot_v_axis = values$plot_v_axis, plot_h_axis = values$plot_h_axis,
                         plot_centre = values$plot_centre, 
                         stroke_params = stroke_params, fill_params = fill_params)  %>%
            add_tooltip(clickFunc, "click")
    })
    
    vis %>% bind_shiny("plot1")
    
    output$morphoPlotSide <- rgl::renderRglwidget({
        
        # open new 3d device (re-evaluated when morphospace params change)
        open_3d_ref_beak()
        
        # add beaks of any already selected species (run when morphospace params change 
        # and species already selected)
        re_add_selected_3d_beaks() 
        
        # Add the beak of last selected species
        add_selected_3d_beak()
        
        # embed RGL scene output into html 
        rgl::rglwidget(webgl = TRUE)
        
        
    })
}

############################################################

######
# UI #
######

ui <- fluidPage(
    titlePanel("MARKMYBIRD-O-SPACE"),
    HTML(paste0("<h4>Visualise and explore the position of ", dim(scores)[1], 
                " bird species (", length(unique(scores$Genus)), 
                " genera) in multidimensional bill morphospace using data crowdsourced from <a href='https://www.markmybird.org/' target='_blank'>MarkMyBird.org</a></h4>", sep="")),
    hr(),
    fluidRow(
        column(7,
               br(), 
               ggvisOutput("plot1")
               
        ),
        column(5,
               h3("Bill viewer"),
               wellPanel(
                   rgl::rglwidgetOutput("morphoPlotSide", width = 500, height = 500)
               )),
        hr()
        
    ),
    fluidRow(
        column(4,
               wellPanel(
                   h4("Axes of shape variation"),
                   helpText("Select which axes of bill shape variation to explore"),
                   selectInput("xaxis", label="X axis", names(scores)[grep("PC", names(scores))], selected="PC1"),
                   selectInput("yaxis", label="Y axis", names(scores)[grep("PC", names(scores))], selected="PC2")
               )),
        column(4,
               wellPanel(
                   h4("Morphospace explorer"),
                   helpText("Investigate how bill shape varies across the chosen region of morphospace"),
                   sliderInput("xval", label="X value:", min=-0.5, max=0.5, value=0, round=-1),
                   sliderInput("yval", label="Y value:", min=-0.5, max=0.5, value=0, round=-1),
                   actionButton("goButton", "Reset all")
               )
               
        ),
        column(4,
               wellPanel(
                   selectInput("fam1", label="Select family", choices = c("Search here...", familyNames), 
                               selected = "Search here..."),
                   selectInput("gen1", label="Select genus", choices = c("Search here...", genusNames), 
                               selected = "Search here..."),
                   selectInput("spp1", label="Select species", choices = c("Search here...", speciesNames),
                               selected = "Search here...")
               )
        )
        
    )
)

############################################################

#######
# RUN #
#######

shinyApp(ui, server)
