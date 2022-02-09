# ---- SETUP ----
library(shiny)
library(ggvis)

options(rgl.printRglwidget = FALSE, rgl.useNULL = TRUE)

# ---- source functions
source("R/morphospace_plot.R")
source("R/3d_beak.R")
source("R/taxo_select.R")
source("R/theme.R")

# ---- DATA ----
# ---- Read in & process data
scores <- read.csv("data/scores.csv", strings = F)
# read in and process coordinate data ready for 3d plotting
data_3d <- data_3d_get(readRDS("data/Filtered_spp_gpa_2015_11_13.rds")$median.symm.coords)
# plotting pars
so <- readRDS("data/PlottingPars_SideOn.rds")
sliders <- read.csv("data/sliders.csv", header = F)
# taxonomic subset select choice vectors
familyNames <- sort(scores$Family)
genusNames <- sort(scores$Genus)
speciesNames <- sort(scores$Spp2)

# ---- Reactive values
values <- reactiveValues()
values$clickspp <- NULL
values$last_selection <- NULL
values$selected <- NULL
values$plot3d <- NULL
values$plot_v_axis <- NULL
values$plot_h_axis <- NULL
values$plot_centre <- NULL
values$subset_fam <- NULL
values$subset_gen <- NULL
values$max_select_exceeded <- FALSE

# Morphospace plot tooltip function
clickFunc <- function(x) {
    if(is.null(x)) return(NULL)
    values$clickspp <-  x$Spp2 
    paste0("<b>", scores$English[scores$Spp2==x$Spp2], 
           "</b><br><i>", x$Spp2, 
           "</i><br><a href='https://www.google.com/search?q=", 
          x$Spp2, "' target='_blank'>Search this species</a>") 
}


#### Styling settings can be found in R/theme.R -----   ####

############################################################
## ---- SERVER --------------------------------------------
server <- function(input, output, clientData, session) {
    # ---- OBSERVERS ----
    # ---- Update morphospace axes ranges in response to changes in axes variables ----
    observeEvent(input$xaxis, {
        updateSliderInput(session, "xval", label = "X value:", 
                          min = round(min(scores[,input$xaxis]),2), 
                          max = round(max(scores[,input$xaxis]),2), value = 0)
    }, ignoreInit = TRUE)
    
    observeEvent(input$yaxis, {
        updateSliderInput(session, "yval", label = "Y value:", 
                          min = round(min(scores[,input$yaxis]),2), 
                          max = round(max(scores[,input$yaxis]),2), value = 0)
    }, ignoreInit = TRUE)
    
    # ---- Update taxonomic select input choices in response to changes in taxo subset ----
    observeEvent(input$fam1, {
        
        values$subset_fam <- if(input$fam1 == "Search here..."){NULL}else{input$fam1}
        values$subset_gen <- NULL
        
        updateSelectInput(session, "gen1", label = "Select genus", 
                          c("Search here...", 
                            get_genus_options(scores, values$subset_fam)), 
                          "Search here...")
        updateSelectInput(session, "spp1", label = "Select species", 
                          c("Search here...", 
                            get_spp_options(scores, values$subset_fam, values$subset_gen)), 
                          "Search here...")
    }, ignoreInit = TRUE)
    
    observeEvent(input$gen1, {
        values$subset_gen <- if(input$gen1 == "Search here..."){NULL}else{input$gen1}
        updateSelectInput(session, "spp1", label="Select species", 
                          c("Search here...", 
                            get_spp_options(scores, req(values$subset_fam), 
                                            req(values$subset_gen))), 
                          "Search here...")
    }, ignoreInit = TRUE)
    
    # ---- Reset events ----
    # Reset morphospace
    observeEvent(input$reset_morphospace, {
        updateSelectInput(session, "xaxis", label="X axis", names(scores)[grep("PC", names(scores))], selected = "PC1")
        updateSelectInput(session, "yaxis", label="Y axis", names(scores)[grep("PC", names(scores))], selected = "PC2")
        updateSliderInput(session, "xval", label = "X value:", 
                          min = round(min(scores[,input$xaxis]),2), 
                          max = round(max(scores[,input$xaxis]),2), value = 0)
        updateSliderInput(session, "yval", label = "Y value:", 
                          min = round(min(scores[,input$yaxis]),2), 
                          max = round(max(scores[,input$yaxis]),2), value = 0)
    }, ignoreInit = TRUE)
    
    # Reset selected species
    observeEvent(input$reset_taxo_select, {
        values$last_selection <- NULL
        values$selected <- NULL
        values$max_select_exceeded <- FALSE
    }, ignoreInit = TRUE)
    
    # Reset taxonomic subset
    observeEvent(input$reset_taxo_subset, {
        values$subset_fam <- NULL
        values$subset_gen <- NULL
        
        updateSelectInput(session, "fam1", label = "Subset family", 
                          c("Search here...", familyNames), 
                          "Search here...")
        updateSelectInput(session, "gen1", label = "Subset genus", 
                          c("Search here...", genusNames), 
                          "Search here...")
        updateSelectInput(session, "spp1", label = "Select species", 
                          c("Search here...", speciesNames), 
                          "Search here...")
    }, ignoreInit = TRUE)
    
    # ---- Update selected and last_selection of spp in response to click on 
    # morphospace plot or taxo selection ----
    observeEvent(c(input$spp1, values$clickspp), {
        species_inputs <- c(input$spp1[input$spp1 != "Search here..."], 
                            values$clickspp)
        
        if(length(species_inputs) > 0){
            select_diff <- species_inputs[!species_inputs %in% values$selected]
            if(length(select_diff) > 0){
                if(length(values$selected) < max_selected){
                    values$last_selection <- select_diff
                    values$selected <- c(values$selected, values$last_selection)
                }else{
                    values$max_select_exceeded <- TRUE
                    showNotification(paste0("Maximum number of allowed selected species (", max_selected
                                            ,") exceeded. Please clear all selected species and start again"),
                                     type = "warning")
                }
            }
        }
    })
    
    observeEvent(input$morphospaceInfo, {
        showModal(modalDialog(
            title = "What does the Morphospace represent?",
            "Details of the morphospace will go here
            Here's a new paragraph",
            easyClose = TRUE
        ))
    })
    
    observeEvent(input$billInfo, {
        showModal(modalDialog(
            title = "What does the bill data represent?",
            p("Details of the bill will go here"),
            helpText("Black reference trace represents the bill shape at the current morphospace centre. Use morphospace navigator to compare selected species to reference shapes at different morphospace region centers"),
            easyClose = TRUE
        ))
    })
    # ---- REACTIVES ----
    # ---- 3D Beak rgl scene ----
    # New scene & update ref beak in response to changes in morphospace centre or species
    # selection resets
    reset_scene <- eventReactive(c(input$xval, input$yval, input$reset_taxo_select),{
        rgl::close3d()
        rgl::open3d()
        rgl::par3d(so)
        
        plot_ref_beak(data_3d, xaxis = input$xaxis, yaxis = input$yaxis, xval = input$xval, 
                      yval = input$yval, sliders = sliders, colour = "black",
                      lwd = ref_beak_lwd, alpha = 1)
        
        rerender_selected_beaks()
    })
    
    # rerender any selected beaks but only in response to changes in morphospace centre
    rerender_selected_beaks <- eventReactive(c(input$xval, input$yval),{
        if(!is.null(values$selected)){
            for(i in seq_along(req(values$selected))){
                plot_selected_beak(data_3d, selected_species = values$selected[i],
                                   sliders = sliders, colour = stroke_pal[i],
                                   lwd = beak_lwd, alpha = 1)
            }
        }
    })
    # Add 3d beak of last selected species
    add_selected_3d_beak <- reactive({
        
        if(!is.null(values$last_selection)){
        plot_selected_beak(data_3d, selected_species = req(values$last_selection),
                           sliders = sliders, lwd = beak_lwd, alpha = 1,
                           colour = stroke_pal[length(req(values$selected))])
        }
        
    })
    
    # Render 3D beak scene
    render_3dbeak_scene <- reactive({
        
        # open new 3d device (re-evaluated when morphospace params change) and 
        # add beaks of any already selected species
        reset_scene()
        
        # Add the beak of last selected species
        add_selected_3d_beak()
        
    })
    
    # ---- Morphospace params ----
    # Update indices and color scale for point fill properties (taxonomic subset)
    get_morphoplot_fill_params <- reactive({
        morphoplot_fill_params_fn(scores, values$subset_fam, 
                                  values$subset_gen,
                                  fill_grey, fill_pal)
    })
    
    # Update indices and color scale for point stroke properties (selected species)
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
                         stroke_params = stroke_params, fill_params = fill_params,
                         font_size = morphoplot_font_size)  %>%
            add_tooltip(clickFunc, "click") %>%
            set_options(width = "auto", resizable=FALSE)
    })
    
    vis %>% ggvis::bind_shiny("morphospace-plot")
    
    output$morphoPlotSide <- rgl::renderRglwidget({
        
        # Render 3D beak scene
        render_3dbeak_scene()
        
        # embed RGL scene output into html
        rgl::rglwidget(webgl = TRUE)
    
        
    })
}

############################################################
## ---- UI --------------------------------------------

ui <- fluidPage(theme = morphospace_theme, style = "max-width: 95%;",

                titlePanel(fluidRow(column(width = 2, tags$img(src = "TUOS_PRIMARY_LOGO_LINEAR_BLACK.png", width = "100%")),
                               column(width = 10, h1("MARKMYBIRD-O-SPACE", style="margin-top: 0;"), style="display: flex; align-items: center;")),
                           windowTitle= "MARKMYBIRD-O-SPACE"),

                div(h5(paste0("Visualise and explore the position of ", nrow(scores), 
                              " bird species (", length(unique(scores$Genus)), 
                              " genera) in multidimensional bill morphospace using data crowdsourced from"),
                       a("MarkMyBird.org", href="https://www.markmybird.org/", target="_blank"))),
                fluidRow(
                    column(7,
                           h3("Morphospace viewer"),
                           fluidRow(
                               column(
                           actionButton("morphospaceInfo", "Find out more about the morphospace",
                                        icon = icon("info-circle"),
                                        class="btn btn-light"), width = 6),
                           column(
                           actionButton("reset_taxo_select", "Reset all selected species"),
                           width = 6)),
                           ggvis::ggvisOutput("morphospace-plot"),
                           br()
                           
                    ),
                    column(5,
                           h3("Bill viewer"),
                           actionButton("billInfo", "Find out more about the bill data",
                                        icon = icon("info-circle"),
                                        class="btn btn-light"),
                               rgl::rglwidgetOutput("morphoPlotSide", width = 500, height = 500)
                           ),
                    br()
                    
                ),
                fluidRow(
                    tabsetPanel(type = "tabs",
                                tabPanel(h4("Taxonomic navigator"),
                                         #column(4,
                                         fluidRow(
                                         column(5,
                                                wellPanel(
                                                    h4("Subset taxonomy"),
                                                    selectInput("fam1", label="Select family", choices = c("Search here...", familyNames), 
                                                                selected = "Subset here..."),
                                                    selectInput("gen1", label="Subset genus", choices = c("Search here...", genusNames), 
                                                                selected = "Search here..."),
                                                    actionButton("reset_taxo_subset", "Reset all subsets")),
                                                br()),
                                         column(5,
                                                wellPanel(
                                                    h4("Select species"),
                                                    selectInput("spp1", label="Select species", choices = c("Search here...", speciesNames),
                                                                selected = "Search here..."))),
                                         column(2)
                                )),
                                tabPanel(h4("Morphospace navigator"),
                                         h6("Investigate how bill shape varies across axes of shape variation and regions of morphospace"),
                                         fluidRow(
                                             column(4,
                                                    wellPanel(
                                                        h4("Axes of shape variation"),
                                                        helpText("Select which axes of bill shape variation to explore"),
                                                        selectInput("xaxis", label="X axis", names(scores)[grep("PC", names(scores))], selected="PC1"),
                                                        selectInput("yaxis", label="Y axis", names(scores)[grep("PC", names(scores))], selected="PC2")
                                                    )),
                                             column(8,
                                                    wellPanel(
                                                        h4("Morphospace region center"),
                                                        helpText("Choose the center of the morphospace region to explore"),
                                                        sliderInput("xval", label="X value:", min=-0.5, max=0.5, value=0, round=-1),
                                                        sliderInput("yval", label="Y value:", min=-0.5, max=0.5, value=0, round=-1),
                                                        actionButton("reset_morphospace", "Reset morphospace params")
                                                    )
                                             )
                                         )
                                )
                    )
                    
                ),
                br(),
                fluidRow(
                    hr(),
                    helpText("The data displayed is v1 of", em("'Dataset Title'"), a("10.15131/shef.data.19145069", href="https://doi.org/10.15131/shef.data.19145069", target="_blank"), "described in",  em("'Paper Title'"),  a("paper DOI", href="", target="_blank") ),
                    helpText("The code for this app is published at", a("10.15131/shef.data.19130714", href="https://doi.org/10.15131/shef.data.19130714", target="_blank")),
                    br(),
                    helpText("Mark My Bird is part of a European Research Council (ERC) funded project based at the University of Sheffield. The project aims to contribute to our understanding of how and why evolutionary rates vary across the tree of life and what this means for the origins and maintenance of biological diversity. The main museums collections used to date are the NHM bird collections at Natural History Museum at Tring and the Manchester Museum. We are extremely grateful for the time and expertise of the staff at both institutions for their continued support. This website was built by the talented team at The Digital Humanities Institute.")#,
                    
                )
)

############################################################

#######
# RUN #
#######
shinyApp(ui, server)
