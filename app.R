library(RColorBrewer) # # # Install these packages
library(shiny)
library(geomorph)
library(ggvis)
library(dplyr)
library(rgl)

options(rgl.printRglwidget = TRUE)

#source("R/morphospace_plot.R")
source("R/3d_beak.R")

# Plotting pars:
so <- readRDS("data/PlottingPars_SideOn.rds")
td <- readRDS("data/PlottingPars_TopDown.rds")

# Read in data:
gpa <- readRDS("data/Filtered_spp_gpa_2015_11_13.rds")
sliders <- read.csv("data/sliders.csv", header=F)
scores <- read.csv("data/MMB_PCscores_median_2015_11_13.csv", strings=F)
csizes <- read.csv("data/MMB_Csizes_median_2015_11_13.csv", strings=F)
taxo <- read.csv("data/BLIOCPhyloMasterTax_2015_05_06.csv", strings=F)

# Set up objects correctly:
coords <- gpa$median.symm.coords
scores$Spp <- scores$Species
scores$Spp2 <- gsub("_", " ", scores$Species)
scores$Family <- taxo$BLFamilyLatin[match(scores$Spp, taxo$TipLabel)]
scores$Genus <- taxo$GenusName[match(scores$Spp, taxo$TipLabel)]
scores$English <- taxo$English[match(scores$Spp, taxo$TipLabel)]
scores$LogCenSize <- csizes$LogCsize[match(scores$Species, csizes$Species)]
scores$Order <- taxo$IOCOrder[match(scores$Spp, taxo$TipLabel)]
scores$Group <- "All species"
scores$Group <- factor(scores$Group, levels=c("All species", "Selected"))

baseGroup <- scores$Group
names(baseGroup) <- scores$Spp
familyNames <- sort(scores$Family)
genusNames <- sort(scores$Genus)
speciesNames <- sort(setNames(scores$Species, gsub("_", " ", scores$Species)))


data_3d <- data_3d_get(coords)

# scores$Group <- ifelse(scores$Order=="PASSERIFORMES", "Passerines", ifelse(scores$Order=="ANSERIFORMES","Ducks & geese", ifelse(scores$Order=="APODIFORMES", "Hummingbirds & swifts", ifelse(scores$Order=="PSITTACIFORMES","Parrots","Others"))))
# scores$Group <- factor(scores$Group, levels=c("Ducks & geese","Hummingbirds & swifts","Parrots","Passerines","Others","Selected"))

# Plotting cols:
#cols <- c(brewer.pal(9, name="Set1")[9], brewer.pal(9, name="Set1")[4], brewer.pal(9, name="Set1")[2], brewer.pal(9, name="Set1")[1], brewer.pal(9, name="Set1")[5], brewer.pal(9, name="Set1")[3])
fcols <- cols <- c(RColorBrewer::brewer.pal(9, name="Set1"))

plot_cols <- c(RColorBrewer::brewer.pal(8, name="Dark2"))
#display.brewer.pal(10, "Set1")

# Create reative values object:
values <- reactiveValues()
values$clickspp <- NULL
values$groups <- scores$Group
values$last_selection <- NULL
values$selected <- NULL
values$plot3d <- NULL

clickFunc <- function(x) {
    if(is.null(x)) return(NULL)
    values$clickspp <- x$Spp
    # temp <- values$group
    # temp[scores$Spp==x$Spp] <- "Selected"
    # values$groups <- as.factor(temp)
    paste0("<b>", scores$English[scores$Spp==x$Spp], "</b><br><i>", gsub("_"," ",x$Spp), "</i><br><a href='https://www.google.com/search?q=", gsub("_"," ",x$Spp), "' target='_blank'>Search this species</a>") # Add tbm=isch& for image search
}
############################################################

##########
# SERVER #
##########

server <- function(input, output, clientData, session) {

	observe({
		updateSliderInput(session, "xval", label = "X value:", min=round(min(scores[,input$xaxis]),2), max=round(max(scores[,input$xaxis]),2), value=0)
		updateSliderInput(session, "yval", label = "Y value:", min=round(min(scores[,input$yaxis]),2), max=round(max(scores[,input$yaxis]),2), value=0)
	})

	observeEvent(input$goButton, {
		values$clickspp <- NULL
		values$groups <- baseGroup
		observe({
			updateSliderInput(session, "xval", label = "X value:", min=round(min(scores[,input$xaxis]),2), max=round(max(scores[,input$xaxis]),2), value=0)
			updateSliderInput(session, "yval", label = "Y value:", min=round(min(scores[,input$yaxis]),2), max=round(max(scores[,input$yaxis]),2), value=0)
			updateSelectInput(session, "fam1", label="Select family", c("Search here...", familyNames), "Search here...")
			updateSelectInput(session, "gen1", label="Select genus", c("Search here...", genusNames), "Search here...")
			updateSelectInput(session, "spp1", label="Select species", c("Search here...", speciesNames), "Search here...")
		})
  	})

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
	
	open_3d_ref_beak <- eventReactive(c(input$xval, input$yval), {
	    
	    rgl::close3d()
	    
	    rgl::par3d(so)
	    
	    plot_ref_beak(data_3d, xaxis = input$xaxis, yaxis = input$yaxis, xval = input$xval, 
	                  yval = input$yval, sliders = sliders, colour = "black")
	})
	
	re_add_selected_3d_beaks <- eventReactive(c(input$xval, input$yval), {
	    
	    if(!is.null(values$selected)){
	        for(i in seq_along(values$selected)){
	            plot_selected_beak(data_3d, selected_species = values$selected[i],
	                               sliders = sliders, colour = plot_cols[i %% 8])
	        }
	    }
	    
	})
	
	add_selected_3d_beak <- eventReactive(values$last_selection, {
	    
	    plot_selected_beak(data_3d, selected_species = values$last_selection,
	               sliders = sliders, colour = plot_cols[length(values$selected) %% 8])
	})
	
	# Plot the morphospace:
	vis <- reactive({

		vert <- data.frame(X=c(input$xval,input$xval), Y=c(min(scores[,input$yaxis]),max(scores[,input$yaxis])))
		hori <- data.frame(X=c(min(scores[,input$xaxis]),max(scores[,input$xaxis])), Y=c(input$yval,input$yval))
		pnt <- data.frame(X=c(input$xval),Y=c(input$yval))

		chosenfams <- c(input$fam1,input$gen1,input$spp1); names(chosenfams) <- c(1:3)
  		chosenfams <- chosenfams[chosenfams != "Search here..."]
  		flabels <- factor(as.character(baseGroup), levels=c("All species"))
  		slabels <- factor(as.character(baseGroup), levels=c("All species"))
  		fcols <- cols[9]
  		scols <- cols[9]
  		
  		if (length(chosenfams) > 0) {
  			for (i in 1:length(chosenfams)) {
  				if (chosenfams[i] %in% scores$Family) {
  				    flabels[scores$Family==chosenfams[i]] <- chosenfams[i]
  				}
  				if (chosenfams[i] %in% scores$Genus) {
  				    flabels[scores$Genus==chosenfams[i]] <- chosenfams[i]
  				}
  				if (chosenfams[i] %in% scores$Spp2) {
  				    flabels[scores$Spp2==chosenfams[i]] <- chosenfams[i]
  				}
  			}
  		    flabels <- factor(flabels, levels=c(levels(flabels), chosenfams))
  			fcols <- c(fcols, cols[c(1,2,3,5,6,7,8)[as.numeric(names(chosenfams))]])
  		}
  		if (!is.null(values$clickspp)) {
  		     slabels[names(baseGroup) == values$clickspp] <- values$clickspp
    		 slabels <- factor(slabels, levels=c(levels(slabels), values$clickspp))
    		 scols <- c(scols, plot_cols[seq_along(values$clickspp)])
    	}
    	values$fgroups <- flabels
    	values$sgroups <- slabels
    	
		scores$FGroup <- values$fgroups
		scores$SGroup <- values$sgroups

		scores %>%
			ggvis(x = prop("x", as.symbol(input$xaxis)), y = prop("y", as.symbol(input$yaxis))) %>%
			layer_points(size = ~LogCenSize, size.hover := 300,
				fillOpacity := 0.2, fillOpacity.hover := 0.2, key := ~Spp, stroke = ~SGroup, fill = ~FGroup) %>%
			layer_paths(data=vert, x=~X, y=~Y, stroke:="red", strokeWidth:=3, strokeOpacity:=0.5) %>%
			layer_paths(data=hori, x=~X, y=~Y, stroke:="red", strokeWidth:=3, strokeOpacity:=0.5) %>%
			layer_points(data=pnt, x=~X, y=~Y, stroke:="red", size := 50, fillOpacity := 0, opacity:=0.5, strokeWidth:=3) %>%
			add_tooltip(clickFunc, "click") %>%
			add_axis("x", title = input$xaxis) %>%
			add_axis("y", title = input$yaxis) %>%
			set_options(height=500, duration=0) %>%
			add_legend("size", title = "Bill size (log)", properties = legend_props(legend=list(y=120))) %>%
			scale_nominal("stroke", range=c(cols[9], scols)) %>%
			scale_nominal("fill", range=c(cols[9], fcols))
	})

	vis %>% bind_shiny("plot1")

  	output$morphoPlotSide <- rgl::renderRglwidget({
  	    
  	    #cat("clickspp:", values$clickspp, "\n\n")
  	    #cat("spp1:", input$spp1, "\n\n")
  	    #cat("selected:", values$selected, "\n\n")
  	    
  	    open_3d_ref_beak()
  	    re_add_selected_3d_beaks() 
  	    
  	    add_selected_3d_beak()
  	    
  	    rgl::rglwidget(webgl = TRUE, width = 100, height = 100)
  	    
 
  	})
}

############################################################

######
# UI #
######

ui <- fluidPage(
  titlePanel("MARKMYBIRD-O-SPACE"),
  HTML(paste0("<h4>Visualise and explore the position of ", dim(scores)[1], " bird species (", length(unique(scores$Genus)), " genera) in multidimensional bill morphospace using data crowdsourced from <a href='https://www.markmybird.org/' target='_blank'>MarkMyBird.org</a></h4>", sep="")),
  hr(),
  fluidRow(
  	column(3,
  		wellPanel(
  			h4("Axes of shape variation"),
  			helpText("Select which axes of bill shape variation to explore"),
  			selectInput("xaxis", label="X axis", names(scores)[grep("PC", names(scores))], selected="PC1"),
  			selectInput("yaxis", label="Y axis", names(scores)[grep("PC", names(scores))], selected="PC2")
  		),
  		wellPanel(
  			h4("Morphospace explorer"),
  			helpText("Investigate how bill shape varies across the chosen region of morphospace"),
  			sliderInput("xval", label="X value:", min=-0.5, max=0.5, value=0, round=-1),
  			sliderInput("yval", label="Y value:", min=-0.5, max=0.5, value=0, round=-1),
  			actionButton("goButton", "Reset all")
  		)
  	),
  	column(6,
  		br(), 
  		ggvisOutput("plot1"),
  		h3("Bill viewer"),
  		wellPanel(
  		    rgl::rglwidgetOutput("morphoPlotSide", width = 400, height = 250)
  		)
  	),
  	column(3,
  	       
  	       selectizeInput("fam1", label="Family", c("Search here...", familyNames)),
  	       selectizeInput("gen1", label="Genus", c("Search here...", genusNames)),
  	       selectizeInput("spp1", label="Species", c("Search here...", speciesNames))
  	)
  ),
  hr()
)

############################################################

#######
# RUN #
#######

shinyApp(ui, server)
