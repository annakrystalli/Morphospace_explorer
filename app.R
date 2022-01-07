
#setwd("~/Downloads/shiny-apps/Morphospace_explorer/")

library(RColorBrewer) # # # Install these packages
library(shiny)
library(geomorph)
library(ggvis)
library(dplyr)
library(rgl)

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

baseGroup <- scores$Group; names(baseGroup) <- scores$Spp
familyNames <- sort(scores$Family)
genusNames <- sort(scores$Genus)
speciesNames <- sort(scores$Spp2)

# scores$Group <- ifelse(scores$Order=="PASSERIFORMES", "Passerines", ifelse(scores$Order=="ANSERIFORMES","Ducks & geese", ifelse(scores$Order=="APODIFORMES", "Hummingbirds & swifts", ifelse(scores$Order=="PSITTACIFORMES","Parrots","Others"))))
# scores$Group <- factor(scores$Group, levels=c("Ducks & geese","Hummingbirds & swifts","Parrots","Passerines","Others","Selected"))

# Plotting cols:
#cols <- c(brewer.pal(9, name="Set1")[9], brewer.pal(9, name="Set1")[4], brewer.pal(9, name="Set1")[2], brewer.pal(9, name="Set1")[1], brewer.pal(9, name="Set1")[5], brewer.pal(9, name="Set1")[3])
fcols <- cols <- c(brewer.pal(9, name="Set1"))
#display.brewer.pal(10, "Set1")

# Create reative values object:
values <- reactiveValues()
values$clickspp <- NULL
values$groups <- scores$Group

# Function for displaying hover names:
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

	# Plot the morphospace:
	vis <- reactive({

		vert <- data.frame(X=c(input$xval,input$xval), Y=c(min(scores[,input$yaxis]),max(scores[,input$yaxis])))
		hori <- data.frame(X=c(min(scores[,input$xaxis]),max(scores[,input$xaxis])), Y=c(input$yval,input$yval))
		pnt <- data.frame(X=c(input$xval),Y=c(input$yval))

		chosenfams <- c(input$fam1,input$gen1,input$spp1); names(chosenfams) <- c(1:3)
  		chosenfams <- chosenfams[chosenfams != "Search here..."]
  		temp2 <- as.character(baseGroup)
  		if (length(chosenfams) > 0) {
  			for (i in 1:length(chosenfams)) {
  				if (chosenfams[i] %in% scores$Family) {
  					temp2[scores$Family==chosenfams[i]] <- chosenfams[i]
  				}
  				if (chosenfams[i] %in% scores$Genus) {
  					temp2[scores$Genus==chosenfams[i]] <- chosenfams[i]
  				}
  				if (chosenfams[i] %in% scores$Spp2) {
  					temp2[scores$Spp2==chosenfams[i]] <- chosenfams[i]
  				}
  			}
  			temp2 <- factor(temp2, levels=c("All species", "Selected", chosenfams))
  			fcols <- cols[c(9,4,c(1,2,3,5,6,7,8)[as.numeric(names(chosenfams))])]
  		} else {
  			temp2 <- factor(temp2, levels=c("All species", "Selected"))
  			fcols <- cols[c(9,4)]
  		}
  		if (!is.null(values$clickspp)) {
    		 temp2[scores$Spp==values$clickspp] <- "Selected"
    	}
    	values$groups <- as.factor(temp2)

		scores$Group <- values$groups

		scores %>%
			ggvis(x = prop("x", as.symbol(input$xaxis)), y = prop("y", as.symbol(input$yaxis))) %>%
			layer_points(size = ~LogCenSize, size.hover := 300,
				fillOpacity := 0.2, fillOpacity.hover := 0.2, key := ~Spp, stroke = ~Group, fill = ~Group) %>%
			layer_paths(data=vert, x=~X, y=~Y, stroke:="red", strokeWidth:=3, strokeOpacity:=0.5) %>%
			layer_paths(data=hori, x=~X, y=~Y, stroke:="red", strokeWidth:=3, strokeOpacity:=0.5) %>%
			layer_points(data=pnt, x=~X, y=~Y, stroke:="red", size := 50, fillOpacity := 0, opacity:=0.5, strokeWidth:=3) %>%
			add_tooltip(clickFunc, "click") %>%
			add_axis("x", title = input$xaxis) %>%
			add_axis("y", title = input$yaxis) %>%
			set_options(height=500, duration=0) %>%
			add_legend("size", title = "Bill size (log)", properties = legend_props(legend=list(y=120))) %>%
			scale_nominal("stroke", range=fcols) %>%
			scale_nominal("fill", range=fcols)
	})

	vis %>% bind_shiny("plot1")

  	output$morphoPlotSide <- rgl::renderRglwidget({

    	A <- coords
    	axis1 <- which(colnames(scores)==input$xaxis)-1 # Account for Spp col
    	axis2 <- which(colnames(scores)==input$yaxis)-1 # Account for Spp col
    	k <- dim(A)[2]; p <- dim(A)[1]; n <- dim(A)[3]
	   	ref <- mshape(A)
    	x <- two.d.array(A)
    	pc.res <- prcomp(x)
    	pcdata <- pc.res$x
    	pc.val.1 <- rep(0, dim(pcdata)[2])
		pc.val.1[axis1] <- input$xval
		pc.val.1[axis2] <- input$yval

		if (!is.null(values$clickspp)) {
    		values$pc.val.2 <- pcdata[substr(rownames(pcdata), 1, nchar(rownames(pcdata))-2)==values$clickspp,]
    	} else {
    		values$pc.val.2 <- NULL
    	}
		if (input$spp1 != "Search here...") {
		    values$pc.val.3 <- pcdata[substr(rownames(pcdata), 1, nchar(rownames(pcdata))-2)==gsub(" ", "_",input$spp1),]
		} else {
		    values$pc.val.3 <- NULL
		}
		shape1 <- arrayspecs(as.matrix(pc.val.1 %*% (t(pc.res$rotation))),p,k)[,,1] + ref
		if (is.null(values$pc.val.2) & is.null(values$pc.val.3)) {
			par3d(so)
			for (i in 1:nrow(sliders)) {
				segments3d(rbind(shape1[sliders[i,1],], shape1[sliders[i,2],]), lwd=1, color="black", alpha=0.5, box=FALSE, axes=FALSE, xlab="", ylab="", zlab="")
			}
		} else {
			par3d(so)
			for (i in 1:nrow(sliders)) {
				segments3d(rbind(shape1[sliders[i,1],], shape1[sliders[i,2],]), lwd=1, color="black", alpha=0.5, box=FALSE, axes=FALSE, xlab="", ylab="", zlab="")
			}
            if (!is.null(values$pc.val.2)) {
                shape2 <- arrayspecs(as.matrix(values$pc.val.2 %*% (t(pc.res$rotation))),p,k)[,,1] + ref
                par3d(so)
                for (i in 1:nrow(sliders)) {
                    segments3d(rbind(shape2[sliders[i,1],], shape2[sliders[i,2],]), lwd=3, color=brewer.pal(9, name="Set1")[4], box=FALSE, axes=FALSE, xlab="", ylab="", zlab="")
                }
            }
			try(if (!is.null(values$pc.val.3)) {
			    shape3 <- arrayspecs(as.matrix(values$pc.val.3 %*% (t(pc.res$rotation))),p,k)[,,1] + ref
			    par3d(so)
			    for (i in 1:nrow(sliders)) {
			        segments3d(rbind(shape3[sliders[i,1],], shape3[sliders[i,2],]), lwd=3, color=brewer.pal(9, name="Set1")[3], box=FALSE, axes=FALSE, xlab="", ylab="", zlab="")
			    }
			}, silent=T)
		}
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
  		br(), br(), br(),
  		ggvisOutput("plot1"),
  		selectizeInput("fam1", label="Family", c("Search here...", familyNames)),
  		selectizeInput("gen1", label="Genus", c("Search here...", genusNames)),
  		selectizeInput("spp1", label="Species", c("Search here...", speciesNames))
  	),
  	column(3,
  		br(), br(),
  		h3("Bill viewer"),
  		rgl::rglwidgetOutput("morphoPlotSide")
  	)
  ),
  hr()
)

############################################################

#######
# RUN #
#######

shinyApp(ui, server)
