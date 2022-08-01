# this is a fast and easy way to publish the app, but the user has to run it in his/her own R environment
#install.packages("shiny")
#require(shiny)
#runGitHub( "Shiny-MP-spectra", "Maki-science")

# for reactive plot content I adapted ideas of https://stackoverflow.com/questions/42104031/shiny-interactive-ggplot-with-vertical-line-and-data-labels-at-mouse-hover-poin
#require(ggplot2)
#require(shiny)
#require(pracma)

ggplot2::theme_set(ggplot2::theme( # Theme (Hintergrund, Textgröße, Text-Positionen und -Ausrichtung)
  axis.text.x = ggplot2::element_text(size=10, angle=0, vjust=0.0), 
  axis.text.y = ggplot2::element_text(size = 10), 
  axis.title = ggplot2::element_text(size = 20), 
  plot.title = ggplot2::element_text(hjust = 0.5), 
  legend.position="top", 
  panel.background = ggplot2::element_blank(), 
  axis.line = ggplot2::element_line(size = 0.5),
  panel.border = ggplot2::element_blank()
)
)

# setwd("//btb1r2.bio.uni-bayreuth.de/home/PhD/Paper/Eigene/Vinay/Shiny MP spectra/")
# read in prepared data 
# this is just done to skip the time consuming step of preparing the data
# mydata <- read.table(
#   file = "prep_data", 
#   sep = ";",
#   dec = "."
# )
mydata <- int.data[[2]]

mydata$polV <- as.factor(mydata$polV)
mydata$pol <- as.factor(mydata$pol)
mydata$incWater <- as.factor(mydata$incWater)
mydata$incWater <- factor(mydata$incWater, levels = c("pristine", "FW", "SW"))


# to provide easily understandable content for user, we need the full names of polymers
polAbr <- levels(mydata$pol)
polNames <- c("ABS: acrylnitrile-butadiene-styrole" = "ABS", 
              "PA: polyamide" = "PA", 
              "PC: polycarbonate" = "PC", 
              "PE: polyethylene" = "PE", 
              "PET: polyethyleneterephthalate" = "PET", 
              "POM: polyoxymethylene" = "POM", 
              "PP: polypropylene" = "PP", 
              "PS: polystyrene" = "PS", 
              "PU: polyurethane" = "PU", 
              "PVC: polyvinyl-chloride" = "PVC", 
              "SAN: styrole-acrylnitrile" = "SAN")

waterAbr <- levels(mydata$incWater)
waterNames <- c("freshwater (FW)" = "FW", "seawater (SW)" = "SW")

# # to provide further information about substance classes, we read another table with those information
# components <- read.table(
#   file = "components",
#   sep = "\t",
#   dec = ".",
#   header = TRUE
# )
component <- int.data[[1]]

server <- function(input, output, session){
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  
  
  ##### use reactive values for mouse position in plot #####
  values <- reactiveValues(loc = 0, component = list(), hjust = -0.1, locy = 0)
  
  observeEvent(input$plot_hover$x, {
    values$loc <- input$plot_hover$x
    values$component <- list(paste("current position: ", round(values$loc), "\n", "typical peak for:"))
    
    # check current wavenumber and whether there is a known component at this point
    for(i in 1:nrow(components)){
      if(values$loc <= (components$rmax[i] + 10) && values$loc >= (components$rmin[i] - 10)){
        values$component <- append(values$component, paste("~", components$value[i], ", ", components$cname[i], sep = ""))
      }
    }
    
    # Check whether position is too far on the right side, to print the component label
    # on the left side of the vline. Otherwise it gets lost at the borders of the plot
    if(values$loc >= 3000){
      values$hjust = 1.1
    }
    else{
      values$hjust = -0.1
    }
    
  })
  # if you want to reset the initial position of the vertical line when input$points changes
  observeEvent(input$plot_hover$y, {
    values$locy <- input$plot_hover$y
  })
  
  ### reactive value for plot-data management for variants####
  # Therefore, the data for the plots don't have to be loaded and processed every time, the 
  # hover function is triggered for the meta data view
  # This improves processing time and therefore user experience
  plotData <- reactiveValues(temp = data.frame(), sSW = "", sFW = "")
  
  observeEvent(c(input$polType, input$nV, input$FW, input$SW), {
    # first not use the reactive value for calculation, because accessing this object
    # needs significantly more computation time
    # therefore do processing first and then insert into reactive value
    if(input$SW == TRUE){
      sSW <- "SW"
    }
    else{
      sSW <- ""
    }
    if(input$FW == TRUE){
      sFW <- "FW"
    }
    else{
      sFW <- ""
    }
    
    temp <- droplevels(mydata[which(mydata$pol == input$polType & 
                                      mydata$v <= input$nV & 
                                      (mydata$incWater == "pristine" |
                                         mydata$incWater == sFW |
                                         mydata$incWater == sSW 
                                      )
    ),]
    )
    
    # reorder polV as the order of the plot is confusing otherwise
    newOrder <- c(levels(droplevels(temp[grep("pristine", temp$polV),])$polV))
    newOrder <- c(newOrder, levels(droplevels(temp[grep("FW", temp$polV),])$polV))
    newOrder <- c(newOrder, levels(droplevels(temp[grep("SW", temp$polV),])$polV))
    temp$polV <- factor(temp$polV, levels = newOrder)
    
    temp$v <- as.factor(temp$v)
    # insert processed data into reactive value
    plotData$temp <- temp
  })
  # separate the response of sep, because this is computationally intense
  observeEvent(input$sep, {
    
    temp <- plotData$temp
    
    # if input$sep == TRUE, multiply values of each spectrum with a certain number, to split them along y-axis
    if(input$sep == TRUE){
      for(i in 1:length(levels(temp$polV))){
        for(j in 1:nrow(temp)){
          if(temp$polV[j] == levels(temp$polV)[i]){
            temp$amp[j] <- temp$amp[j] + 0.5 * (i-1)
          }
        }
      }
    }
    
    plotData$temp <- temp
    
  })
  
  
  
  ### reactive value for plot-data management for comparison ####
  # Therefore, the data for the plots don't have to be loaded and processed every time, the 
  # hover function is triggered for the meta data view
  plotData.comp <- reactiveValues(temp = data.frame())
  
  observeEvent(c(input$comp.polType1, input$comp.polType2, input$comp.variant1, input$comp.variant2, input$comp.water1, input$comp.water2), {
    # first not use the reactive value for calculation, because accessing this object
    # needs significantly more computation time
    # therefore do processing first and then insert into reactive value
    
    temp <- droplevels(subset(mydata, (mydata$pol == input$comp.polType1 & 
                                         mydata$v == input$comp.variant1 & 
                                         (mydata$incWater == input$comp.water1 |
                                            mydata$incWater == "pristine")
    ) |
      (mydata$pol == input$comp.polType2 & 
         mydata$v == input$comp.variant2 & 
         (mydata$incWater == input$comp.water2 |
            mydata$incWater == "pristine")
      )
    )
    )
    
    # # reorder polV as the order of the plot is confusing otherwise
    # newOrder <- c(levels(droplevels(temp[grep("pristine", temp$polV),])$polV))
    # newOrder <- c(newOrder, levels(droplevels(temp[grep("FW", temp$polV),])$polV))
    # newOrder <- c(newOrder, levels(droplevels(temp[grep("SW", temp$polV),])$polV))
    # temp$polV <- factor(temp$polV, levels = newOrder)
    # 
    # temp$v <- as.factor(temp$v)
    # insert processed data into reactive value
    plotData.comp$temp <- temp
  })
  
  
  ### reactive value for plot-data management for comparison ####
  # Therefore, the data for the plots don't have to be loaded and processed every time, the 
  # hover function is triggered for the meta data view
  plotData.own <- reactiveValues(temp = data.frame())
  
  observeEvent(c(input$own.polType, input$own.variant, input$own.water, input$own.spec), {
    # first not use the reactive value for calculation, because accessing this object
    # needs significantly more computation time
    # therefore do processing first and then insert into reactive value
    
    temp <- droplevels(subset(mydata, (mydata$pol == input$own.polType & 
                                         mydata$v == input$own.variant & 
                                         (mydata$incWater == input$own.water |
                                            mydata$incWater == "pristine")
    )
    )
    )
    # check whether own spectrum is provided (already)
    if(input$own.spec == ""){ # if not, insert NAs
      od <- rep(NA, 1600)
    }
    else{ # if yes, process them
      # check whether user input has same length as our spectra
      # otherwise stretch/dampen data accordingly
      od <- strsplit(input$own.spec, "\n")[[1]]
      
      if(length(od) != length(temp$wavenumber)){
        # if users resolution is higher, delete points evenly distributed
        if(length(od) > length(temp$wavenumber)){
          
          exod <- round(seq(length.out = (length(od) / length(temp$wavenumber) * 1600 -1600),
                            from = 1,
                            to = length(od)
          )
          )
          od <- od[-exod]
          
        }
        # if users resolution is lower, insert NAs at evenly distributed points
        else{
          seqin <- round(seq(length.out = length(temp$wavenumber) %% length(od), from = 1, to = 1600))
          for(i in 1:length(seqin)){
            od <- append(od, NA, after = seqin[i])
          }
          # interpolate the NAs to have a smooth line (without gaps) in the graph
          od <- as.numeric(od)
          od <- pracma::interp1(x = temp$wavenumber, y = od, xi = temp$wavenumber, "linear")
        }
      }
    }
    
    own <- data.frame(wavenumber = temp$wavenumber,
                      amp = od,
                      pol = "your polymer",
                      polV = "your.V1",
                      v = 1,
                      incWater = "n.a."
    )
    
    temp <- rbind(temp, own)
    temp$amp <- as.numeric(temp$amp)
    
    # # reorder polV as the order of the plot is confusing otherwise
    # newOrder <- c(levels(droplevels(temp[grep("pristine", temp$polV),])$polV))
    # newOrder <- c(newOrder, levels(droplevels(temp[grep("FW", temp$polV),])$polV))
    # newOrder <- c(newOrder, levels(droplevels(temp[grep("SW", temp$polV),])$polV))
    # temp$polV <- factor(temp$polV, levels = newOrder)
    # 
    # temp$v <- as.factor(temp$v)
    # insert processed data into reactive value
    plotData.own$temp <- temp
  })
  
  
  
  
  ##### render plots #######
  
  #### plot variants #####
  
  output$plot.variants <- renderPlot({
    
    g <- ggplot2::ggplot(plotData$temp, 
                         ggplot2::aes(x = wavenumber, 
                    y = amp, 
                    group = interaction(incWater, v, sep = " | "),
                    colour = interaction(incWater, v, sep = " | ")
                )
    )+
      ggplot2::geom_line()+
      ggplot2::coord_cartesian(xlim = c(500.51919, 3681.31383))+
      ggplot2::ylab("Raman-Intensity")+
      ggplot2::xlab( expression(Wavenumber~cm^{-1}) )+
      ggplot2::scale_color_discrete(name = "Variant number | Incubation water")+ 
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank())+
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 20))
    
    if(values$loc > 400 && values$loc < 3700 && values$locy < max(plotData$temp$amp) && values$locy > 0){
      # reactive vertical line and text
      g <- g+ ggplot2::geom_vline(ggplot2::aes(xintercept = values$loc), linetype = "dotted")+
        ggplot2::geom_text(ggplot2::aes(x = values$loc,
                      y = 0.8,
                      label = suppressWarnings(
                        if(length(values$component > 1)){
                          paste(values$component, collapse = "\n")
                        }
                        else{
                          ""
                        }
                      ),
                      vjust = length(values$component)/10,
                      hjust = values$hjust
        ),
        size=4,
        show.legend = FALSE,
        colour = "black"
        )
    }
    g
    
  }) # end render plot
  
  #### plot comparisons #####
  output$plot.comp <- renderPlot({
    
    g <- ggplot2::ggplot(plotData.comp$temp, 
                         ggplot2::aes(x = wavenumber, 
                    y = amp, 
                    group = interaction(pol, factor(v), incWater, sep = " | "),
                    colour = interaction(pol, factor(v), incWater, sep = " | ")
                )
    )+
      ggplot2::geom_line()+
      ggplot2::coord_cartesian(xlim = c(500.51919, 3681.31383))+
      ggplot2::ylab("Raman-Intensity")+
      ggplot2::xlab( expression(Wavenumber~cm^{-1}) )+ 
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank())+
      ggplot2::scale_color_discrete(name = "Polymer | Variant number | Incubation water")+
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 20))
    # reactive vertical line and text
    if(values$loc > 400 && values$loc < 3700 && values$locy < max(plotData.comp$temp$amp) && values$locy > 0){
      # reactive vertical line and text
      g <- g+ ggplot2::geom_vline(ggplot2::aes(xintercept = values$loc), linetype = "dotted")+
        ggplot2::geom_text(ggplot2::aes(x = values$loc,
                      y = 0.8,
                      label = suppressWarnings(
                        if(length(values$component > 1)){
                          paste(values$component, collapse = "\n")
                        }
                        else{
                          ""
                        }
                      ),
                      vjust = length(values$component)/10,
                      hjust = values$hjust
        ),
        size=4,
        show.legend = FALSE,
        colour = "black"
        )
    }
    g
  }) # end render plot
  
  
  #### plot own ######
  output$plot.own <- renderPlot({
    
    g <- ggplot2::ggplot(plotData.own$temp, 
                         ggplot2::aes(x = wavenumber, 
                    y = amp, 
                    group = interaction(pol, factor(v), incWater, sep = " | "),
                    colour = interaction(pol, factor(v), incWater, sep = " | ")
                )
    )+
      ggplot2::geom_line(na.rm = TRUE)+
      ggplot2::coord_cartesian(xlim = c(500.51919, 3681.31383))+
      ggplot2::ylab("Raman-Intensity")+
      ggplot2::xlab( expression(Wavenumber~cm^{-1}) )+
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank())+
      ggplot2::scale_color_discrete(name = "Polymer | Variant number | Incubation water")+
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 20))
    if(values$loc > 400 && values$loc < 3700 && values$locy < max(plotData.own$temp$amp, na.rm = TRUE) && values$locy > 0){
      # reactive vertical line and text
      g <- g+ ggplot2::geom_vline(ggplot2::aes(xintercept = values$loc), linetype = "dotted")+
        ggplot2::geom_text(ggplot2::aes(x = values$loc,
                      y = 0.8,
                      label = suppressWarnings(
                        if(length(values$component > 1)){
                          paste(values$component, collapse = "\n")
                        }
                        else{
                          ""
                        }
                      ),
                      vjust = length(values$component)/10,
                      hjust = values$hjust
        ),
        size=4,
        show.legend = FALSE,
        colour = "black"
        )
    }
    g
  }) # end render plot
  
} # end server



#### ui #####
ui <- shiny::fluidPage(
  shiny::fluidRow(
    shiny::titlePanel(
      shiny::h1("Raman spectra of Microplastic Particles' Plastisphere", align = "center")
    ),
    shiny::column(12, style="font-size:1.4em", align = "center", shiny::tags$a(href="http://www.maki-science.org", "Spatio-chemical analysis of the plastisphere using Raman spectroscopy", target="_blank")),
    shiny::column(12, style="font-size:0.8em", align = "center", "VKB Narayana, A. Ramsperger, M. Kiene, J. Brehm, M. Löder, C. Laforsch 2022"),
    shiny::column(12, style="line-height:3em", align = "center", "Press 'ctrl' & '-' or '+' to adjust the object sizes if necessary."),
    shiny::mainPanel(align = "center", width = 12,
              shiny::tabsetPanel(
                ##### ui variants #####
                shiny::tabPanel("Show Variants",
                        shiny::column(6,
                                shiny::selectInput("polType", 
                                            label = "Select polymer type:", 
                                            choices = polNames, 
                                            selected = "PET"
                                ),
                                shiny::sliderInput("nV", 
                                            label = "Number of variants (1 = pristine):", 
                                            min = 1, 
                                            max = max(mydata$v, na.rm = TRUE), 
                                            value = 1, 
                                            step = 1
                                ),
                         ),
                        shiny::column(5, offset = 1,
                               shiny::fluidRow(
                                 shiny::checkboxInput("FW", 
                                                label = "Fresh water samples (FW)", 
                                                value = TRUE, 
                                                width = NULL
                                  ),
                                ),
                               shiny::fluidRow(
                                 shiny::checkboxInput("SW", 
                                                label = "Sea water samples (SW)", 
                                                value = TRUE, 
                                                width = NULL
                                  )
                                ),
                               shiny::hr(),
                               shiny::fluidRow(
                                 shiny::checkboxInput("sep", 
                                                label = "Separate spectra", 
                                                value = FALSE, 
                                                width = NULL
                                  )
                                )
                         ),
                        shiny::hr(),
                        shiny::column(12, "Move your mouse over the graph to view meta data."),
                        shiny::plotOutput(outputId = "plot.variants",
                                    hover = shiny::hoverOpts(id = "plot_hover",
                                                      delay = 40,
                                                      delayType = "debounce"
                                    )
                         )
                ), # end tabPanel
                ##### ui compare #####
                shiny::tabPanel("Compare two Spectra",
                        shiny::column(6,
                                shiny::h4("Select the first spectrum:"),
                                shiny::selectInput("comp.polType1", 
                                            label = "Select polymer type:", 
                                            choices = polNames, 
                                            selected = "PET"
                                ),
                                shiny::selectInput("comp.variant1", 
                                            label = "Select Variant number (1 = pristine):", 
                                            choices = c(1:max(mydata$v, na.rm = TRUE)), 
                                            selected = "1"
                                ),
                                shiny::selectInput("comp.water1", 
                                            label = "Select incubation water:", 
                                            choices = waterNames, 
                                            selected = "FW"
                                )
                         ),
                        shiny::column(6,
                                shiny::h4("Select the second spectrum:"),
                                shiny::selectInput("comp.polType2", 
                                            label = "Select polymer type:", 
                                            choices = polNames, 
                                            selected = "PET"
                                ),
                                shiny::selectInput("comp.variant2", 
                                            label = "Select Variant number (1 = pristine):", 
                                            choices = c(1:max(mydata$v, na.rm = TRUE)), 
                                            selected = "2"
                                ),
                                shiny::selectInput("comp.water2", 
                                            label = "Select incubation water:", 
                                            choices = waterNames, 
                                            selected = "FW"
                                )
                         ),
                        shiny::hr(),
                        shiny::column(12, "Move your mouse over the graph to view meta data."),
                        shiny::plotOutput(outputId = "plot.comp",
                                    hover = shiny::hoverOpts(id = "plot_hover",
                                                      delay = 40,
                                                      delayType = "debounce"
                                    ))
                ), # end tabPanel
                ##### ui own #####
                shiny::tabPanel("Compare with own Spectrum",
                        shiny::column(6,
                                shiny::h4("Select spectrum to compare:"),
                                shiny::selectInput("own.polType", 
                                            label = "Select polymer type:", 
                                            choices = polNames, 
                                            selected = "PET"
                                ),
                                shiny::selectInput("own.variant", 
                                            label = "Select Variant number (1 = pristine):", 
                                            choices = c(1:max(mydata$v, na.rm = TRUE)), 
                                            selected = "1"
                                ),
                                shiny::selectInput("own.water", 
                                            label = "Select incubation water:", 
                                            choices = c("fresh water" = "FW", "sea water" = "SW"), 
                                            selected = "FW"
                                )
                         ),
                        shiny::column(6,
                                shiny::h4("Your own Spectrum"),
                                shiny::textAreaInput("own.spec",
                                              label = "Insert Vector of your spectrum:",
                                              placeholder = "copy and paste from excel file column..."
                                ),
                                "note: if your resolution is higher/lower, your spectrum will be compressed/streched accordingly"
                         ),
                        shiny::hr(),
                        shiny::column(12, "Move your mouse over the graph to view meta data."),
                        shiny::plotOutput(outputId = "plot.own",
                                    hover = shiny::hoverOpts(id = "plot_hover",
                                                      delay = 40,
                                                      delayType = "debounce"
                                    ))
                ) # end tabPanel
              ) # end tabsetPanel
    ) # end mainPanel
  )#, # end row
  # fluidRow(style = "width:100%;",
  #          mainPanel("Find the corresponding publication to this app at ", tags$a(href="http://www.maki-science.org", "(to be changed)", target="_blank"),
  #                    align = "center", 
  #                    style = "
  #                  position:absolute;
  #                  bottom:0;
  #                  width:100%;
  #                  color: white;
  #                  padding: 10px;
  #                  background-color: lightgrey;"
  #          )
  # )
) # end fluidPage ; end ui

shiny::shinyApp(ui = ui, server = server)