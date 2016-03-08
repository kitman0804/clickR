library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  dfx0 <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile)) return(NULL)
    
    dfx <- read.table(inFile$datapath, header = input$header == "TRUE", sep = input$sep, quote = input$quote, stringsAsFactors = input$str_as_fac == "TRUE")
    
    return(dfx)
  })
  
  
  dfx <- reactive({
    dfx <- dfx0()
    
    if (is.null(dfx)) return(NULL)
    if (input$set_class == 0) return(dfx)
    
    input$set_class
    isolate({
      for (i in 1:ncol(dfx)) dfx[, i] <- do.call(paste0("as.", input[[paste0("col", i)]]), list(dfx[, i])) # Change column class
    })
    
    return(dfx)
  })
  
  
  
  
  #==== Data Table ====#
  output$view_data <- DT::renderDataTable({
    DT::datatable(dfx(), options = list(pageLength = 10))
  })
  
  
  
  
  #==== Set column class ====#
  output$set_vrb_class <- renderUI({
    dfx <- dfx0()
    
    if (is.null(dfx)) return(NULL)
    
    vrb_name <- names(dfx)
    vrb_type <- unlist(lapply(dfx, class))
    
    ui <- lapply(1:ncol(dfx), function(i) {
      class_list <- c("character", "factor", "integer", "logical", "numeric")
      if (vrb_type[i] %in% c("character", "factor")) class_list <- c("character", "factor")
      
      radioButtons(paste0("col", i), vrb_name[i], choice = class_list, selected = vrb_type[i], inline = TRUE)
    })
    
    return(ui)
  })
  
  
  
  
  #==== Summary of data ====#
  output$dfx_smry <- renderPrint({
    dfx <- dfx()
    
    if (is.null(dfx)) {
      cat("Hihi!\nYou have not imported your data. \n") 
    } else {
      cat("Dimension of data:\n", "Row:", nrow(dfx), "Col:", ncol(dfx), "\n\n")
      vrbs <- lapply(dfx, function(col) data.frame(TYPE = class(col), MISSING = sum(is.na(col))))
      vrbs <- do.call("rbind", vrbs)
      cat("Summary of data:\n")
      print(vrbs)
    }
  })
  
  
  
  
  #==== Chart - Update input ====#
  observe({
    d <- dfx()
    vrb_name <- names(d)
    vrb_type <- unlist(lapply(d, class))
    
    vrb_all <- vrb_name
    vrb_num <- vrb_name[vrb_type %in% c("numeric", "integer")]
    vrb_nom <- vrb_name[vrb_type %in% c("character", "factor", "logical")]
    
    if (input$plot_type == 'b') {
      updateSelectInput(session, "vrb_x", label = "Variable", choices = vrb_nom)
      updateSelectInput(session, "vrb_weight", label = "Weight", choices = c("-" = "-", vrb_num))
      updateSelectInput(session, "vrb_col", label = "Group (Colour)", choices = c("-" = "-", vrb_nom))
    } else if (input$plot_type %in% "l") {
      updateSelectInput(session, "vrb_x", label = "x-axis", choices = vrb_num)
      updateSelectInput(session, "vrb_y", label = "y-axis", choices = vrb_num)
      updateSelectInput(session, "vrb_col", label = "Group (Colour)", choices = c("-" = "-", vrb_nom))
      updateSelectInput(session, "vrb_lty", label = "Group (Linetype)", choices = c("-" = "-", vrb_nom))
    } else if (input$plot_type %in% "p") {
      updateSelectInput(session, "vrb_x", label = "x-axis", choices = vrb_num)
      updateSelectInput(session, "vrb_y", label = "y-axis", choices = vrb_num)
      updateSelectInput(session, "vrb_col", label = "Group (Colour)", choices = c("-" = "-", vrb_all))
    }
  })
  
  
  
  
  #==== Chart ====#
  output$plot <- renderPlot({
    dfx <- dfx()
    
    if (is.null(dfx)) return(NULL)
    
    input$plot_buttom
    isolate({
      # if (input$plot_type == "") return(NULL)
      
      # arg0 <- "x = dfx[[input$vrb_x]]"
      # chart <- chart + xlab(input$vrb_x)
      # 
      # if (!is.null(input$vrb_y)) {
      #   if (input$vrb_y != "") {
      #     arg0 <- paste0(arg0, ", y = dfx[[input$vrb_y]]")
      #     # chart <- chart + ylab(input$vrb_y)
      #   }
      # }
      # 
      # arg <- arg0
      # if (!is.null(input$vrb_weight)) {
      #   if (input$vrb_weight != "") {
      #     arg <- paste0(arg, ", weight = dfx[[input$vrb_weight]]")
      #   }
      # }
      # if (!is.null(input$vrb_col)) {
      #   if (input$vrb_col != "") {
      #     arg <- paste0(arg, ", colour = dfx[[input$vrb_col]]")
      #     # chart <- chart + labs(colour = input$vrb_col)
      #   }
      # }
      # if (!is.null(input$vrb_lty)) {
      #   if (input$vrb_lty != "") {
      #     arg <- paste0(arg, ", linetype = dfx[[input$vrb_lty]]")
      #     # chart <- chart + labs(linetype = dfx[[input$vrb_lty]])
      #   }
      # }
      # arg <- paste0("aes(", arg, ")")
      # print(arg)
      # if (input$plot_type == "b") chart <- chart + geom_bar(eval(parse(text = arg)))
      # if (input$plot_type == "l") chart <- chart + geom_line(eval(parse(text = arg)))
      # if (input$plot_type == "p") chart <- chart + geom_point(eval(parse(text = arg)))
      
      
      chart <- ggplot()
      
      if (input$plot_type %in% c("l")) {
        # Plot
        arg0 <- "x = dfx[[input$vrb_x]], y = dfx[[input$vrb_y]]"
        arg <- arg0
        if (input$vrb_col != "-") {
          arg <- paste0(arg, ", colour = dfx[[input$vrb_col]]")
          chart <- chart + labs(colour = input$vrb_col)
        }
        if (input$vrb_lty != "-") {
          arg <- paste0(arg, ", linetype = dfx[[input$vrb_lty]]")
          chart <- chart + labs(linetype = input$vrb_lty)
        }
        arg <- paste0("aes(", arg, ")")
        chart <- chart + geom_line(eval(parse(text = arg))) + xlab(input$vrb_x) + ylab(input$vrb_y)
      } else if (input$plot_type %in% c("p")) {
        # Plot
        arg0 <- "x = dfx[[input$vrb_x]], y = dfx[[input$vrb_y]]"
        arg <- arg0
        if (input$vrb_col != "-") {
          arg <- paste0(arg, ", colour = dfx[[input$vrb_col]]")
          chart <- chart + labs(colour = input$vrb_col)
        }
        arg <- paste0("aes(", arg, ")")
        chart <- chart + geom_point(eval(parse(text = arg))) + xlab(input$vrb_x) + ylab(input$vrb_y)
      } else if (input$plot_type %in% c("b")) {
        # Plot
        arg0 <- "x = dfx[[input$vrb_x]]"
        arg <- arg0
        if (input$vrb_weight != "-") {
          arg <- paste0(arg, ", weight = dfx[[input$vrb_weight]]")
        }
        if (input$vrb_col != "-") {
          arg <- paste0(arg, ", colour = as.factor(dfx[[input$vrb_col]]), fill = as.factor(dfx[[input$vrb_col]])")
          chart <- chart + labs(colour = input$vrb_col) + labs(fill = input$vrb_col)
        }
        arg <- paste0("aes(", arg, ")")
        chart <- chart + geom_bar(eval(parse(text = arg)), position = input$bar_position) + xlab(input$vrb_x)
      } else {
        return(NULL)
      }
      
      chart <- chart + theme_bw()
      return(chart)
    })
  })
  
  
  #==== END ====#
})