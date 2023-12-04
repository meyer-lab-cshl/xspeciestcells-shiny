library(shiny) 
library(shinyhelper) 
library(data.table) 
library(Matrix) 
library(DT) 
library(magrittr) 
sc_huconf = readRDS("sc_huconf.rds")
sc_hudef  = readRDS("sc_hudef.rds")



sc_msconf = readRDS("sc_msconf.rds")
sc_msdef  = readRDS("sc_msdef.rds")



### Start server code 
shinyUI(fluidPage( 
### HTML formatting of error messages 
 
tags$head(tags$style(HTML(".shiny-output-error-validation {color: red; font-weight: bold;}"))), 
list(tags$style(HTML(".navbar-default .navbar-nav { font-weight: bold; font-size: 16px; }"))), 
 
   
### Page title 
titlePanel("ShinyCell Human & Mouse innate T cell development"),  
navbarPage( 
  NULL,  
 navbarMenu("Human data",### Tab1.a1: cellInfo vs geneExpr on dimRed 
  tabPanel( 
    HTML("CellInfo vs GeneExpr"), 
    h4("Cell information vs gene expression on reduced dimensions"), 
    "In this tab, users can visualise both cell information and gene ",  
    "expression side-by-side on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("sc_hua1drX", "X-axis:", choices = sc_huconf[dimred == TRUE]$UI, 
                           selected = sc_hudef$dimred[1]), 
            selectInput("sc_hua1drY", "Y-axis:", choices = sc_huconf[dimred == TRUE]$UI, 
                        selected = sc_hudef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("sc_hua1togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.sc_hua1togL % 2 == 1", 
          selectInput("sc_hua1sub1", "Cell information to subset:", 
                      choices = sc_huconf[grp == TRUE]$UI, 
                      selected = sc_hudef$grp1), 
          uiOutput("sc_hua1sub1.ui"), 
          actionButton("sc_hua1sub1all", "Select all groups", class = "btn btn-primary"), 
          actionButton("sc_hua1sub1non", "Deselect all groups", class = "btn btn-primary") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("sc_hua1tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.sc_hua1tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("sc_hua1siz", "Point size:", 
                             min = 0, max = 4, value = 1.25, step = 0.25), 
              radioButtons("sc_hua1psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Medium", inline = TRUE), 
              radioButtons("sc_hua1fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Medium", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("sc_hua1asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("sc_hua1txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Cell information"), 
        fluidRow( 
          column( 
            6, selectInput("sc_hua1inp1", "Cell information:", 
                           choices = sc_huconf$UI, 
                           selected = sc_hudef$meta1) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("sc_hua1tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.sc_hua1tog1 % 2 == 1", 
              radioButtons("sc_hua1col1", "Colour (Continuous data):", 
                           choices = c("White-Red","Blue-Yellow-Red","Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("sc_hua1ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("sc_hua1lab1", "Show cell info labels", value = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("sc_hua1oup1.ui"))), 
        downloadButton("sc_hua1oup1.pdf", "Download PDF"), 
        downloadButton("sc_hua1oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("sc_hua1oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("sc_hua1oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)), br(), 
        actionButton("sc_hua1tog9", "Toggle to show cell numbers / statistics"), 
        conditionalPanel( 
          condition = "input.sc_hua1tog9 % 2 == 1", 
          h4("Cell numbers / statistics"), 
          radioButtons("sc_hua1splt", "Split continuous cell info into:", 
                       choices = c("Quartile", "Decile"), 
                       selected = "Decile", inline = TRUE), 
          dataTableOutput("sc_hua1.dt") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, h4("Gene expression"), 
        fluidRow( 
          column( 
            6, selectInput("sc_hua1inp2", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("sc_hua1tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.sc_hua1tog2 % 2 == 1", 
              radioButtons("sc_hua1col2", "Colour:", 
                           choices = c("White-Red","Blue-Yellow-Red","Yellow-Green-Purple"), 
                           selected = "White-Red"), 
              radioButtons("sc_hua1ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ) , 
        fluidRow(column(12, uiOutput("sc_hua1oup2.ui"))), 
        downloadButton("sc_hua1oup2.pdf", "Download PDF"), 
        downloadButton("sc_hua1oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("sc_hua1oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("sc_hua1oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
 
  ### Tab1.a2: cellInfo vs cellInfo on dimRed 
  tabPanel( 
    HTML("CellInfo vs CellInfo"), 
    h4("Cell information vs cell information on dimension reduction"), 
    "In this tab, users can visualise two cell informations side-by-side ", 
    "on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("sc_hua2drX", "X-axis:", choices = sc_huconf[dimred == TRUE]$UI, 
                           selected = sc_hudef$dimred[1]), 
            selectInput("sc_hua2drY", "Y-axis:", choices = sc_huconf[dimred == TRUE]$UI, 
                        selected = sc_hudef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("sc_hua2togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.sc_hua2togL % 2 == 1", 
          selectInput("sc_hua2sub1", "Cell information to subset:", 
                      choices = sc_huconf[grp == TRUE]$UI, 
                      selected = sc_hudef$grp1), 
          uiOutput("sc_hua2sub1.ui"), 
          actionButton("sc_hua2sub1all", "Select all groups", class = "btn btn-primary"), 
          actionButton("sc_hua2sub1non", "Deselect all groups", class = "btn btn-primary") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("sc_hua2tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.sc_hua2tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("sc_hua2siz", "Point size:", 
                             min = 0, max = 4, value = 1.25, step = 0.25), 
              radioButtons("sc_hua2psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Medium", inline = TRUE), 
              radioButtons("sc_hua2fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Medium", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("sc_hua2asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("sc_hua2txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Cell information 1"), 
        fluidRow( 
          column( 
            6, selectInput("sc_hua2inp1", "Cell information:", 
                           choices = sc_huconf$UI, 
                           selected = sc_hudef$meta1) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("sc_hua2tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.sc_hua2tog1 % 2 == 1", 
              radioButtons("sc_hua2col1", "Colour (Continuous data):", 
                           choices = c("White-Red", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("sc_hua2ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("sc_hua2lab1", "Show cell info labels", value = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("sc_hua2oup1.ui"))), 
        downloadButton("sc_hua2oup1.pdf", "Download PDF"), 
        downloadButton("sc_hua2oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("sc_hua2oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("sc_hua2oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      ), # End of column (6 space) 
      column( 
        6, h4("Cell information 2"), 
        fluidRow( 
          column( 
            6, selectInput("sc_hua2inp2", "Cell information:", 
                           choices = sc_huconf$UI, 
                           selected = sc_hudef$meta2) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("sc_hua2tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.sc_hua2tog2 % 2 == 1", 
              radioButtons("sc_hua2col2", "Colour (Continuous data):", 
                           choices = c("White-Red", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("sc_hua2ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("sc_hua2lab2", "Show cell info labels", value = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("sc_hua2oup2.ui"))), 
        downloadButton("sc_hua2oup2.pdf", "Download PDF"), 
        downloadButton("sc_hua2oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("sc_hua2oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("sc_hua2oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
   
  ### Tab1.a3: geneExpr vs geneExpr on dimRed 
  tabPanel( 
    HTML("GeneExpr vs GeneExpr"), 
    h4("Gene expression vs gene expression on dimension reduction"), 
    "In this tab, users can visualise two gene expressions side-by-side ", 
    "on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("sc_hua3drX", "X-axis:", choices = sc_huconf[dimred == TRUE]$UI, 
                           selected = sc_hudef$dimred[1]), 
            selectInput("sc_hua3drY", "Y-axis:", choices = sc_huconf[dimred == TRUE]$UI, 
                        selected = sc_hudef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("sc_hua3togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.sc_hua3togL % 2 == 1", 
          selectInput("sc_hua3sub1", "Cell information to subset:", 
                      choices = sc_huconf[grp == TRUE]$UI, 
                      selected = sc_hudef$grp1), 
          uiOutput("sc_hua3sub1.ui"), 
          actionButton("sc_hua3sub1all", "Select all groups", class = "btn btn-primary"), 
          actionButton("sc_hua3sub1non", "Deselect all groups", class = "btn btn-primary") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("sc_hua3tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.sc_hua3tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("sc_hua3siz", "Point size:", 
                             min = 0, max = 4, value = 1.25, step = 0.25), 
              radioButtons("sc_hua3psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Medium", inline = TRUE), 
              radioButtons("sc_hua3fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Medium", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("sc_hua3asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("sc_hua3txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Gene expression 1"), 
        fluidRow( 
          column( 
            6, selectInput("sc_hua3inp1", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("sc_hua3tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.sc_hua3tog1 % 2 == 1", 
              radioButtons("sc_hua3col1", "Colour:", 
                           choices = c("White-Red", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "White-Red"), 
              radioButtons("sc_hua3ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("sc_hua3oup1.ui"))), 
        downloadButton("sc_hua3oup1.pdf", "Download PDF"), 
        downloadButton("sc_hua3oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("sc_hua3oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("sc_hua3oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      ), # End of column (6 space) 
      column( 
        6, h4("Gene expression 2"), 
        fluidRow( 
          column( 
            6, selectInput("sc_hua3inp2", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("sc_hua3tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.sc_hua3tog2 % 2 == 1", 
              radioButtons("sc_hua3col2", "Colour:", 
                           choices = c("White-Red", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "White-Red"), 
              radioButtons("sc_hua3ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("sc_hua3oup2.ui"))), 
        downloadButton("sc_hua3oup2.pdf", "Download PDF"), 
        downloadButton("sc_hua3oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("sc_hua3oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("sc_hua3oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
 
 ### Tab1.b2: Gene coexpression plot 
 tabPanel( 
   HTML("Gene coexpression"), 
   h4("Coexpression of two genes on reduced dimensions"), 
   "In this tab, users can visualise the coexpression of two genes ", 
   "on low-dimensional representions.", 
   br(),br(), 
   fluidRow( 
     column( 
       3, h4("Dimension Reduction"), 
       fluidRow( 
         column( 
           12, selectInput("sc_hub2drX", "X-axis:", choices = sc_huconf[dimred == TRUE]$UI, 
                           selected = sc_hudef$dimred[1]), 
           selectInput("sc_hub2drY", "Y-axis:", choices = sc_huconf[dimred == TRUE]$UI, 
                       selected = sc_hudef$dimred[2])) 
       ) 
     ), # End of column (6 space) 
     column( 
       3, actionButton("sc_hub2togL", "Toggle to subset cells"), 
       conditionalPanel( 
         condition = "input.sc_hub2togL % 2 == 1", 
         selectInput("sc_hub2sub1", "Cell information to subset:", 
                     choices = sc_huconf[grp == TRUE]$UI, 
                    selected = sc_hudef$grp1), 
         uiOutput("sc_hub2sub1.ui"), 
         actionButton("sc_hub2sub1all", "Select all groups", class = "btn btn-primary"), 
         actionButton("sc_hub2sub1non", "Deselect all groups", class = "btn btn-primary") 
       ) 
     ), # End of column (6 space) 
     column( 
       6, actionButton("sc_hub2tog0", "Toggle graphics controls"), 
       conditionalPanel( 
         condition = "input.sc_hub2tog0 % 2 == 1", 
         fluidRow( 
           column( 
             6, sliderInput("sc_hub2siz", "Point size:", 
                            min = 0, max = 4, value = 1.25, step = 0.25), 
             radioButtons("sc_hub2psz", "Plot size:", 
                          choices = c("Small", "Medium", "Large"), 
                          selected = "Medium", inline = TRUE), 
             radioButtons("sc_hub2fsz", "Font size:", 
                          choices = c("Small", "Medium", "Large"), 
                          selected = "Medium", inline = TRUE) 
           ), 
           column( 
             6, radioButtons("sc_hub2asp", "Aspect ratio:", 
                             choices = c("Square", "Fixed", "Free"), 
                             selected = "Square", inline = TRUE), 
             checkboxInput("sc_hub2txt", "Show axis text", value = FALSE) 
           ) 
         ) 
       ) 
     )  # End of column (6 space) 
   ),   # End of fluidRow (4 space) 
   fluidRow( 
     column( 
       3, style="border-right: 2px solid black", h4("Gene Expression"), 
       selectInput("sc_hub2inp1", "Gene 1:", choices=NULL) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
               title = "Gene expression to colour cells by", 
               content = c("Select gene to colour cells by gene expression", 
                          paste0("- Gene expression are coloured in a ", 
                                 "White-Red colour scheme which can be ", 
                                 "changed in the plot controls"))), 
       selectInput("sc_hub2inp2", "Gene 2:", choices=NULL) %>% 
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Gene expression to colour cells by", 
                content = c("Select gene to colour cells by gene expression", 
                            paste0("- Gene expression are coloured in a ", 
                                   "White-Blue colour scheme which can be ", 
                                   "changed in the plot controls"))), 
       actionButton("sc_hub2tog1", "Toggle plot controls"), 
       conditionalPanel( 
         condition = "input.sc_hub2tog1 % 2 == 1", 
         radioButtons("sc_hub2col1", "Colour:", 
                      choices = c("Red (Gene1); Blue (Gene2)", 
                                  "Orange (Gene1); Blue (Gene2)", 
                                  "Red (Gene1); Green (Gene2)", 
                                  "Green (Gene1); Blue (Gene2)"), 
                      selected = "Red (Gene1); Blue (Gene2)"), 
         radioButtons("sc_hub2ord1", "Plot order:", 
                      choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                      selected = "Max-1st", inline = TRUE) 
       ) 
     ), # End of column (6 space) 
     column( 
       6, style="border-right: 2px solid black", 
       uiOutput("sc_hub2oup1.ui"), 
       downloadButton("sc_hub2oup1.pdf", "Download PDF"), 
       downloadButton("sc_hub2oup1.png", "Download PNG"), br(), 
       div(style="display:inline-block", 
           numericInput("sc_hub2oup1.h", "PDF / PNG height:", width = "138px", 
                        min = 4, max = 20, value = 8, step = 0.5)), 
       div(style="display:inline-block", 
           numericInput("sc_hub2oup1.w", "PDF / PNG width:", width = "138px", 
                        min = 4, max = 20, value = 10, step = 0.5)) 
     ), # End of column (6 space) 
     column( 
       3, uiOutput("sc_hub2oup2.ui"), 
       downloadButton("sc_hub2oup2.pdf", "Download PDF"), 
       downloadButton("sc_hub2oup2.png", "Download PNG"), 
       br(), h4("Cell numbers"), 
       dataTableOutput("sc_hub2.dt") 
     )  # End of column (6 space) 
   )    # End of fluidRow (4 space) 
 ),     # End of tab (2 space) 
 
 ### Tab1.c1: violinplot / boxplot 
 tabPanel( 
    HTML("Violinplot / Boxplot"),  
   h4("Cell information / gene expression violin plot / box plot"), 
   "In this tab, users can visualise the gene expression or continuous cell information ",  
   "(e.g. Number of UMIs / module score) across groups of cells (e.g. libary / clusters).", 
   br(),br(), 
   fluidRow( 
     column( 
       3, style="border-right: 2px solid black", 
       selectInput("sc_huc1inp1", "Cell information (X-axis):", 
                   choices = sc_huconf[grp == TRUE]$UI, 
                   selected = sc_hudef$grp1) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Cell information to group cells by",  
                content = c("Select categorical cell information to group cells by",  
                            "- Single cells are grouped by this categorical covariate",  
                            "- Plotted as the X-axis of the violin plot / box plot")),  
       selectInput("sc_huc1inp2", "Cell Info / Gene name (Y-axis):", choices=NULL) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Cell Info / Gene to plot", 
                content = c("Select cell info / gene to plot on Y-axis", 
                            "- Can be continuous cell information (e.g. nUMIs / scores)", 
                            "- Can also be gene expression")), 
       radioButtons("sc_huc1typ", "Plot type:", 
                    choices = c("violin", "boxplot"), 
                    selected = "violin", inline = TRUE), 
       checkboxInput("sc_huc1pts", "Show data points", value = FALSE), 
       actionButton("sc_huc1togL", "Toggle to subset cells"), 
       conditionalPanel( 
         condition = "input.sc_huc1togL % 2 == 1", 
         selectInput("sc_huc1sub1", "Cell information to subset:", 
                     choices = sc_huconf[grp == TRUE]$UI, 
                     selected = sc_hudef$grp1), 
         uiOutput("sc_huc1sub1.ui"), 
         actionButton("sc_huc1sub1all", "Select all groups", class = "btn btn-primary"), 
         actionButton("sc_huc1sub1non", "Deselect all groups", class = "btn btn-primary") 
       ), br(), br(), 
       actionButton("sc_huc1tog", "Toggle graphics controls"), 
       conditionalPanel( 
         condition = "input.sc_huc1tog % 2 == 1", 
         sliderInput("sc_huc1siz", "Data point size:",  
                     min = 0, max = 4, value = 1.25, step = 0.25),  
         radioButtons("sc_huc1psz", "Plot size:", 
                      choices = c("Small", "Medium", "Large"), 
                      selected = "Medium", inline = TRUE), 
         radioButtons("sc_huc1fsz", "Font size:", 
                      choices = c("Small", "Medium", "Large"), 
                      selected = "Medium", inline = TRUE)) 
     ), # End of column (6 space) 
     column(9, uiOutput("sc_huc1oup.ui"),  
            downloadButton("sc_huc1oup.pdf", "Download PDF"),  
            downloadButton("sc_huc1oup.png", "Download PNG"), br(), 
            div(style="display:inline-block", 
                numericInput("sc_huc1oup.h", "PDF / PNG height:", width = "138px", 
                             min = 4, max = 20, value = 8, step = 0.5)), 
            div(style="display:inline-block", 
                numericInput("sc_huc1oup.w", "PDF / PNG width:", width = "138px", 
                             min = 4, max = 20, value = 10, step = 0.5)) 
     )  # End of column (6 space) 
   )    # End of fluidRow (4 space) 
 ),     # End of tab (2 space) 
 
### Tab1.c2: Proportion plot 
tabPanel( 
  HTML("Proportion plot"), 
  h4("Proportion / cell numbers across different cell information"), 
  "In this tab, users can visualise the composition of single cells based on one discrete ", 
  "cell information across another discrete cell information. ",  
  "Usage examples include the library or cellcycle composition across clusters.", 
  br(),br(), 
  fluidRow( 
    column( 
      3, style="border-right: 2px solid black", 
      selectInput("sc_huc2inp1", "Cell information to plot (X-axis):", 
                  choices = sc_huconf[grp == TRUE]$UI, 
                  selected = sc_hudef$grp2) %>%  
        helper(type = "inline", size = "m", fade = TRUE, 
               title = "Cell information to plot cells by",  
               content = c("Select categorical cell information to plot cells by", 
                           "- Plotted as the X-axis of the proportion plot")), 
      selectInput("sc_huc2inp2", "Cell information to group / colour by:", 
                  choices = sc_huconf[grp == TRUE]$UI, 
                  selected = sc_hudef$grp1) %>%  
        helper(type = "inline", size = "m", fade = TRUE, 
               title = "Cell information to group / colour cells by", 
               content = c("Select categorical cell information to group / colour cells by", 
                           "- Proportion / cell numbers are shown in different colours")), 
      radioButtons("sc_huc2typ", "Plot value:", 
                   choices = c("Proportion", "CellNumbers"), 
                   selected = "Proportion", inline = TRUE), 
      checkboxInput("sc_huc2flp", "Flip X/Y", value = FALSE), 
      actionButton("sc_huc2togL", "Toggle to subset cells"), 
      conditionalPanel( 
        condition = "input.sc_huc2togL % 2 == 1", 
        selectInput("sc_huc2sub1", "Cell information to subset:", 
                    choices = sc_huconf[grp == TRUE]$UI, 
                    selected = sc_hudef$grp1), 
        uiOutput("sc_huc2sub1.ui"), 
        actionButton("sc_huc2sub1all", "Select all groups", class = "btn btn-primary"), 
        actionButton("sc_huc2sub1non", "Deselect all groups", class = "btn btn-primary") 
      ), br(), br(), 
      actionButton("sc_huc2tog", "Toggle graphics controls"), 
      conditionalPanel( 
        condition = "input.sc_huc2tog % 2 == 1", 
        radioButtons("sc_huc2psz", "Plot size:", 
                     choices = c("Small", "Medium", "Large"), 
                     selected = "Medium", inline = TRUE), 
        radioButtons("sc_huc2fsz", "Font size:", 
                     choices = c("Small", "Medium", "Large"), 
                     selected = "Medium", inline = TRUE)) 
    ), # End of column (6 space) 
    column(9, uiOutput("sc_huc2oup.ui"),  
           downloadButton("sc_huc2oup.pdf", "Download PDF"),  
           downloadButton("sc_huc2oup.png", "Download PNG"), br(), 
           div(style="display:inline-block", 
               numericInput("sc_huc2oup.h", "PDF / PNG height:", width = "138px", 
                            min = 4, max = 20, value = 8, step = 0.5)), 
           div(style="display:inline-block", 
               numericInput("sc_huc2oup.w", "PDF / PNG width:", width = "138px", 
                            min = 4, max = 20, value = 10, step = 0.5)) 
    )  # End of column (6 space) 
  )    # End of fluidRow (4 space) 
),     # End of tab (2 space) 
 
  ### Tab1.d1: Multiple gene expr 
  tabPanel( 
    HTML("Bubbleplot / Heatmap"), 
    h4("Gene expression bubbleplot / heatmap"), 
    "In this tab, users can visualise the gene expression patterns of ", 
    "multiple genes grouped by categorical cell information (e.g. library / cluster).", br(), 
    "The normalised expression are averaged, log-transformed and then plotted.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, style="border-right: 2px solid black", 
        textAreaInput("sc_hud1inp", HTML("List of gene names <br /> 
                                          (Max 50 genes, separated <br /> 
                                           by , or ; or newline):"), 
                      height = "200px", 
                      value = paste0(sc_hudef$genes, collapse = ", ")) %>% 
          helper(type = "inline", size = "m", fade = TRUE, 
                 title = "List of genes to plot on bubbleplot / heatmap", 
                 content = c("Input genes to plot", 
                             "- Maximum 50 genes (due to ploting space limitations)", 
                             "- Genes should be separated by comma, semicolon or newline")), 
        selectInput("sc_hud1grp", "Group by:", 
                    choices = sc_huconf[grp == TRUE]$UI, 
                    selected = sc_huconf[grp == TRUE]$UI[1]) %>% 
          helper(type = "inline", size = "m", fade = TRUE, 
                 title = "Cell information to group cells by", 
                 content = c("Select categorical cell information to group cells by", 
                             "- Single cells are grouped by this categorical covariate", 
                             "- Plotted as the X-axis of the bubbleplot / heatmap")), 
        radioButtons("sc_hud1plt", "Plot type:", 
                     choices = c("Bubbleplot", "Heatmap"), 
                     selected = "Bubbleplot", inline = TRUE), 
        checkboxInput("sc_hud1scl", "Scale gene expression", value = TRUE), 
        checkboxInput("sc_hud1row", "Cluster rows (genes)", value = TRUE), 
        checkboxInput("sc_hud1col", "Cluster columns (samples)", value = FALSE), 
        br(), 
        actionButton("sc_hud1togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.sc_hud1togL % 2 == 1", 
          selectInput("sc_hud1sub1", "Cell information to subset:", 
                      choices = sc_huconf[grp == TRUE]$UI, 
                      selected = sc_hudef$grp1), 
          uiOutput("sc_hud1sub1.ui"), 
          actionButton("sc_hud1sub1all", "Select all groups", class = "btn btn-primary"), 
          actionButton("sc_hud1sub1non", "Deselect all groups", class = "btn btn-primary") 
        ), br(), br(), 
        actionButton("sc_hud1tog", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.sc_hud1tog % 2 == 1", 
          radioButtons("sc_hud1cols", "Colour scheme:", 
                       choices = c("White-Red", "Blue-Yellow-Red", 
                                   "Yellow-Green-Purple"), 
                       selected = "Blue-Yellow-Red"), 
          radioButtons("sc_hud1psz", "Plot size:", 
                       choices = c("Small", "Medium", "Large"), 
                       selected = "Medium", inline = TRUE), 
          radioButtons("sc_hud1fsz", "Font size:", 
                       choices = c("Small", "Medium", "Large"), 
                       selected = "Medium", inline = TRUE)) 
      ), # End of column (6 space) 
      column(9, h4(htmlOutput("sc_hud1oupTxt")), 
             uiOutput("sc_hud1oup.ui"), 
             downloadButton("sc_hud1oup.pdf", "Download PDF"), 
             downloadButton("sc_hud1oup.png", "Download PNG"), br(), 
             div(style="display:inline-block", 
                 numericInput("sc_hud1oup.h", "PDF / PNG height:", width = "138px", 
                              min = 4, max = 20, value = 10, step = 0.5)), 
             div(style="display:inline-block", 
                 numericInput("sc_hud1oup.w", "PDF / PNG width:", width = "138px", 
                              min = 4, max = 20, value = 10, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  )      # End of tab (2 space) 
   ), 

navbarMenu("Mouse data",### Tab1.a1: cellInfo vs geneExpr on dimRed 
  tabPanel( 
    HTML("CellInfo vs GeneExpr"), 
    h4("Cell information vs gene expression on reduced dimensions"), 
    "In this tab, users can visualise both cell information and gene ",  
    "expression side-by-side on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("sc_msa1drX", "X-axis:", choices = sc_msconf[dimred == TRUE]$UI, 
                           selected = sc_msdef$dimred[1]), 
            selectInput("sc_msa1drY", "Y-axis:", choices = sc_msconf[dimred == TRUE]$UI, 
                        selected = sc_msdef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("sc_msa1togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.sc_msa1togL % 2 == 1", 
          selectInput("sc_msa1sub1", "Cell information to subset:", 
                      choices = sc_msconf[grp == TRUE]$UI, 
                      selected = sc_msdef$grp1), 
          uiOutput("sc_msa1sub1.ui"), 
          actionButton("sc_msa1sub1all", "Select all groups", class = "btn btn-primary"), 
          actionButton("sc_msa1sub1non", "Deselect all groups", class = "btn btn-primary") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("sc_msa1tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.sc_msa1tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("sc_msa1siz", "Point size:", 
                             min = 0, max = 4, value = 1.25, step = 0.25), 
              radioButtons("sc_msa1psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Medium", inline = TRUE), 
              radioButtons("sc_msa1fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Medium", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("sc_msa1asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("sc_msa1txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Cell information"), 
        fluidRow( 
          column( 
            6, selectInput("sc_msa1inp1", "Cell information:", 
                           choices = sc_msconf$UI, 
                           selected = sc_msdef$meta1) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("sc_msa1tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.sc_msa1tog1 % 2 == 1", 
              radioButtons("sc_msa1col1", "Colour (Continuous data):", 
                           choices = c("White-Red","Blue-Yellow-Red","Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("sc_msa1ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("sc_msa1lab1", "Show cell info labels", value = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("sc_msa1oup1.ui"))), 
        downloadButton("sc_msa1oup1.pdf", "Download PDF"), 
        downloadButton("sc_msa1oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("sc_msa1oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("sc_msa1oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)), br(), 
        actionButton("sc_msa1tog9", "Toggle to show cell numbers / statistics"), 
        conditionalPanel( 
          condition = "input.sc_msa1tog9 % 2 == 1", 
          h4("Cell numbers / statistics"), 
          radioButtons("sc_msa1splt", "Split continuous cell info into:", 
                       choices = c("Quartile", "Decile"), 
                       selected = "Decile", inline = TRUE), 
          dataTableOutput("sc_msa1.dt") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, h4("Gene expression"), 
        fluidRow( 
          column( 
            6, selectInput("sc_msa1inp2", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("sc_msa1tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.sc_msa1tog2 % 2 == 1", 
              radioButtons("sc_msa1col2", "Colour:", 
                           choices = c("White-Red","Blue-Yellow-Red","Yellow-Green-Purple"), 
                           selected = "White-Red"), 
              radioButtons("sc_msa1ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ) , 
        fluidRow(column(12, uiOutput("sc_msa1oup2.ui"))), 
        downloadButton("sc_msa1oup2.pdf", "Download PDF"), 
        downloadButton("sc_msa1oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("sc_msa1oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("sc_msa1oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
 
  ### Tab1.a2: cellInfo vs cellInfo on dimRed 
  tabPanel( 
    HTML("CellInfo vs CellInfo"), 
    h4("Cell information vs cell information on dimension reduction"), 
    "In this tab, users can visualise two cell informations side-by-side ", 
    "on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("sc_msa2drX", "X-axis:", choices = sc_msconf[dimred == TRUE]$UI, 
                           selected = sc_msdef$dimred[1]), 
            selectInput("sc_msa2drY", "Y-axis:", choices = sc_msconf[dimred == TRUE]$UI, 
                        selected = sc_msdef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("sc_msa2togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.sc_msa2togL % 2 == 1", 
          selectInput("sc_msa2sub1", "Cell information to subset:", 
                      choices = sc_msconf[grp == TRUE]$UI, 
                      selected = sc_msdef$grp1), 
          uiOutput("sc_msa2sub1.ui"), 
          actionButton("sc_msa2sub1all", "Select all groups", class = "btn btn-primary"), 
          actionButton("sc_msa2sub1non", "Deselect all groups", class = "btn btn-primary") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("sc_msa2tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.sc_msa2tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("sc_msa2siz", "Point size:", 
                             min = 0, max = 4, value = 1.25, step = 0.25), 
              radioButtons("sc_msa2psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Medium", inline = TRUE), 
              radioButtons("sc_msa2fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Medium", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("sc_msa2asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("sc_msa2txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Cell information 1"), 
        fluidRow( 
          column( 
            6, selectInput("sc_msa2inp1", "Cell information:", 
                           choices = sc_msconf$UI, 
                           selected = sc_msdef$meta1) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("sc_msa2tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.sc_msa2tog1 % 2 == 1", 
              radioButtons("sc_msa2col1", "Colour (Continuous data):", 
                           choices = c("White-Red", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("sc_msa2ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("sc_msa2lab1", "Show cell info labels", value = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("sc_msa2oup1.ui"))), 
        downloadButton("sc_msa2oup1.pdf", "Download PDF"), 
        downloadButton("sc_msa2oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("sc_msa2oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("sc_msa2oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      ), # End of column (6 space) 
      column( 
        6, h4("Cell information 2"), 
        fluidRow( 
          column( 
            6, selectInput("sc_msa2inp2", "Cell information:", 
                           choices = sc_msconf$UI, 
                           selected = sc_msdef$meta2) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("sc_msa2tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.sc_msa2tog2 % 2 == 1", 
              radioButtons("sc_msa2col2", "Colour (Continuous data):", 
                           choices = c("White-Red", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("sc_msa2ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("sc_msa2lab2", "Show cell info labels", value = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("sc_msa2oup2.ui"))), 
        downloadButton("sc_msa2oup2.pdf", "Download PDF"), 
        downloadButton("sc_msa2oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("sc_msa2oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("sc_msa2oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
   
  ### Tab1.a3: geneExpr vs geneExpr on dimRed 
  tabPanel( 
    HTML("GeneExpr vs GeneExpr"), 
    h4("Gene expression vs gene expression on dimension reduction"), 
    "In this tab, users can visualise two gene expressions side-by-side ", 
    "on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("sc_msa3drX", "X-axis:", choices = sc_msconf[dimred == TRUE]$UI, 
                           selected = sc_msdef$dimred[1]), 
            selectInput("sc_msa3drY", "Y-axis:", choices = sc_msconf[dimred == TRUE]$UI, 
                        selected = sc_msdef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("sc_msa3togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.sc_msa3togL % 2 == 1", 
          selectInput("sc_msa3sub1", "Cell information to subset:", 
                      choices = sc_msconf[grp == TRUE]$UI, 
                      selected = sc_msdef$grp1), 
          uiOutput("sc_msa3sub1.ui"), 
          actionButton("sc_msa3sub1all", "Select all groups", class = "btn btn-primary"), 
          actionButton("sc_msa3sub1non", "Deselect all groups", class = "btn btn-primary") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("sc_msa3tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.sc_msa3tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("sc_msa3siz", "Point size:", 
                             min = 0, max = 4, value = 1.25, step = 0.25), 
              radioButtons("sc_msa3psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Medium", inline = TRUE), 
              radioButtons("sc_msa3fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Medium", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("sc_msa3asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("sc_msa3txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Gene expression 1"), 
        fluidRow( 
          column( 
            6, selectInput("sc_msa3inp1", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("sc_msa3tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.sc_msa3tog1 % 2 == 1", 
              radioButtons("sc_msa3col1", "Colour:", 
                           choices = c("White-Red", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "White-Red"), 
              radioButtons("sc_msa3ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("sc_msa3oup1.ui"))), 
        downloadButton("sc_msa3oup1.pdf", "Download PDF"), 
        downloadButton("sc_msa3oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("sc_msa3oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("sc_msa3oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      ), # End of column (6 space) 
      column( 
        6, h4("Gene expression 2"), 
        fluidRow( 
          column( 
            6, selectInput("sc_msa3inp2", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("sc_msa3tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.sc_msa3tog2 % 2 == 1", 
              radioButtons("sc_msa3col2", "Colour:", 
                           choices = c("White-Red", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "White-Red"), 
              radioButtons("sc_msa3ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("sc_msa3oup2.ui"))), 
        downloadButton("sc_msa3oup2.pdf", "Download PDF"), 
        downloadButton("sc_msa3oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("sc_msa3oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("sc_msa3oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
 
 ### Tab1.b2: Gene coexpression plot 
 tabPanel( 
   HTML("Gene coexpression"), 
   h4("Coexpression of two genes on reduced dimensions"), 
   "In this tab, users can visualise the coexpression of two genes ", 
   "on low-dimensional representions.", 
   br(),br(), 
   fluidRow( 
     column( 
       3, h4("Dimension Reduction"), 
       fluidRow( 
         column( 
           12, selectInput("sc_msb2drX", "X-axis:", choices = sc_msconf[dimred == TRUE]$UI, 
                           selected = sc_msdef$dimred[1]), 
           selectInput("sc_msb2drY", "Y-axis:", choices = sc_msconf[dimred == TRUE]$UI, 
                       selected = sc_msdef$dimred[2])) 
       ) 
     ), # End of column (6 space) 
     column( 
       3, actionButton("sc_msb2togL", "Toggle to subset cells"), 
       conditionalPanel( 
         condition = "input.sc_msb2togL % 2 == 1", 
         selectInput("sc_msb2sub1", "Cell information to subset:", 
                     choices = sc_msconf[grp == TRUE]$UI, 
                    selected = sc_msdef$grp1), 
         uiOutput("sc_msb2sub1.ui"), 
         actionButton("sc_msb2sub1all", "Select all groups", class = "btn btn-primary"), 
         actionButton("sc_msb2sub1non", "Deselect all groups", class = "btn btn-primary") 
       ) 
     ), # End of column (6 space) 
     column( 
       6, actionButton("sc_msb2tog0", "Toggle graphics controls"), 
       conditionalPanel( 
         condition = "input.sc_msb2tog0 % 2 == 1", 
         fluidRow( 
           column( 
             6, sliderInput("sc_msb2siz", "Point size:", 
                            min = 0, max = 4, value = 1.25, step = 0.25), 
             radioButtons("sc_msb2psz", "Plot size:", 
                          choices = c("Small", "Medium", "Large"), 
                          selected = "Medium", inline = TRUE), 
             radioButtons("sc_msb2fsz", "Font size:", 
                          choices = c("Small", "Medium", "Large"), 
                          selected = "Medium", inline = TRUE) 
           ), 
           column( 
             6, radioButtons("sc_msb2asp", "Aspect ratio:", 
                             choices = c("Square", "Fixed", "Free"), 
                             selected = "Square", inline = TRUE), 
             checkboxInput("sc_msb2txt", "Show axis text", value = FALSE) 
           ) 
         ) 
       ) 
     )  # End of column (6 space) 
   ),   # End of fluidRow (4 space) 
   fluidRow( 
     column( 
       3, style="border-right: 2px solid black", h4("Gene Expression"), 
       selectInput("sc_msb2inp1", "Gene 1:", choices=NULL) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
               title = "Gene expression to colour cells by", 
               content = c("Select gene to colour cells by gene expression", 
                          paste0("- Gene expression are coloured in a ", 
                                 "White-Red colour scheme which can be ", 
                                 "changed in the plot controls"))), 
       selectInput("sc_msb2inp2", "Gene 2:", choices=NULL) %>% 
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Gene expression to colour cells by", 
                content = c("Select gene to colour cells by gene expression", 
                            paste0("- Gene expression are coloured in a ", 
                                   "White-Blue colour scheme which can be ", 
                                   "changed in the plot controls"))), 
       actionButton("sc_msb2tog1", "Toggle plot controls"), 
       conditionalPanel( 
         condition = "input.sc_msb2tog1 % 2 == 1", 
         radioButtons("sc_msb2col1", "Colour:", 
                      choices = c("Red (Gene1); Blue (Gene2)", 
                                  "Orange (Gene1); Blue (Gene2)", 
                                  "Red (Gene1); Green (Gene2)", 
                                  "Green (Gene1); Blue (Gene2)"), 
                      selected = "Red (Gene1); Blue (Gene2)"), 
         radioButtons("sc_msb2ord1", "Plot order:", 
                      choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                      selected = "Max-1st", inline = TRUE) 
       ) 
     ), # End of column (6 space) 
     column( 
       6, style="border-right: 2px solid black", 
       uiOutput("sc_msb2oup1.ui"), 
       downloadButton("sc_msb2oup1.pdf", "Download PDF"), 
       downloadButton("sc_msb2oup1.png", "Download PNG"), br(), 
       div(style="display:inline-block", 
           numericInput("sc_msb2oup1.h", "PDF / PNG height:", width = "138px", 
                        min = 4, max = 20, value = 8, step = 0.5)), 
       div(style="display:inline-block", 
           numericInput("sc_msb2oup1.w", "PDF / PNG width:", width = "138px", 
                        min = 4, max = 20, value = 10, step = 0.5)) 
     ), # End of column (6 space) 
     column( 
       3, uiOutput("sc_msb2oup2.ui"), 
       downloadButton("sc_msb2oup2.pdf", "Download PDF"), 
       downloadButton("sc_msb2oup2.png", "Download PNG"), 
       br(), h4("Cell numbers"), 
       dataTableOutput("sc_msb2.dt") 
     )  # End of column (6 space) 
   )    # End of fluidRow (4 space) 
 ),     # End of tab (2 space) 
 
 ### Tab1.c1: violinplot / boxplot 
 tabPanel( 
    HTML("Violinplot / Boxplot"),  
   h4("Cell information / gene expression violin plot / box plot"), 
   "In this tab, users can visualise the gene expression or continuous cell information ",  
   "(e.g. Number of UMIs / module score) across groups of cells (e.g. libary / clusters).", 
   br(),br(), 
   fluidRow( 
     column( 
       3, style="border-right: 2px solid black", 
       selectInput("sc_msc1inp1", "Cell information (X-axis):", 
                   choices = sc_msconf[grp == TRUE]$UI, 
                   selected = sc_msdef$grp1) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Cell information to group cells by",  
                content = c("Select categorical cell information to group cells by",  
                            "- Single cells are grouped by this categorical covariate",  
                            "- Plotted as the X-axis of the violin plot / box plot")),  
       selectInput("sc_msc1inp2", "Cell Info / Gene name (Y-axis):", choices=NULL) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Cell Info / Gene to plot", 
                content = c("Select cell info / gene to plot on Y-axis", 
                            "- Can be continuous cell information (e.g. nUMIs / scores)", 
                            "- Can also be gene expression")), 
       radioButtons("sc_msc1typ", "Plot type:", 
                    choices = c("violin", "boxplot"), 
                    selected = "violin", inline = TRUE), 
       checkboxInput("sc_msc1pts", "Show data points", value = FALSE), 
       actionButton("sc_msc1togL", "Toggle to subset cells"), 
       conditionalPanel( 
         condition = "input.sc_msc1togL % 2 == 1", 
         selectInput("sc_msc1sub1", "Cell information to subset:", 
                     choices = sc_msconf[grp == TRUE]$UI, 
                     selected = sc_msdef$grp1), 
         uiOutput("sc_msc1sub1.ui"), 
         actionButton("sc_msc1sub1all", "Select all groups", class = "btn btn-primary"), 
         actionButton("sc_msc1sub1non", "Deselect all groups", class = "btn btn-primary") 
       ), br(), br(), 
       actionButton("sc_msc1tog", "Toggle graphics controls"), 
       conditionalPanel( 
         condition = "input.sc_msc1tog % 2 == 1", 
         sliderInput("sc_msc1siz", "Data point size:",  
                     min = 0, max = 4, value = 1.25, step = 0.25),  
         radioButtons("sc_msc1psz", "Plot size:", 
                      choices = c("Small", "Medium", "Large"), 
                      selected = "Medium", inline = TRUE), 
         radioButtons("sc_msc1fsz", "Font size:", 
                      choices = c("Small", "Medium", "Large"), 
                      selected = "Medium", inline = TRUE)) 
     ), # End of column (6 space) 
     column(9, uiOutput("sc_msc1oup.ui"),  
            downloadButton("sc_msc1oup.pdf", "Download PDF"),  
            downloadButton("sc_msc1oup.png", "Download PNG"), br(), 
            div(style="display:inline-block", 
                numericInput("sc_msc1oup.h", "PDF / PNG height:", width = "138px", 
                             min = 4, max = 20, value = 8, step = 0.5)), 
            div(style="display:inline-block", 
                numericInput("sc_msc1oup.w", "PDF / PNG width:", width = "138px", 
                             min = 4, max = 20, value = 10, step = 0.5)) 
     )  # End of column (6 space) 
   )    # End of fluidRow (4 space) 
 ),     # End of tab (2 space) 
 
### Tab1.c2: Proportion plot 
tabPanel( 
  HTML("Proportion plot"), 
  h4("Proportion / cell numbers across different cell information"), 
  "In this tab, users can visualise the composition of single cells based on one discrete ", 
  "cell information across another discrete cell information. ",  
  "Usage examples include the library or cellcycle composition across clusters.", 
  br(),br(), 
  fluidRow( 
    column( 
      3, style="border-right: 2px solid black", 
      selectInput("sc_msc2inp1", "Cell information to plot (X-axis):", 
                  choices = sc_msconf[grp == TRUE]$UI, 
                  selected = sc_msdef$grp2) %>%  
        helper(type = "inline", size = "m", fade = TRUE, 
               title = "Cell information to plot cells by",  
               content = c("Select categorical cell information to plot cells by", 
                           "- Plotted as the X-axis of the proportion plot")), 
      selectInput("sc_msc2inp2", "Cell information to group / colour by:", 
                  choices = sc_msconf[grp == TRUE]$UI, 
                  selected = sc_msdef$grp1) %>%  
        helper(type = "inline", size = "m", fade = TRUE, 
               title = "Cell information to group / colour cells by", 
               content = c("Select categorical cell information to group / colour cells by", 
                           "- Proportion / cell numbers are shown in different colours")), 
      radioButtons("sc_msc2typ", "Plot value:", 
                   choices = c("Proportion", "CellNumbers"), 
                   selected = "Proportion", inline = TRUE), 
      checkboxInput("sc_msc2flp", "Flip X/Y", value = FALSE), 
      actionButton("sc_msc2togL", "Toggle to subset cells"), 
      conditionalPanel( 
        condition = "input.sc_msc2togL % 2 == 1", 
        selectInput("sc_msc2sub1", "Cell information to subset:", 
                    choices = sc_msconf[grp == TRUE]$UI, 
                    selected = sc_msdef$grp1), 
        uiOutput("sc_msc2sub1.ui"), 
        actionButton("sc_msc2sub1all", "Select all groups", class = "btn btn-primary"), 
        actionButton("sc_msc2sub1non", "Deselect all groups", class = "btn btn-primary") 
      ), br(), br(), 
      actionButton("sc_msc2tog", "Toggle graphics controls"), 
      conditionalPanel( 
        condition = "input.sc_msc2tog % 2 == 1", 
        radioButtons("sc_msc2psz", "Plot size:", 
                     choices = c("Small", "Medium", "Large"), 
                     selected = "Medium", inline = TRUE), 
        radioButtons("sc_msc2fsz", "Font size:", 
                     choices = c("Small", "Medium", "Large"), 
                     selected = "Medium", inline = TRUE)) 
    ), # End of column (6 space) 
    column(9, uiOutput("sc_msc2oup.ui"),  
           downloadButton("sc_msc2oup.pdf", "Download PDF"),  
           downloadButton("sc_msc2oup.png", "Download PNG"), br(), 
           div(style="display:inline-block", 
               numericInput("sc_msc2oup.h", "PDF / PNG height:", width = "138px", 
                            min = 4, max = 20, value = 8, step = 0.5)), 
           div(style="display:inline-block", 
               numericInput("sc_msc2oup.w", "PDF / PNG width:", width = "138px", 
                            min = 4, max = 20, value = 10, step = 0.5)) 
    )  # End of column (6 space) 
  )    # End of fluidRow (4 space) 
),     # End of tab (2 space) 
 
  ### Tab1.d1: Multiple gene expr 
  tabPanel( 
    HTML("Bubbleplot / Heatmap"), 
    h4("Gene expression bubbleplot / heatmap"), 
    "In this tab, users can visualise the gene expression patterns of ", 
    "multiple genes grouped by categorical cell information (e.g. library / cluster).", br(), 
    "The normalised expression are averaged, log-transformed and then plotted.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, style="border-right: 2px solid black", 
        textAreaInput("sc_msd1inp", HTML("List of gene names <br /> 
                                          (Max 50 genes, separated <br /> 
                                           by , or ; or newline):"), 
                      height = "200px", 
                      value = paste0(sc_msdef$genes, collapse = ", ")) %>% 
          helper(type = "inline", size = "m", fade = TRUE, 
                 title = "List of genes to plot on bubbleplot / heatmap", 
                 content = c("Input genes to plot", 
                             "- Maximum 50 genes (due to ploting space limitations)", 
                             "- Genes should be separated by comma, semicolon or newline")), 
        selectInput("sc_msd1grp", "Group by:", 
                    choices = sc_msconf[grp == TRUE]$UI, 
                    selected = sc_msconf[grp == TRUE]$UI[1]) %>% 
          helper(type = "inline", size = "m", fade = TRUE, 
                 title = "Cell information to group cells by", 
                 content = c("Select categorical cell information to group cells by", 
                             "- Single cells are grouped by this categorical covariate", 
                             "- Plotted as the X-axis of the bubbleplot / heatmap")), 
        radioButtons("sc_msd1plt", "Plot type:", 
                     choices = c("Bubbleplot", "Heatmap"), 
                     selected = "Bubbleplot", inline = TRUE), 
        checkboxInput("sc_msd1scl", "Scale gene expression", value = TRUE), 
        checkboxInput("sc_msd1row", "Cluster rows (genes)", value = TRUE), 
        checkboxInput("sc_msd1col", "Cluster columns (samples)", value = FALSE), 
        br(), 
        actionButton("sc_msd1togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.sc_msd1togL % 2 == 1", 
          selectInput("sc_msd1sub1", "Cell information to subset:", 
                      choices = sc_msconf[grp == TRUE]$UI, 
                      selected = sc_msdef$grp1), 
          uiOutput("sc_msd1sub1.ui"), 
          actionButton("sc_msd1sub1all", "Select all groups", class = "btn btn-primary"), 
          actionButton("sc_msd1sub1non", "Deselect all groups", class = "btn btn-primary") 
        ), br(), br(), 
        actionButton("sc_msd1tog", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.sc_msd1tog % 2 == 1", 
          radioButtons("sc_msd1cols", "Colour scheme:", 
                       choices = c("White-Red", "Blue-Yellow-Red", 
                                   "Yellow-Green-Purple"), 
                       selected = "Blue-Yellow-Red"), 
          radioButtons("sc_msd1psz", "Plot size:", 
                       choices = c("Small", "Medium", "Large"), 
                       selected = "Medium", inline = TRUE), 
          radioButtons("sc_msd1fsz", "Font size:", 
                       choices = c("Small", "Medium", "Large"), 
                       selected = "Medium", inline = TRUE)) 
      ), # End of column (6 space) 
      column(9, h4(htmlOutput("sc_msd1oupTxt")), 
             uiOutput("sc_msd1oup.ui"), 
             downloadButton("sc_msd1oup.pdf", "Download PDF"), 
             downloadButton("sc_msd1oup.png", "Download PNG"), br(), 
             div(style="display:inline-block", 
                 numericInput("sc_msd1oup.h", "PDF / PNG height:", width = "138px", 
                              min = 4, max = 20, value = 10, step = 0.5)), 
             div(style="display:inline-block", 
                 numericInput("sc_msd1oup.w", "PDF / PNG width:", width = "138px", 
                              min = 4, max = 20, value = 10, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  )      # End of tab (2 space) 
   ), 

   
br(), 
p(strong("Reference: "),"L.Loh, S.Carcy, et al. "," ",em(" "),strong(", ")," ","() ","doi:  ",a("[Link]", href = "", target="_blank"),style = "font-size: 125%;"), 
p(em("This webpage was made using "), a("ShinyCell", 
  href = "https://github.com/SGDDNB/ShinyCell",target="_blank")), 
br(),br(),br(),br(),br() 
))) 
 
 
 
 