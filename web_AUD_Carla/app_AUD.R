#libraries #####
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(markdown)
library(plotly)
library(shinycustomloader)
library(shinydlplot)
library(DT)
library(visNetwork)
library(shinyBS)
library(shinydashboard) 
library(htmltools)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(shinyjs)  # Usaremos shinyjs para inicializar

source("./global.R")

# UI OPTIONS #####

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  useShinyjs(),
  
  tags$head(
    # jQuery para inicializar los tooltips con HTML permitido
    tags$script(HTML('
      $(document).ready(function(){
        $(\'[data-toggle="tooltip"]\').tooltip({html: true});
      });
    '))
  ),
  
  align = "center",
  
  navbarPage(
    title = tags$div(
      style = "display: flex; justify-content: center; width: 100%;",
      tags$img(src = "CBL_navbar.png", height = "30px",  style = "margin-right: 10px; vertical-align: baseline;"),
      tags$span("SexEVEthOmics", style = "font-size: 20px; font-weight: bold;")
    ),
    ## HOME #####    
    tabPanel(
      title = "Home",
      icon = icon("house"),
      
      fluidRow(
        # Columna principal (ancho 9)
        column(
          width = 8,
          # Descripción arriba de la imagen
          tags$div(
            style = "text-align: left; margin-bottom: 20px;",
            h3(
              tags$strong(
                "Welcome to ",
                tags$span(style = "color: #D55E00;display: inline;margin-right: -4px;", "Sex"),  # Naranja (sex differences)
                tags$span(style = "color: #0072B2;display: inline;margin-right: -4px;", "EV"),   # Azul (EVs)
                tags$span(style = "color: #009E73;display: inline;margin-right: -4px;", "Eth"),  # Verde (ethanol)
                tags$span(style = "color: #CC79A7;display: inline;margin-right: -4px;", "Omics") # Morado (multi-omics)
              )
            ),
            tags$p(
              style = "text-align: justify; font-size: 18px; padding: 10px 20px;",
              "This platform integrates",
              tags$span(tags$strong(style = "color: #CC79A7;", "multi-omics")), "analyses to explore ",
              tags$span(tags$strong(style = "color: #D55E00;", "sex differences")),
              " in ",
              tags$span(tags$strong(style = "color: #0072B2;", "extracellular vesicles (EVs)")),
              " from plasma, focusing on the impact of ",
              tags$span(tags$strong(style = "color: #009E73;", "ethanol (alcohol)")),
              " exposure. Discover interactive ",
              tags$span(tags$strong("lipidomics")),
              " and ",
              tags$span(tags$strong("transcriptomics")),
              " data to unravel AUD-associated biomarkers."),
            
            tags$div(
              style = "background-color: #f9f9f9; border-left: 6px solid #009E73; padding: 15px 20px; margin: 10px 20px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
              tags$p(
                style = "margin: 0; font-size: 18px; text-align: justify;",
                tags$strong("SexEVEthOmics "),
                "presents the results of our analyses while offering flexible tools for users to:"
              ),
              tags$ul(
                style = "font-size: 16px; margin-left: 20px;",
                tags$li("explore"),
                tags$li("query"),
                tags$li("draw their own conclusions")
              ),
              tags$p(
                style = "margin: 0; font-size: 18px; text-align: justify;",
                "You can even adjust analysis parameters to suit your specific research goals."
              )
            )
            
            # tags$p(style = "text-align: justify; font-size: 18px; padding: 10px 20px;",
            #    "The full",tags$strong("methodology"),"applied in this study is detailed below:")
          ),
          
          # # Imagen con tooltips 
          # tags$div(
          #   style = "position: relative; display: inline-block; width: 90%;",
          #   tags$img(src = "mm.png", width = "90%"),
          #   
          #   # Botones invisibles para activar tooltips
          #   tags$button(id = "area1", style = "position: absolute; top: 1%; left: 0%; width: 100%; height: 20%; opacity: 0; border: none; background: none; text-align: justify;"),
          #   bsTooltip("area1", "Lipids and miRNAs were extracted from plasma EVs of AUD patients and healthy individuals of both sexes.", placement = "top", trigger = "hover"),
          #   
          #   tags$button(id = "area2", style = "position: absolute; top: 20%; left: 5%; width: 40%; height: 70%; opacity: 0; border: none; background: none;text-align: justify;"),
          #   bsTooltip("area2", "Lipids were extracted, quantified, and identified via LC-MS/MS, followed by data normalization for downstream integration. <br><br>Differential abundance analysis and functional interpretation of these lipidomics data were performed in a prior study.", placement = "right", trigger = "hover"),
          #   
          #   tags$button(id = "area3", style = "position: absolute; top: 20%; left: 60%; width: 40%; height: 80%; opacity: 0; border: none; background: none; text-align: justify;"),
          #   bsTooltip("area3", "The workflow includes <b>quality control</b> (FastQC, MultiQC), <b>adapter trimming</b> (cutadapt), and <b>mapping</b> (Bowtie). <br><br>Expression data is normalized, followed by <b>differential expression analysis</b> (DEA) with DESeq2. <br><br>TarBase and mirTarBase are used for <b>functional annotation</b> to link microRNAs to their target genes. KEGG and GO are used for functional information via AnnotationDbi for <b>enrichment analysis</b> with mdgsa.", placement = "left", trigger = "hover"),
          #   
          #   tags$button(id = "area4", style = "position: absolute; top: 80%; left: 20%; width: 40%; height: 20%; opacity: 0; border: none; background: none;text-align: justify;"),
          #   bsTooltip("area4", "A multi-omic integration of miRNA and lipid data was performed using the <b>N-integration</b> approach with <b>block.sPLS</b> from the mixOmics package. <br><br>miRNA and lipid expression (X) were related to experimental groups (Y), defined by AUD status and sex. <br><br> This analysis identified distinct EV-derived signatures that discriminate AUD individuals from controls, as well as <b>sex-specific molecular patterns</b>.", placement = "top", trigger = "hover")
          # )
          # tags$div(
          #   style = "margin-top: 30px; text-align: left; padding: 0 20px;",
          #   h4(icon("flask"), " EV Isolation & Characterization"),
          #   p("Methods for EV isolation and quality control procedures."),
          #   
          #   h4(icon("chart-bar"), " Lipidomics Analysis"),
          #   p("Sex-specific lipid profiles and differential abundance results."),
          #   
          #   h4(icon("dna"), " Transcriptomics Analysis"),
          #   p("Gene expression data and functional annotation results."),
          #   
          #   h4(icon("project-diagram"), " Multi-Omics Integration"),
          #   p("Integrated analysis combining lipidomics and transcriptomics data."),
          #   
          #   h4(icon("book"), "Citation"),
          #   p("Integrated analysis combining lipidomics and transcriptomics data.")
          # )
        ),  # Cierre del primer column(width = 9)
        
        # Columna lateral derecha (ancho 3)
        column(
          width = 4,
          # Usamos div con clase box en lugar del box de shinydashboard
          tags$div(
            class = "panel panel-default",
            style = "border: 1px solid #ddd; border-radius: 4px; box-shadow: 0 1px 1px rgba(0,0,0,.05);",
            tags$div(
              class = "panel-heading",
              style = "background-color: #f5f5f5; border-bottom: 1px solid #ddd; padding: 10px 15px;",
              tags$strong("Authors")
            ),
            tags$div(
              class = "panel-body",
              style = "padding: 15px; text-align: justify;",
              tags$span(tags$a(href = "https://orcid.org/0000-0003-0401-4015", "Perpiñá-Clérigues, Carla",
                               tags$img(src = "https://orcid.org/sites/default/files/images/orcid_16x16.png", 
                                        style = "width:1em; margin-right:.0em;", 
                                        alt = "ORCID iD icon"),";")),
              tags$span(tags$a(href = "https://orcid.org/0009-0001-1464-307X", "Mellado, Susana",
                               tags$img(src = "https://orcid.org/sites/default/files/images/orcid_16x16.png", 
                                        style = "width:1em; margin-right:.0em;", 
                                        alt = "ORCID iD icon"),";")),
              tags$span(tags$a(href = "https://orcid.org/0000-0003-1365-1323", "Galiana-Roselló, Cristina",
                               tags$img(src = "https://orcid.org/sites/default/files/images/orcid_16x16.png", 
                                        style = "width:1em; margin-right:.0em;", 
                                        alt = "ORCID iD icon"),";")),
              tags$span(tags$a(href = "https://orcid.org/0000-0002-7039-8398", "Kodikara, Saritha",
                               tags$img(src = "https://orcid.org/sites/default/files/images/orcid_16x16.png", 
                                        style = "width:1em; margin-right:.0em;", 
                                        alt = "ORCID iD icon"),";")),

              # tags$span(tags$a(href = "https://orcid.org/0000-0001-6789-8933", "Fernández-Regueras, María",
              #                  tags$img(src = "https://orcid.org/sites/default/files/images/orcid_16x16.png", 
              #                           style = "width:1em; margin-right:.0em;", 
              #                           alt = "ORCID iD icon"),";")),
              tags$span(tags$a(href = "https://orcid.org/0009-0004-5997-3939", "Martín-Uriales, Blanca",
                               tags$img(src = "https://orcid.org/sites/default/files/images/orcid_16x16.png", 
                                        style = "width:1em; margin-right:.0em;", 
                                        alt = "ORCID iD icon"),";")),
              tags$span(tags$a(href = "https://orcid.org/0000-0003-1269-4487", "Marcos, Miguel",
                               tags$img(src = "https://orcid.org/sites/default/files/images/orcid_16x16.png", 
                                        style = "width:1em; margin-right:.0em;", 
                                        alt = "ORCID iD icon"),";")),
              tags$span(tags$a(href = "https://orcid.org/0000-0003-3923-1116", "Lê Cao, Kim-Anh",
                               tags$img(src = "https://orcid.org/sites/default/files/images/orcid_16x16.png", 
                                        style = "width:1em; margin-right:.0em;", 
                                        alt = "ORCID iD icon"),";")),
              tags$span(tags$a(href = "https://orcid.org/0000-0001-8354-5636", "García García, Francisco",
                               tags$img(src = "https://orcid.org/sites/default/files/images/orcid_16x16.png", 
                                        style = "width:1em; margin-right:.0em;", 
                                        alt = "ORCID iD icon"),";")),
              tags$span(tags$a(href = "https://orcid.org/0000-0003-1420-631X", "Pascual, María",
                               tags$img(src = "https://orcid.org/sites/default/files/images/orcid_16x16.png", 
                                        style = "width:1em; margin-right:.0em;", 
                                        alt = "ORCID iD icon")))
            )
          ),
          
          # Segundo box convertido a div
          tags$div(
            class = "panel panel-default",
            style = "border: 1px solid #ddd; border-radius: 4px; box-shadow: 0 1px 1px rgba(0,0,0,.05); margin-top: 20px;",
            tags$div(
              class = "panel-heading",
              style = "background-color: #f5f5f5; border-bottom: 1px solid #ddd; padding: 10px 15px;",
              tags$strong("Affiliations & Funding")
            ),
            tags$div(
              class = "panel-body",
              style = "padding: 15px; text-align: center;",
              
              # Logos institucionales (dos por fila)
              tags$div(
                tags$img(src = "UBB.png", width = "250px", style = "margin: 10px; display: inline-block;"),
                tags$img(src = "CIPF.png", width = "250px", style = "margin: 10px; display: inline-block;")
              ),
              tags$div(
                tags$img(src = "medicina_odonto_2_linies.jpg", width = "400px", style = "margin: 10px; display: inline-block;")
              ),
              tags$div(
                tags$img(src = "melb.png", width = "150px", style = "margin: 10px; display: inline-block;"),
                tags$img(src = "Logo-IBSAL_bueno.jpg", width = "200px", style = "margin: 10px; display: inline-block;")
              ),
              
              tags$strong(p("Supported by", style = "margin-top: 15px;")),
              
              # Logos de apoyo (dos por fila)
              tags$div(
                tags$img(src = "GVA.png", width = "300px", style = "margin: 10px; display: inline-block;"),
                tags$img(src = "05logoUE.png", width = "150px", style = "margin: 10px; display: inline-block;")
                
              ),
              tags$div(
                tags$img(src = "pnsdapp.png", width = "150px", style = "margin: 10px; display: inline-block;"),
                tags$img(src = "JUNTA-castilla_leon.jpg", width = "200px", style = "margin: 10px; display: inline-block;"),
                tags$img(src = "01_impact_data_bktransparent_300x208.png", width = "200px", style = "margin: 10px; display: inline-block;")
              ),
              tags$div(
                tags$img(src = "02_ISCIII_FEDER_lema_horizontal.png", width = "300px", style = "margin: 10px; display: inline-block;")
              )
            )
          )
        )  # Cierre del column(width = 3)
      )  # Cierre del fluidRow
    ),  # Cierre del tabPanel
    
    # Otros tabPanels...
    ## MIRNA EXPRESSION ####
    tabPanel(title = "miRNA Expression", icon = icon("align-left", lib = "glyphicon"),
             actionButton("show_mirna_expression", "🔍 What Can I Do Here?"),
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput(
                   inputId = "contrast_select", "Select contrast:",
                   choices = c("Impact in Female (Female AUD vs Female Control)" = "Female",
                               "Impact in Male (Male AUD vs Male Control)" = "Male",
                               "Impact of Sex (IF vs IM)" = "Sex",
                               "General (AUD vs Control)" = "Case"),
                   selected = "Female"
                 ),
                 
                 sliderInput(
                   inputId = "lfc_threshold",
                   label = "Log2 Fold Change threshold:",
                   min = 0,
                   max = 5,
                   value = 1,
                   step = 0.1
                 ),
                 
                 sliderInput(
                   inputId = "pval_threshold",
                   label = "Adjusted p-value threshold:",
                   min = 0,
                   max = 0.5,
                   value = 0.05,
                   step = 0.001
                 ),
                 
                 bsTooltip(
                   id = "show_labels",
                   title = "Only applies to the static volcano plot (downloadable), not the interactive plot.",
                   placement = "right",
                   trigger = "hover"
                 ),
                 
                 checkboxInput(
                   inputId = "show_labels",
                   label = "Show top significant miRNAs",
                   value = TRUE
                 ),
                 numericInput(
                   inputId = "top_n",
                   label = "Number of miRNAs to label:",
                   min = 1,
                   max = 20,
                   value = 5
                 ),
                 
                 downloadButton("download_volcano", "Download Volcano Plot")
               ),
               
               mainPanel(
                 width = 9,
                 tabsetPanel(
                   tabPanel(
                     "Volcano Plot",
                     withLoader(
                       plotlyOutput("volcano_plot", height = "700px"),
                       type = "html",
                       loader = "loader1"
                     )
                   ),
                   tabPanel(
                     "Results Table",
                     withLoader(
                       DTOutput("deg_table"),
                       type = "html",
                       loader = "loader1"
                     )
                   ),
                   tabPanel(
                     "Summary",
                     fluidRow(
                       column(
                         width = 6,
                         height = 10,
                         h4("Upregulated miRNAs"),
                         verbatimTextOutput("up_summary")
                       ),
                       column(
                         width = 6,
                         h4("Downregulated miRNAs"),
                         verbatimTextOutput("down_summary")
                       )
                     )
                   )
                 )
               )
             )
    ),
    ## FUNCTIONAL EXP ####
    tabPanel(title = "Functional Profiling", icon = icon("braille"),
             actionButton("show_functional", "🔍 What Can I Do Here?"),
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4(tags$b("Contrasts to show:"),style = "text-align: left;"),
                 # tags$br(),  # <--- Esto añade un salto de línea/espacio
                 checkboxGroupInput("selected_contrasts", label = NULL,
                                    choices = c("IF: Impact in Female (Female AUD vs Female Control)" = "IF",
                                                "IM: Impact in Male (Male AUD vs Male Control)" = "IM",
                                                "IS: Impact of Sex (IF vs IM)" = "IS",
                                                "C: (AUD vs Control)" = "C"),
                                    selected = c("IF", "IM")) %>%
                   tagAppendAttributes(style = "text-align: left;"),
                 
                 h4(tags$b("Functional annotation:"), style = "text-align: left;"),
                 
                 h5(tags$b("Ontologies to include:"), style = "text-align: center;"),
                 
                 checkboxGroupInput("selected_ontologies", NULL,
                                    choices = c("BP", "CC", "MF", "KEGG", "REACTOME"),
                                    selected = c("BP"))%>%
                   tagAppendAttributes(style = "text-align: left;"),
                 
                 h5(tags$b("Filter terms by keyword(s):"), style = "text-align: center;"),
                 
                 textInput("keyword_filter", NULL,
                           placeholder = "e.g. stress, calcium")%>%
                   tagAppendAttributes(style = "text-align: left;"),
                 
                 h4(tags$b("Results Filters:"), style = "text-align: left;"),
                 
                 checkboxInput("require_all", "Show terms that meet filters in all selected contrasts", TRUE),
                 
                 sliderInput("lor_filter", "Minimum absolute LOR:",
                             min = 0, max = 2.5, value = 0.3, step = 0.1),
                 
                 sliderInput("padj_filter", "Adjusted p-value threshold:",
                             min = 0, max = 0.1, value = 0.05, step = 0.001),
                 
                 h4(tags$b("Output:"), style = "text-align: left;"),
                 
                 sliderInput("dotplot_height", "Plot height (pixels):",
                             min = 300, max = 1500, value = 800, step = 100),
                 
                 selectInput(
                   inputId = "dotplot_format",
                   label = "Download format:",
                   choices = c("PNG" = "png", "SVG" = "svg"),
                   selected = "svg"
                 ),
                 
                 downloadButton("download_dotplot", "Download Dotplot")
               ),
               
               mainPanel(
                 width = 9,
                 DTOutput("gsea_table"),
                 br(),
                 plotOutput("gsea_dotplot", height = "800px")
               )
             ) # cierre del layaout
    ), # cierre del tabpanel
    
    ## LIPIDOMIC ANALISYS ####
    tabPanel(title = "Lipidomics Analysis", icon = icon("align-right"),
             fluidPage(
               tags$head(
                 tags$style(HTML("
        .justified-text { text-align: justify; }
        .section-title { font-weight: bold; font-size: 20px; margin-top: 20px; }
        .subsection-title { font-weight: bold; font-size: 16px; margin-top: 15px; }
        .custom-box {
      background-color: #D81B60;  /* fuchsia */
      color: white;
      padding: 20px;
      border-radius: 8px;
      text-align: center;
      margin-bottom: 20px;
    }
    .custom-box a {
      color: white;
      font-weight: bold;
      text-decoration: underline;
    }
    .custom-box-purple {
      background-color: #6f42c1;  /* purple */
    }
      "))
               ),
               
               # 🧪 Overview
               fluidRow(
                 column(12,
                        div(class = "section-title", "🧪 Overview of Lipidomics Results in AUD"),
                        div(class = "justified-text",
                            p("This section refers to a previously published lipidomics analysis focused on plasma extracellular vesicles (EVs) from patients with Alcohol Use Disorder (AUD)."),
                            p("The analysis was conducted on the same individuals as the miRNome analysis, using normalized data for multiomics integration.")
                        )
                 )
               ),
               fluidRow(
                 column(6,
                        div(class = "custom-box",
                            icon("file-alt", "fa-2x"),
                            br(),
                            "📄 ", tags$a("Sex-based lipidomic differences in AUD", 
                                          href = "https://bsd.biomedcentral.com/articles/10.1186/s13293-024-00584-5", 
                                          target = "_blank")
                        )
                 ),
                 column(6,
                        div(class = "custom-box custom-box-purple",
                            icon("globe", "fa-2x"),
                            br(),
                            "🌐 ", tags$a("Explore the dataset and visualizations", 
                                          href = "http://bioinfo.cipf.es/sal-chronics/", 
                                          target = "_blank")
                        )
                 )
               ),
               
               # 🔍 Key Findings
               fluidRow(
                 column(12,
                        div(class = "section-title", "🔍 Key Findings at a Glance"),
                        div(class = "justified-text",
                            p("The study highlights key lipids, altered metabolic pathways, and sex-specific lipid signatures:"),
                            tags$ul(
                              tags$li("♀️ ", tags$b("Females with AUD"), 
                                      " displayed significant alterations in lysophosphatidylcholine/phosphatidylcholine lipids and phospholipase/acyltransferase activity, suggesting links to cancer progression and neuroinflammation."),
                              tags$li("♂️ ", tags$b("Males with AUD"), 
                                      " showed specific changes in sphingomyelinase, phosphodiesterase, and sphingomyelin synthase activity, pointing to hepatotoxicity."),
                              tags$li("🧬 The lipidome properties, assessed using the ", tags$b("LION algorithm"), 
                                      ", revealed sex-based differences in membrane remodeling and lipid-mediated signaling."),
                              tags$li("💡 The study employed an ", tags$b("innovative network-based approach"), 
                                      ", uncovering novel lipid targets and sex-specific clinical biomarkers in AUD.")
                            )
                        )
                 )
               )
             )
             
    ),
    ## MULTIOMIC INTEGRATION #####
    tabPanel(
      title = "Multiomics Integration", 
      icon = icon("puzzle-piece"),
      actionButton("show_integration", "🔍 What Can I Do Here?"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4(tags$b("Analysis Parameters")),
          
          # Número de componentes
          numericInput(
            inputId = "ncomp",
            label = "Number of components:",
            min = 1,
            max = 10,
            value = 3
          ),
          
          # Títulos y salidas dinámicas
          h5(tags$b("miRNA features (per component):")),
          uiOutput("mirna_ui"),
          
          h5(tags$b("Lipid features (per component):")),
          uiOutput("lipid_ui"),
          
          h5(tags$b("Y features (per component):")),
          uiOutput("y_ui"),
          
          actionButton("run_analysis", "Run Integration Analysis", 
                       icon = icon("play"), 
                       class = "btn-primary")
        ),
        
        mainPanel(
          width = 9,
          tabsetPanel(
            tabPanel("Component 1", plotOutput("loading_plot_1",click = "click_plot1")),
            tabPanel("Component 2", plotOutput("loading_plot_2",click = "click_plot2")),
            tabPanel("Component 3", plotOutput("loading_plot_3",click = "click_plot3")),
            tabPanel("Correlation", plotOutput("corplot",click = "click_corplot")),
            tabPanel("Results Summary", verbatimTextOutput("analysis_summary"))
          )
        )
      )
    ), # tabPanel
    
    ## STUDY OVERVIEW  ####
    tabPanel(title = "Study Overview", icon = icon("search"),
             tags$p(
               style = "text-align: justify; font-size: 18px; padding: 10px 20px;",
               tags$span(tags$strong(style = "color: #D55E00;display: inline;margin-right: -4px;", "Sex")),  # Naranja (sex differences)
               tags$span(tags$strong(style = "color: #0072B2;display: inline;margin-right: -4px;", "EV")),   # Azul (EVs)
               tags$span(tags$strong(style = "color: #009E73;display: inline;margin-right: -4px;", "Eth")),  # Verde (ethanol)
               tags$span(tags$strong(style = "color: #CC79A7;display: inline;margin-right: -4px;", "Omics ")), # Morado (multi-omics)
               " is an interactive platform built upon the results of a study that integrates lipidomic and transcriptomic data derived from plasma extracellular vesicles (EVs). ",
               tags$ul(style = "margin-left: 20px; font-size: 17px; text-align: left; line-height: 190%;",
                       
                       tags$li(icon("flask", style = "color: #009E73; margin: 0 6px;"),
                               "The data were obtained from individuals with alcohol use disorder (AUD) and healthy controls, including both women and men. "),
                       
                       tags$li(icon("lightbulb", style = "color: #F0AD4E; margin: 0 6px;"),
                               "The main objective of the study was to uncover molecular mechanisms underlying AUD, while accounting for sex as a biological variable—an often-overlooked factor in alcohol-related research. "),
                       
                       tags$li(icon("layer-group", style = "color: #337ab7; margin: 0 6px;"),
                               "Using a multi-omics approach combining miRNA and lipid data, we identified distinct EV-associated molecular signatures linked to AUD, some of which reflect sex-specific alterations. "),
                       
                       tags$li(icon("info-circle", style = "color: #5bc0de; margin: 0 6px;"),
                               " This section provides an overview of the data generation process, a description of the samples and the different approaches adopted, as well as the main results obtained. It also covers the implemented code.")
               )
             ),
             mainPanel(
               width = 10,
               tabsetPanel(
                 #### Methods #####
                 tabPanel("Methodology", 
                          tags$p(style = "text-align: justify; font-size: 18px; padding: 10px 20px;",
                                 "The full",tags$strong("methodology"),"applied in this study is detailed below:"),
                          # Imagen con tooltips 
                          tags$div(
                            style = "position: relative; display: inline-block; width: 90%;",
                            tags$img(src = "mm.png", width = "80%"),
                            
                            # Tooltip 1
                            tags$button(
                              `data-toggle` = "tooltip",
                              `data-placement` = "bottom",
                              title = "Lipids and miRNAs were extracted from plasma EVs of AUD patients and healthy individuals of both sexes.",
                              style = "position: absolute; top: 1%; left: 0%; width: 100%; height: 20%; opacity: 0; border: none; background: none;"
                            ),
                            
                            # Tooltip 2
                            tags$button(
                              `data-toggle` = "tooltip",
                              `data-placement` = "right",
                              title = "Lipids were extracted, quantified, and identified via LC-MS/MS, followed by data normalization for downstream integration. <br><br>Differential abundance analysis and functional interpretation of these lipidomics data were performed in a prior study.",
                              style = "position: absolute; top: 20%; left: 5%; width: 40%; height: 70%; opacity: 0; border: none; background: none;"
                            ),
                            
                            # Tooltip 3
                            tags$button(
                              `data-toggle` = "tooltip",
                              `data-placement` = "left",
                              title = "The workflow includes <b>quality control</b> (FastQC, MultiQC), <b>adapter trimming</b> (cutadapt), and <b>mapping</b> (Bowtie). <br><br>Expression data is normalized, followed by <b>differential expression analysis</b> (DEA) with DESeq2. <br><br>TarBase and mirTarBase are used for <b>functional annotation</b> to link microRNAs to their target genes. KEGG and GO are used for functional information via AnnotationDbi for <b>enrichment analysis</b> with mdgsa.",
                              style = "position: absolute; top: 20%; left: 60%; width: 40%; height: 80%; opacity: 0; border: none; background: none;"
                            ),
                            
                            # Tooltip 4
                            tags$button(
                              `data-toggle` = "tooltip",
                              `data-placement` = "right",
                              title = "A multi-omic integration of miRNA and lipid data was performed using the <b>N-integration</b> approach with <b>block.sPLS</b> from the mixOmics package. <br><br>miRNA and lipid expression (X) were related to experimental groups (Y), defined by AUD status and sex. <br><br> This analysis identified distinct EV-derived signatures that discriminate AUD individuals from controls, as well as <b>sex-specific molecular patterns</b>.",
                              style = "position: absolute; top: 80%; left: 20%; width: 40%; height: 20%; opacity: 0; border: none; background: none;"
                            )
                          )
                 ),
                 #### Samples #####
                 tabPanel("Samples",
                          icon = icon("table"),
                          fluidRow(
                            column(
                              12,
                              tags$h4("Cross-Omic Sample Correspondence"),
                              tags$p(
                                "This table shows how samples from different omics (lipids and miRNAs) correspond to the same individuals. ",
                                "Only those present in both omics could be included in the integrated multi-omic analysis."
                              ),
                              DTOutput("sample_table")
                            )
                          )
                 ),
                 
                 #### Results table #####
                 tabPanel("Results Summary",
                          tabsetPanel(
                            tabPanel("Signatures",
                                     fluidRow(
                                       column(
                                         width = 12,
                                         tags$h4("Multi-omics Signatures Identified via Integration Analysis"),
                                         tags$p(style = "text-align: justify;","We identified an ", tags$b("AUD Signature"), "with predictive potential for both diagnostic and prognostic applications. 
                                              While individual features such as hsa-miR-99b-3p, hsa-miR-556-5p, Cer_NDS d39:1, and PI 18:0_18:2 illustrate components of this signature, 
                                              its strength lies in the integrated profile rather than isolated markers. Additionally, we revealed a", tags$b("AUD-Sex Signature"), " providing insights into how biological responses to alcohol differ between women and men. This signature includes features like hsa-miR-1301-3p and PC 39:4, further underscoring the power of multi-feature integration."
                                         ),
                                         tags$hr()
                                       )
                                     ),
                                     fluidRow(
                                       column(
                                         width = 6,
                                         tags$h5("Table 1. AUD Signature (Component 1)"),
                                         DT::dataTableOutput("table_signature1")
                                       ),
                                       column(
                                         width = 6,
                                         tags$h5("Table 2. AUD-Sex Signature (Component 3)"),
                                         DT::dataTableOutput("table_signature2")
                                       )
                                     )
                            ),
                            
                            tabPanel("miRNA Results Table",  # Segundo panel del tabset
                                     
                                     sidebarLayout(
                                       sidebarPanel(
                                         textInput("selected_mirna", "Enter microRNA name:", value = ""),
                                         uiOutput("suggestions"),
                                         actionButton("plot_button", "Generate Boxplot"),
                                         tags$br(),  # Espacio
                                         helpText("The boxplot will appear below the table."),
                                         width = 3
                                       ),
                                       mainPanel(
                                         DTOutput("table_mirna"),
                                         plotOutput("boxplot")
                                       )
                                     )
                            ),
                            tabPanel("Common Target Genes",  # Tercer panel del tabset
                                     sidebarLayout(
                                       sidebarPanel(
                                         textAreaInput("selected_mirnas", 
                                                       "Enter microRNAs (comma separated):", 
                                                       value = "", 
                                                       rows = 5, 
                                                       placeholder = "miR-1, miR-2, miR-3"),
                                         actionButton("common_targets_button", "Find Common Targets"),
                                         width = 3
                                       ),
                                       mainPanel(
                                         DTOutput("common_targets_table")
                                       )
                                     )
                            )
                          ),
                 ),
                 #### Code #####
                 tabPanel("Code")
               )
             )
    ),
    
    ## HELP ####
    tabPanel(
      title = "Help",
      icon = icon("question"),
      tags$iframe(src = "help.html",
                  width = "100%", height = "4200px",
                  frameborder = 0, scrolling = "no")
    )
  ),  # Cierre del navbarPage
  
  # Pie de página no fijo (aparece al final del contenido)
  tags$footer(
    style = "background-color: #f8f9fa; padding: 15px; text-align: center; 
             border-top: 1px solid #ddd; font-size: 0.9em; margin-top: 30px;",
    HTML("
      <div style='max-width: 800px; margin: 0 auto;'>
        SexEVEthOmics is a tool developed by Carla Perpiñá Clérigues and Blanca Martín Urdiales in the 
        <a href='https://bioinfo.cipf.es/cbl/tools/' target='_blank'>Computational Biomedicine Laboratory and University of Valencia</a> |  
        License: <a href='https://opensource.org/licenses/MIT' target='_blank'>MIT</a> | 
        <a href='https://doi.org/10.1186/s13293-024-00584-5' target='_blank'>Perpiñá-Clérigues, C., Mellado, S., Galiana-Roselló, C. et al. Multi-omics integration reveals sex-based differences in the circulating extracellular vesicle lipidome and miRNome of alcohol use disorder patients. [preprint].</a>
      </div>
    ")
  )
)  # Cierre del fluidPage

# SERVER OPTIONS ####
server <- function(input, output, session) {
  ## MIRNA EXPRESSION ####
  observeEvent(input$show_mirna_expression,{ 
    showModal( 
      modalDialog(  title = "🔍 What Can I Do Here?",
                    HTML(paste(
                      "<p>In this panel, you can explore the <b>differential expression of miRNAs</b> across four comparisons:</p>
                    <ul> <li><b>Impact in Female (IF):</b> AUD vs Control in females</li> <li><b>Impact in Male (IM):</b> 
                    AUD vs Control in males</li> <li><b>Impact of Sex (IS):</b> IF vs IM</li> 
                    <li><b>General:</b> AUD vs Control (regardless of sex)</li> </ul> 
                    <hr> <p><b>🎯 Customize your volcano plot using:</b></p> <ul> <li> <b>Log2 Fold Change threshold</b> – 
                    to highlight miRNAs with large expression changes</li> <li> <b>Adjusted p-value threshold</b> – to control 
                    for statistical significance</li> <li> <b>Show top significant miRNAs</b> – optionally label the most 
                    significant ones in the exported figure</li> </ul> <hr> <p><b>📊 Explore results in three ways:</b></p> 
                    <ul> <li>🔍 <b>Volcano Plot</b> – interactive visualization of significance vs fold change</li> 
                    <li>📋 <b>Results Table</b> – sortable and downloadable list of all miRNAs</li> <li>📈 <b>Summary</b> – 
                    counts and top 5 upregulated/downregulated miRNAs</li> </ul> <hr> <p><b>⬇️ Export your results:</b></p> 
                    <ul> <li>Download the volcano plot as PNG (labels apply only to this static version)</li> 
                    <li>Export the full table as CSV or Excel</li> </ul>"
                    )),
                    easyClose = TRUE,
                    footer = modalButton("Dismiss")
      ))
  })
  
  load("data/mirna_exp/mirna_exp_res.RData")
  deg_data <- reactive({
    contrast <- input$contrast_select
    # Datos
    
    df <- res_list[[contrast]] %>%
      as.data.frame() %>%
      dplyr::rename(c(miRNA = mirna, log2FC = log2FoldChange))
    return(df)
  })
  
  output$volcano_plot <- renderPlotly({
    req(deg_data())
    df <- deg_data()
    lfc_thresh <- input$lfc_threshold
    pval_thresh <- input$pval_threshold
    
    df$color <- ifelse(
      df$padj < pval_thresh & abs(df$log2FC) > lfc_thresh,
      ifelse(df$log2FC > 0, "Upregulated", "Downregulated"),
      "Not significant"
    )
    
    p <- ggplot(df, aes(x = log2FC, y = -log10(padj), color = color, text = miRNA)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = -log10(pval_thresh), linetype = "dashed") +
      geom_vline(xintercept = c(-lfc_thresh, lfc_thresh), linetype = "dashed") +
      scale_color_manual(values = c("Upregulated" = "red", "Downregulated" = "blue", "Not significant" = "grey")) +
      labs(x = "log2 Fold Change", y = "-log10(adjusted p-value)") +
      theme_minimal()
    
    if(input$show_labels) {
      top_up <- df %>% 
        filter(color == "Upregulated") %>% 
        arrange(padj) %>% 
        head(input$top_n)
      
      top_down <- df %>% 
        filter(color == "Downregulated") %>% 
        arrange(padj) %>% 
        head(input$top_n)
      
      p <- p + 
        geom_text_repel(
          data = rbind(top_up, top_down),
          aes(label = miRNA),
          size = 3,
          box.padding = 0.5
        )
    }
    
    ggplotly(p, tooltip = c("text", "x", "y"))
  })
  
  output$deg_table <- renderDT({
    df <- deg_data()
    df <- df %>% mutate_if(is.numeric, ~ round(., 4))
    datatable(
      df,
      rownames = F,
      extensions = 'Buttons',
      filter = 'top',
      options = list(
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel'),
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50), c('10', '25', '50'))
      )
    )
  })
  
  output$up_summary <- renderPrint({
    df <- deg_data()
    up <- df %>% 
      filter(padj < input$pval_threshold, log2FC > input$lfc_threshold) %>% 
      arrange(padj)
    
    cat(nrow(up), "upregulated miRNAs\n")
    if(nrow(up) > 0) {
      cat("Top 5:\n")
      print(head(up$miRNA, 5))
    }
  })
  
  output$down_summary <- renderPrint({
    df <- deg_data()
    down <- df %>% 
      filter(padj < input$pval_threshold, log2FC < -input$lfc_threshold) %>% 
      arrange(padj)
    
    cat(nrow(down), "downregulated miRNAs\n")
    if(nrow(down) > 0) {
      cat("Top 5:\n")
      print(head(down$miRNA, 5))
    }
  })
  
  output$download_volcano <- downloadHandler(
    filename = function() {
      paste("volcano_", input$contrast_select, ".png", sep = "")
    },
    content = function(file) {
      df <- deg_data()
      lfc_thresh <- input$lfc_threshold
      pval_thresh <- input$pval_threshold
      
      df$color <- ifelse(
        df$padj < pval_thresh & abs(df$log2FC) > lfc_thresh,
        ifelse(df$log2FC > 0, "Upregulated", "Downregulated"),
        "Not significant"
      )
      
      p <- ggplot(df, aes(x = log2FC, y = -log10(padj), color = color)) +  # Corregido: paréntesis de cierre para aes()
        geom_point(alpha = 0.6) +
        geom_hline(yintercept = -log10(pval_thresh), linetype = "dashed") +
        geom_vline(xintercept = c(-lfc_thresh, lfc_thresh), linetype = "dashed") +
        scale_color_manual(values = c("Upregulated" = "red", "Downregulated" = "blue", "Not significant" = "grey")) +
        labs(x = "log2 Fold Change", y = "-log10(adjusted p-value)") +
        theme_minimal()
      
      if(input$show_labels) {
        top_up <- df %>% 
          filter(color == "Upregulated") %>% 
          arrange(padj) %>% 
          head(input$top_n)
        
        top_down <- df %>% 
          filter(color == "Downregulated") %>% 
          arrange(padj) %>% 
          head(input$top_n)
        
        p <- p + 
          geom_text_repel(
            data = rbind(top_up, top_down),
            aes(label = miRNA),
            size = 3,
            box.padding = 0.5
          )
      }
      
      ggsave(file, plot = p, device = "png", width = 10, height = 8)
    }
  )
  ## FUNCTIONAL ENRICHMENT ####
  observeEvent(input$show_functional,{ 
    showModal( 
      modalDialog(  title = "🔍 What Can I Do Here?",
                    HTML(paste(
                      "<p style='text-align: justify;'>🧬 <b>Functional Profiling Panel</b><br>
This section allows you to explore and filter functional enrichment results from five annotation sources:<br>
<ul>
<li><a href='https://geneontology.org/' target='_blank'><b>GO (Gene Ontology):</b></a><br>
  - BP: Biological Processes<br>
  - CC: Cellular Components<br>
  - MF: Molecular Functions.<br></li>
  
<li><a href='https://www.genome.jp/kegg/' target='_blank'><b>KEGG Pathways</b></a><br></li>
<li><a href='https://reactome.org/' target='_blank'><b>Reactome</b></a></li>
                        </ul></p>",
                      
                      "<hr><p style='text-align: justify;'>📂 <b>Comparisons:</b><br>
You can browse functional results for each of the four experimental comparisons from the study and compare them with your own findings.</p>",
                      
                      "<hr><p style='text-align: justify;'>🔍 <b>Filters:</b><br>
Refine the results using:
<ul>
<li>🔑 Keyword search (e.g., inflammation, synapse)</li>
<li>📈 Log odds ratio (effect size)</li>
<li>📉 Adjusted p-value (significance)</li>
</ul></p>",
                      
                      "<hr><p style='text-align: justify;'>💾 <b>Export:</b><br>
Customize the plot height and download the figure in <code>PNG</code> or <code>SVG</code> format for publication or reporting.</p>")),
                    easyClose = TRUE,
                    footer = modalButton("Dismiss")
      ))
  }) 
  
  load("data/functional/gsea_all.RData")
  
  filtered_gsea <- reactive({
    req(gsea_all)
    
    df <- gsea_all %>%
      filter(
        Ontology %in% input$selected_ontologies,
        Contrast %in% input$selected_contrasts,
        abs(LOR) >= input$lor_filter,
        padj < input$padj_filter
      )
    
    if (input$keyword_filter != "") {
      keywords <- strsplit(tolower(input$keyword_filter), ",\\s*")[[1]]
      df <- df %>% filter(
        sapply(keywords, function(k) grepl(k, tolower(Term))) %>% apply(1, any)
      )
    }
    
    if (input$require_all) {
      df <- df %>%
        group_by(Term) %>%
        filter(all(input$selected_contrasts %in% Contrast)) %>%
        ungroup()
    }
    
    return(df)
  })
  
  output$gsea_table <- renderDT({
    df <- filtered_gsea()
    df <- df %>% mutate_if(is.numeric, ~ round(., 4))
    
    datatable(
      df,
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',  # Añade botones y filtros
        buttons = c('copy', 'csv', 'excel', 'pdf'),
        autoWidth = F,
        scrollX = TRUE,
        filter = 'top'   # Filtros en la parte superior
      ),
      extensions = c('Buttons'),
      rownames = FALSE
    )
    
  })
  
  output$gsea_dotplot <- renderPlot({
    df <- filtered_gsea()
    validate(need(nrow(df) > 0, "No enriched terms match the filters."))
    
    df$Contrast <- factor(df$Contrast, levels = c("IF", "IM", "IS", "C"))
    df$OntologyLabel <- "Ontology"  # columna ficticia para el tile
    
    ggplot(df, aes(x = Contrast, y = Term)) +
      geom_point(
        aes(fill = LOR),
        shape = 21,
        color = "black",
        stroke = 0.1,
        size = 5
      ) +
      scale_fill_gradient2(
        low = "deepskyblue4",
        high = "red",
        midpoint = 0,
        name = "LOR"
      ) +
      facet_wrap(~ Ontology, scales = "free_y", ncol = 1) +
      theme_bw(base_size = 12) +
      labs(x = "Comparison", y = "Enriched Term") +
      theme(
        strip.text = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(face = "bold", size = 13)
      )
  }, height = function() { input$dotplot_height })
  
  ### FUNCTIONAL ENRICHMENT - DOTPLOT DOWNLOAD ####
  output$download_dotplot <- downloadHandler(
    filename = function() {
      paste("functional_dotplot_", Sys.Date(), ".", input$dotplot_format, sep = "")
    },
    content = function(file) {
      df <- filtered_gsea()
      validate(need(nrow(df) > 0, "No enriched terms match the filters."))
      
      df$Contrast <- factor(df$Contrast, levels = c("IF", "IM", "IS", "C"))
      df$OntologyLabel <- "Ontology"
      
      p <- ggplot(df, aes(x = Contrast, y = Term)) +
        geom_point(
          aes(fill = LOR),
          shape = 21,
          color = "black",
          stroke = 0.1,
          size = 5
        ) +
        scale_fill_gradient2(
          low = "deepskyblue4",
          high = "red",
          midpoint = 0,
          name = "LOR"
        ) +
        facet_wrap(~ Ontology, scales = "free_y", ncol = 1) +
        theme_bw(base_size = 12) +
        labs(x = "Comparison", y = "Enriched Term") +
        theme(
          strip.text = element_text(size = 13, face = "bold"),
          axis.text.y = element_text(size = 10)
        )
      
      # Ajustar altura dinámica para la descarga
      height <- max(6, min(20, nrow(df) * 0.3))
      
      ggsave(file, plot = p, device = input$dotplot_format, 
             width = 10, height = height, units = "in", dpi = 300)
    }
  )
  
  # MULTIOMIC INTEGRATION ####
  
  ### help ####
  
  observeEvent(input$show_integration,{ 
    showModal( 
      modalDialog(  title = "🔍 What Can I Do Here?",
                    HTML(paste(
                      "<p style='text-align: justify;'>🚀 <b>Multi-omics Integration Panel</b><br>
This section allows you to integrate miRNA and lipid data using the <code>block.sPLS</code> method from the <i>mixOmics</i> package ( 
                      <a href= 'https://doi.org/10.1371/journal.pcbi.1005752'target='_blank'><i>Rohart et al., 2017</i></a> ). You can customize the number of components and features, and launch your own integration.</p>",
                      
                      "<hr><p style='text-align: justify;'>🧾 <b>Input matrices:</b><br>
• <b>X</b> (predictors): miRNA + lipid expression levels<br>
• <b>Y</b> (response): dummy matrix encoding AUD condition and sex</p>",
                      
                      "<hr><p style='text-align: justify;'>⚙️ <b>Default model setup:</b><br>
• Components: 3<br>
• <b>X</b> (miRNA & lipid features): 20, 20, 10<br>
• <b>Y</b> (group features): 6, 6, 4<br>
These default values were used in the study associated with this web platform.</p>",
                      
                      "<hr><p style='text-align: justify;'>📊 <b>Visualization tools:</b><br>
• <code>plotLoadings</code>: identify top contributing features<br>
• <code>plotVar</code>: explore cross-omics correlations</p>",
                      
                      "<hr><p style='text-align: justify;'>📚 <b>Learn more:</b><br>
<a href='https://mixomics.org/methods/multiblock-spls/' target='_blank'>
https://mixomics.org/methods/multiblock-spls/</a></p>"
                    )),
                    easyClose = TRUE,
                    footer = modalButton("Dismiss")
      ))
  })
  
  library(mixOmics)
  load("data/integration/X.RData")
  load("data/integration/Y.RData")
  
  
  # Reactive value to store the analysis results
  block_spls_results <- reactiveVal(NULL)
  
  # Observe the run button
  
  
  observeEvent(input$run_analysis, {
    req(X, Y)
    
    showModal(modalDialog("Running integration analysis...", footer = NULL))
    
    n <- input$ncomp
    
    list.keepX <- list(
      mirna = sapply(1:n, function(i) input[[paste0("mirna_comp", i)]]),
      lipid = sapply(1:n, function(i) input[[paste0("lipid_comp", i)]])
    )
    
    list.keepY <- sapply(1:n, function(i) input[[paste0("y_comp", i)]])
    
    result <- tryCatch({
      model <- block.spls(
        X,
        Y$Y.dummy,
        design = "full",
        keepX = list.keepX,
        keepY = list.keepY,
        ncomp = input$ncomp
      )
      
      # Blanca: esto es lo que he añadido necesario para que analysis_summary funcione
      # Añadir los parámetros 'keepX' y keepY como atributo del objeto 'model'
      # Esto almacena la lista de variables seleccionadas para cada componente y tipo (mirna, lipid, Y)
      
      attr(model, "keepX") <- list.keepX
      attr(model, "keepY") <- list.keepY
      
      # Ahora el objeto 'model' modificado incluye estos atributos extras
      model
      
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return(NULL)
    })
    
    block_spls_results(result)
    removeModal()
  })
  
  
  
  # Render loading plots
  output$loading_plot_1 <- renderPlot({
    req(block_spls_results())
    plotLoadings(
      block_spls_results(), 
      method = 'mean', 
      contrib = 'max', 
      size.title = 1,
      comp = 1
    )
  })
  
  output$loading_plot_2 <- renderPlot({
    req(block_spls_results())
    plotLoadings(
      block_spls_results(), 
      method = 'mean', 
      contrib = 'max', 
      size.title = 1,
      comp = 2
    )
  })
  
  output$loading_plot_3 <- renderPlot({
    req(block_spls_results())
    plotLoadings(
      block_spls_results(), 
      method = 'mean', 
      contrib = 'max', 
      size.title = 1,
      comp = 3
    )
  })
  
  output$corplot <- renderPlot({
    req(block_spls_results())
    plotVar(
      block_spls_results(), 
      cutoff = 0.5,
      var.names = T,
      pch = c(16, 17,15), cex = c(4,4,6),
      title = "PLS Correlations - cutoff of 0.5")
  }, height = 570, width = 750)
  
  # Modal para loading_plot_1
  observeEvent(input$click_plot1, {
    showModal(modalDialog(
      title = "Loadings Plot – Component 1",
      plotOutput("large_plot1", height = "600px"),
      size = "l", easyClose = TRUE
    ))
  })
  output$large_plot1 <- renderPlot({
    req(block_spls_results())
    plotLoadings(block_spls_results(), method = 'mean', contrib = 'max', size.title = 1, comp = 1)
  })
  
  # Modal para loading_plot_2
  observeEvent(input$click_plot2, {
    showModal(modalDialog(
      title = "Loadings Plot – Component 2",
      plotOutput("large_plot2", height = "600px"),
      size = "l", easyClose = TRUE
    ))
  })
  output$large_plot2 <- renderPlot({
    req(block_spls_results())
    plotLoadings(block_spls_results(), method = 'mean', contrib = 'max', size.title = 1, comp = 2)
  })
  
  # Modal para loading_plot_3
  observeEvent(input$click_plot3, {
    showModal(modalDialog(
      title = "Loadings Plot – Component 3",
      plotOutput("large_plot3", height = "500px",width = "100%"),
      size = "l", easyClose = TRUE
    ))
  })
  output$large_plot3 <- renderPlot({
    req(block_spls_results())
    plotLoadings(block_spls_results(), method = 'mean', contrib = 'max', size.title = 1, comp = 3)
  })
  
  # Modal para corplot
  observeEvent(input$click_corplot, {
    showModal(modalDialog(
      title = "PLS Correlation Plot",
      plotOutput("large_corplot", height = "650px", width = "100%"),
      size = "l", easyClose = TRUE
    ))
  })
  output$large_corplot <- renderPlot({
    req(block_spls_results())
    plotVar(
      block_spls_results(), 
      cutoff = 0.5,
      var.names = T,
      pch = c(16, 17,15), cex = c(4,4,6),
      title = "PLS Correlations - cutoff of 0.5"
    )
  })
  
  # Analysis summary
  output$analysis_summary <- renderPrint({
    req(block_spls_results())
    res <- block_spls_results()
    
    cat("Multi-Omics Integration Results Summary\n")
    cat("======================================\n\n")
    cat("Method: block.spls\n")
    cat("Number of components:", res$ncomp, "\n")
    cat("Design: full\n\n")
    
    # Blanca: esto es lo que he añadido para que results summary funcione: 
    # Extrae los atributos personalizados "keepX" y "keepY" del objeto resultante del análisis.
    # Estos atributos son los añadidos manualmente al objeto `model` al final de la función `block.spls`.
    # No forman parte del objeto original, pero son necesarios para mostrar cuántas variables fueron seleccionadas por componente.
    
    keepX <- attr(res, "keepX")
    keepY <- attr(res, "keepY")
    
    cat("Features selected per component:\n")
    cat("- miRNA:", paste(keepX$mirna, collapse = ", "), "\n")
    cat("- Lipids:", paste(keepX$lipid, collapse = ", "), "\n")
    cat("- Y:", paste(keepY, collapse = ", "), "\n\n")
    
    cat("Variance explained:\n")
    print(res$prop_expl_var)
  })
  # Genera los inputs dinámicos para miRNA
  output$mirna_ui <- renderUI({
    n <- input$ncomp
    req(n)
    
    # Valores por defecto para los primeros 3 componentes
    default_vals <- c(20, 20, 10)
    
    fluidRow(
      lapply(1:n, function(i) {
        column(
          4,
          numericInput(
            inputId = paste0("mirna_comp", i),
            label = paste("Comp", i),
            value = ifelse(i <= length(default_vals), default_vals[i], 10),
            min = 1
          )
        )
      })
    )
  })
  
  # Inputs dinámicos para lípidos
  output$lipid_ui <- renderUI({
    n <- input$ncomp
    req(n)
    
    default_vals <- c(20, 20, 10)
    
    fluidRow(
      lapply(1:n, function(i) {
        column(
          4,
          numericInput(
            inputId = paste0("lipid_comp", i),
            label = paste("Comp", i),
            value = ifelse(i <= length(default_vals), default_vals[i], 10),
            min = 1
          )
        )
      })
    )
  })
  
  
  # Inputs dinámicos para Y
  output$y_ui <- renderUI({
    n <- input$ncomp
    req(n)
    
    default_vals <- c(6, 6, 4)
    
    fluidRow(
      lapply(1:n, function(i) {
        column(
          4,
          numericInput(
            inputId = paste0("y_comp", i),
            label = paste("Comp", i),
            value = ifelse(i <= length(default_vals), default_vals[i], 4),
            min = 1
          )
        )
      })
    )
  })

  
  ## STUDY OVERVIEW #####
  # Load sample table
  load("data/samples/samples.RData")  # This should load 'sample_df'
  
  output$sample_table <- renderDT({
    datatable(
      sample_df,
      rownames = FALSE,
      options = list(
        pageLength = nrow(sample_df),
        autoWidth = TRUE,
        dom = 'tip'
      )
    ) %>%
      formatStyle(
        columns = names(sample_df)[1:3],
        valueColumns = names(sample_df)[1:3],
        backgroundColor = styleEqual(NA, "#f2dede"),  # light red for missing values
        color = styleEqual(NA, "black")
      )
  })
  
  ### signatures #####
  data <- read.csv("data/integration/aud_signature_component1.csv")
  data2 <- read.csv("data/integration/aud_sex.csv")
  
  
  output$table_signature1 <- renderDT({
    datatable(data, rownames = F, escape = FALSE, options = list(pageLength = 50)) %>%
      formatStyle(
        "Pattern",
        target = "cell",
        backgroundColor = styleEqual(
          c("+", "-"),
          c("#d4f4dd", "#f8d7da")
        ),
        color = styleEqual(
          c("+", "-"),
          c("darkgreen", "darkred")
        )
      ) %>%
      formatStyle(
        "DEA_DAA",
        target = "cell",
        backgroundColor = styleEqual(
          c("F", "M", "F & M"),
          c("#a1cca1ff", "#f8f8a3ff", "#fff3cd")
        )
      )
  })
  
  output$table_signature2 <- renderDT({
    datatable(data2, rownames = F, escape = FALSE, options = list(pageLength = 50)) %>%
      formatStyle(
        "PATTERN",
        target = "cell",
        backgroundColor = styleEqual(
          c("I", "II", "III"),
          c("lightgrey", "darkgrey", "grey")
        )
      )
  })
  
  
  ### mirna exp ####
  
  load("data/mirna_exp/final_df.RData")
  load("data/mirna_exp/all_info.RData")
  
  load("data/mirna_exp/normalized_counts.RData")
  load("data/mirna_exp/colData.RData")
  
  
  
  mirna_names <- rownames(normalized_counts)
  
  final_df <- final_df %>% mutate_if(is.numeric, ~ round(., 4))
  
  output$table_mirna <- renderDT({
    datatable(final_df, 
              extensions = 'Buttons',
              rownames = FALSE,  
              filter = "top",  
              options = list(
                dom = 'Bfrtip', 
                pageLength = 15,
                lengthMenu = list(c(10, 25, 50, 100), c('10', '25', '50', '100')),
                autoWidth = TRUE,
                buttons = c('colvis', 'copy', 'excel', 'pdf')
              )) %>%
      
      # Highlight padj < 0.05 (significant)
      formatStyle('padj_IF', 
                  backgroundColor = styleInterval(0.05, c("#FFEDC2", NA))) %>%
      formatStyle('padj_IM', 
                  backgroundColor = styleInterval(0.05, c("#FFEDC2", NA))) %>%
      formatStyle('padj_IS', 
                  backgroundColor = styleInterval(0.05, c("#FFEDC2", NA))) %>%
      
      # Highlight log2FC direction
      formatStyle('log2FoldChange_IF', 
                  backgroundColor = styleInterval(0, c("#C2EDFF", "#FBB39D"))) %>%
      formatStyle('log2FoldChange_IM', 
                  backgroundColor = styleInterval(0, c("#C2EDFF", "#FBB39D"))) %>%
      formatStyle('log2FoldChange_IS', 
                  backgroundColor = styleInterval(0, c("#C2EDFF", "#FBB39D")))
  })
  
  # Suggest microRNAs as user types
  output$suggestions <- renderUI({
    req(input$selected_mirna)
    search_term <- input$selected_mirna
    
    # Filter names that match the input
    matches <- mirna_names[grepl(search_term, mirna_names, ignore.case = TRUE)]
    
    if (length(matches) > 0) {
      suggestions <- tags$ul(
        lapply(matches, function(mirna) {
          tags$li(
            a(href = "#", onclick = sprintf(
              "Shiny.setInputValue('selected_mirna', '%s', {priority: 'event'});", mirna
            ), mirna)
          )
        })
      )
      suggestions
    }
  })
  
  # Generate boxplot when button is clicked
  observeEvent(input$plot_button, {
    req(input$selected_mirna)
    selected_mirna <- input$selected_mirna
    
    if (!(selected_mirna %in% rownames(normalized_counts))) {
      showNotification("microRNA not found. Please check the name.", type = "error")
      return()
    }
    
    gene_counts <- normalized_counts[selected_mirna, ]
    
    gene_df <- data.frame(
      Sample = colnames(normalized_counts),
      Count = as.numeric(gene_counts),
      Condition = metadata_colData$Condition  # Adjust if your metadata uses a different name
    )
    
    output$boxplot <- renderPlot({
      ggplot(gene_df, aes(x = Condition, y = Count, fill = Condition)) +
        geom_boxplot() +
        labs(title = paste("Abundance of", selected_mirna),
             x = "Condition", y = "Normalized Counts") +
        theme_minimal() +
        theme(legend.position = "none")
    })
  })
  
  # Generate common target genes table upon button click
  observeEvent(input$common_targets_button, {
    req(input$selected_mirnas)
    
    selected_mirnas <- strsplit(input$selected_mirnas, ",")[[1]]
    selected_mirnas <- trimws(selected_mirnas)
    
    if (length(selected_mirnas) > 20) {
      showNotification("Please select a maximum of 20 microRNAs.", type = "error")
      return()
    }
    
    target_lists <- lapply(selected_mirnas, function(mirna) {
      unique.mirna.union$target_symbol[unique.mirna.union$mature_mirna_id == mirna]
    })
    
    valid_targets <- target_lists[!sapply(target_lists, is.null)]
    
    if (length(valid_targets) < length(selected_mirnas)) {
      showNotification("Some microRNAs were not found. Please check the names.", type = "warning")
    }
    
    if (length(valid_targets) > 0) {
      common_targets <- Reduce(intersect, valid_targets)
    } else {
      common_targets <- character(0)
    }
    
    common_targets_df <- data.frame(Common_Targets = common_targets)
    
    output$common_targets_table <- renderDT({
      req(common_targets_df)
      
      datatable(
        common_targets_df,
        rownames = FALSE,
        options = list(
          dom = 'Blfti',  # B=botones, l=lenght, f=filtro, t=tabla, i=información
          buttons = c('copy'),
          language = list(
            info = "_TOTAL_ target genes",  # Resumen simple
            search = "Filter:"
          )
        ),
        filter = 'top'
      )
    })
  })
  
  ## OTHER TAB SECTIONS ####
  # Añade aquí los observers o outputs para las otras pestañas
  
}

shinyApp(ui, server)
