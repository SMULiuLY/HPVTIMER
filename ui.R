
dashboardPage(
  title= "HPVTIMER",
  dashboardHeader(title = "HPVTIMER"),
  
  ##======================================Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction", icon = icon("home")),
      menuItem("DE analysis", tabName = "Differentexpression", icon = icon("atom"),
               menuSubItem("Overall", tabName = "DE_overall"),
               menuSubItem("Immune Cells", tabName = "DE_immunecell"),
               menuSubItem("Pathways", tabName = "DE_pathway")
      ),
      menuItem("Correlation", tabName = "Correlation", icon = icon("chart-line"),
               menuSubItem("Co-expression", tabName = "Coexpression"),
               menuSubItem("Scatter plot", tabName = "genepathimmcor"),
               menuSubItem("Multigenes", tabName = "Multigenes"),
               menuSubItem("Meta analysis", tabName = "corMetaAnalysis")
      ),
      menuItem("Immune infiltration", tabName = "Immuneinfiltration", icon = icon("chart-pie"),
               menuSubItem("Immune Cells", tabName = "imm_infli_ImmuneCells"),
               menuSubItem("Meta analysis", tabName = "imm_infli_meta")),
      menuItem("Pathway Analysis", tabName = "PathwayEnrichment", icon = icon("bezier-curve"),
               menuSubItem("Pathway Enrichment", tabName = "ClusterProfiler"),
               menuSubItem("Pathview", tabName = "Pathview"),
               menuSubItem("ssGSEA", tabName = "ssGSEA")),
      menuItem("Data", tabName = "Data", icon = icon("cubes"),
               menuSubItem("Datasets", tabName = "data_for_datastes"),
               menuSubItem("Clinical Data", tabName = "data_for_clinical"),
               menuSubItem("Immune Cells", tabName = "data_for_immunecell_related")),
      menuItem("About", tabName = "about_panel", icon = icon("comments"),
               menuSubItem("FAQ", tabName = "question_panel"),
               menuSubItem("Contact", tabName = "contact_panel"))
    )
  ),
  
  ##============================================Body content
  dashboardBody(
    useWaiter(), # dependencies
    waiterPreloader(
      html = tagList(
        spin_circle(),
        "Please wait ..."
      ),
      color = "#6c6689"),
    
    useShinyFeedback(),
    useShinyjs(),
    includeCSS("www/custom.css"),
    
    tabItems(
      ##==============home
      tabItem(tabName = "Introduction",
              column(width=12,style = "padding-right:25px;padding-left:25px",
                     h1(class = "homeTitle","Welcome to HPVTIMER!"),
                     p(home_whole_intro_text)),
              
              column(width=12,style = "padding-right:25px;padding-left:25px",
                     slickROutput("home_slick_output",width='100%',height='200px')%>% withSpinner(color="#6c6689")),
              column(width=12,style = "padding-right:25px;padding-left:25px;padding-top: 25px",
                     bsCollapse(id = "document_for_changlog", open = "News and Updates",
                                bsCollapsePanel("News and Updates", 
                                                "Updates: 2023-06-12",br(),"HPVTIMER release 1.2", hr(),
                                                "Updates: 2023-04-22",br(),"HPVTIMER release 1.1", hr(),
                                                "Updates: 2023-02-12",br(),"HPVTIMER release 1.0", 
                                                style = "primary")
                     )
              ),
              column(width = 12,class = "footer-container",
                     HTML('<div style="text-align: center;"><p>Copyrig
ht © 2023. All rights reserved.</p></div> <div style="text-align: center;"> <div style
="display:inline-block;width:400px;"><script type="text/javascript" src="//rf.revolvermaps.com/0/0/7.js?i=5cj7bnyzooe&amp;m=0&amp;c=ff0000&amp;cr1=007eff&amp;sx=0" async="async"></script></div> </div>')
                     )
              
      ),
      
      ##=================================1.DE analysis_overall=============================
      
      tabItem(tabName = "DE_overall",
              fluidRow( 
                column(width=12,style = "padding-right:40px;padding-left:40px",
                       
                       column(width=12,align="center",style="padding-top:10px",
                              actionBttn(
                                inputId = "DE_overall_title",
                                label = h1(class = "pageTitle","Differential Expression Analysis for All Genes",
                                           icon(name = "search",lib="glyphicon")), 
                                style = "minimal",
                                color = "danger",
                                size = "lg")),
                       bsModal(id="modal_DE_overall_title", title="Introduction", trigger="DE_overall_title", 
                               size = "large",DEanalysis_overall_text),
                       
                       radioGroupButtons(
                         inputId = "DE_overall_plottype",#width="900px",
                         label = NULL,
                         choices = c("Volcano","Heatmap","Boxplot"),
                         selected = "Volcano",
                         justified = TRUE,
                         status = "warning"
                       ),
                       
                       conditionalPanel(
                         condition = "input.DE_overall_plottype=='Volcano'",
                         shinydashboard::box(width=NULL,
                                             column(width = 12,
                                                    column(width = 5,style = "padding-right:15px;padding-left:0px",
                                                           selectizeInput(
                                                             inputId = "DE_overall_vol_dataset",
                                                             label = p("Search, select a dataset:", style="margin: 0 0 5px;",
                                                                       icon(name = "question-sign",lib="glyphicon",id="DE_all_vol_dataset_q")),
                                                             choices = setNames(GSEdatalist$name[which(GSEdatalist$DEoverall==TRUE)],
                                                                                GSEdatalist$samplenum[which(GSEdatalist$DEoverall==TRUE)]),
                                                             selected="HNSCC_GSE72536"
                                                           ),
                                                           bsModal(id="modal_DE_all_vol_dataset_q", title="Dataset Selection", trigger="DE_all_vol_dataset_q", 
                                                                   size = "large",
                                                                   p(class = "submitboxContext","The dataset name is a combination of the corresponding tumor name/cell line and GSE ID."),
                                                                   p(class = "submitboxContext","For example: "),
                                                                   p(class = "submitboxContext","OPSCC_GSE55544 indicates that GSE55544 is a dataset of oropharyngeal squamous cell carcinoma."),
                                                                   DT::datatable(abbreviation,width = "500px",height = "300px",
                                                                                 caption="Table: Abbreviation for cancer/cell line.",
                                                                                 rownames = FALSE,
                                                                                 options = list(
                                                                                   dom = 'tpr',
                                                                                   pageLength = 5,
                                                                                   initComplete = JS(
                                                                                     "function(settings, json) {",
                                                                                     "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                                     "}")
                                                                                   
                                                                                 )
                                                                   ))
                                                    ),
                                                    column(width=2,style="padding-top:25px",
                                                           actionButton("DE_all_vol_update", 
                                                                        "Visualize",icon = icon('palette')))
                                             ),
                                             
                                             column(width = 12,
                                                    DE_ALL_UI("id_DE_All_Vol"))
                                             
                         ),
                         div(id="DE_overall_vol_result_sum",
                             tabBox(id="DE_overall_vol_result_panel", title="Result",width = NULL,
                                    tabPanel("Plot",
                                             div(
                                               fluidRow(
                                                 column(width = 8,
                                                        dropdownButton(
                                                          inputId = "DE_overall_volcano_introduction",
                                                          label = "Plot Introduction",
                                                          #icon = icon("question"),
                                                          status = "primary",
                                                          circle = FALSE,
                                                          width="500px",
                                                          DE_overall_volcano_text)),
                                                 column(width = 4,
                                                        dropdownButton(
                                                          inputId = "DE_overall_volcano_download",
                                                          label = "Download",
                                                          status = "primary",
                                                          icon=icon("download"),
                                                          circle = FALSE,
                                                          width="50px",
                                                          downloadButton(outputId = "DE_overall_volcano_download_svg",
                                                                         label = "SVG"),
                                                          downloadButton(outputId = "DE_overall_volcano_download_pdf",
                                                                         label = "PDF"),
                                                          downloadButton(outputId = "DE_overall_volcano_download_png",
                                                                         label = "PNG")))
                                                 
                                               ),
                                               plotOutput("DE_overall_volcano_result_plot_show")%>% withSpinner(color="#6c6689")
                                             )
                                    ),
                                    tabPanel("Data",
                                             fluidRow(
                                               column(width = 7,
                                                      dropdownButton(
                                                        inputId = "DE_overall_vol_fulldata_download",
                                                        label = "Download Full Data",
                                                        status = "primary",
                                                        icon=icon("download"),
                                                        circle = FALSE,
                                                        width="50px",
                                                        downloadButton(outputId = "DE_overall_vol_fulldata_download_csv",
                                                                       label = "CSV"),
                                                        downloadButton(outputId = "DE_overall_vol_fulldata_download_txt",
                                                                       label = "TXT"))),
                                               column(width = 5,
                                                      helpText("Please wait as the table take a few seconds to load."))
                                               
                                             ),
                                             uiOutput("DE_overall_vol_result_data_panel"))))
                         
                       ),
                       
                       conditionalPanel(
                         condition = "input.DE_overall_plottype=='Heatmap'",
                         shinydashboard::box(width=NULL,
                                             column(width = 12,
                                                    column(width = 5,style = "padding-right:15px;padding-left:0px",
                                                           selectizeInput(
                                                             inputId = "DE_overall_heat_dataset",
                                                             label = p("Search, select a dataset:", style="margin: 0 0 5px;",
                                                                       icon(name = "question-sign",lib="glyphicon",id="DE_all_heat_dataset_q")),
                                                             choices = setNames(GSEdatalist$name[which(GSEdatalist$DEoverall==TRUE)],
                                                                                GSEdatalist$samplenum[which(GSEdatalist$DEoverall==TRUE)]),
                                                             selected="HNSCC_GSE74927"
                                                           ),
                                                           bsModal(id="modal_DE_all_heat_dataset_q", title="Dataset Selection", trigger="DE_all_heat_dataset_q", 
                                                                   size = "large",
                                                                   p(class = "submitboxContext","The dataset name is a combination of the corresponding tumor name/cell line and GSE ID."),
                                                                   p(class = "submitboxContext","For example: "),
                                                                   p(class = "submitboxContext","OPSCC_GSE55544 indicates that GSE55544 is a dataset of oropharyngeal squamous cell carcinoma."),
                                                                   DT::datatable(abbreviation,width = "500px",height = "300px",
                                                                                 caption="Table: Abbreviation for cancer/cell line.",
                                                                                 rownames = FALSE,
                                                                                 options = list(
                                                                                   dom = 'tpr',
                                                                                   pageLength = 5,
                                                                                   initComplete = JS(
                                                                                     "function(settings, json) {",
                                                                                     "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                                     "}")
                                                                                   
                                                                                 )
                                                                   ))
                                                    ),
                                                    column(width=2,style="padding-top:25px",
                                                           actionButton("DE_all_heat_update", 
                                                                        "Visualize",icon = icon('palette')))
                                             ),
                                             
                                             column(width = 12,
                                                    DE_ALL_UI("id_DE_All_Heat"))
                                             
                         ),
                         div(id="DE_overall_heat_result_sum",
                             
                             tabBox(id="DE_overall_heat_result_panel", title="Result",width = NULL,
                                    tabPanel("Plot",
                                             div(
                                               fluidRow(
                                                 column(width = 8,
                                                        dropdownButton(
                                                          inputId = "DE_overall_heatmap_introduction",
                                                          label = "Plot Introduction",
                                                          #icon = icon("question"),
                                                          status = "primary",
                                                          circle = FALSE,
                                                          width="500px",
                                                          DE_overall_heatmap_text)),
                                                 column(width = 4,
                                                        dropdownButton(
                                                          inputId = "DE_overall_heatmap_download",
                                                          label = "Download",
                                                          status = "primary",
                                                          icon=icon("download"),
                                                          circle = FALSE,
                                                          width="50px",
                                                          downloadButton(outputId = "DE_overall_heatmap_download_pdf",
                                                                         label = "PDF"),
                                                          downloadButton(outputId = "DE_overall_heatmap_download_svg",
                                                                         label = "SVG"),
                                                          downloadButton(outputId = "DE_overall_heatmap_download_png",
                                                                         label = "PNG")))
                                                 
                                               ),
                                               plotOutput("DE_overall_heatmap_result_plot_show")%>% withSpinner(color="#6c6689")
                                             )
                                    ),
                                    tabPanel("Data",
                                             
                                             fluidRow(
                                               column(width = 7,
                                                      dropdownButton(
                                                        inputId = "DE_overall_heat_fulldata_download",
                                                        label = "Download Full Data",
                                                        status = "primary",
                                                        icon=icon("download"),
                                                        circle = FALSE,
                                                        width="50px",
                                                        downloadButton(outputId = "DE_overall_heat_fulldata_download_csv",
                                                                       label = "CSV"),
                                                        downloadButton(outputId = "DE_overall_heat_fulldata_download_txt",
                                                                       label = "TXT"))),
                                               column(width = 5,
                                                      helpText("Please wait as the table take a few seconds to load."))
                                               
                                             ),
                                             uiOutput("DE_overall_heat_result_data_panel"))))
                         
                       ),
                       
                       conditionalPanel(
                         condition = "input.DE_overall_plottype=='Boxplot'",
                         shinydashboard::box(width=NULL,
                                             column(width = 12,
                                                    column(width = 5,style = "padding-right:15px;padding-left:0px",
                                                           selectizeInput(
                                                             inputId = "DE_overall_box_dataset",
                                                             label = p("Search, select a dataset:", style="margin: 0 0 5px;",
                                                                       icon(name = "question-sign",lib="glyphicon",id="DE_all_box_dataset_q")),
                                                             choices = setNames(GSEdatalist$name[which(GSEdatalist$DEoverall==TRUE)],
                                                                                GSEdatalist$samplenum[which(GSEdatalist$DEoverall==TRUE)]),
                                                             selected="HNSCC_GSE72536"
                                                           ),
                                                           bsModal(id="modal_DE_all_box_dataset_q", title="Dataset Selection", trigger="DE_all_box_dataset_q", 
                                                                   size = "large",
                                                                   p(class = "submitboxContext","The dataset name is a combination of the corresponding tumor name/cell line and GSE ID."),
                                                                   p(class = "submitboxContext","For example: "),
                                                                   p(class = "submitboxContext","OPSCC_GSE55544 indicates that GSE55544 is a dataset of oropharyngeal squamous cell carcinoma."),
                                                                   DT::datatable(abbreviation,width = "500px",height = "300px",
                                                                                 caption="Table: Abbreviation for cancer/cell line.",
                                                                                 rownames = FALSE,
                                                                                 options = list(
                                                                                   dom = 'tpr',
                                                                                   pageLength = 5,
                                                                                   initComplete = JS(
                                                                                     "function(settings, json) {",
                                                                                     "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                                     "}")
                                                                                   
                                                                                 )
                                                                   ))
                                                    ),
                                                    column(width=2,style="padding-top:25px",
                                                           actionButton("DE_all_box_update", 
                                                                        "Visualize",icon = icon('palette')))
                                             ),
                                             
                                             column(width = 12,
                                                    DE_ALL_UI("id_DE_All_Box"))
                                             
                         ),
                         div(id="DE_overall_box_result_sum",
                             tabBox(id="DE_overall_box_result_panel", title="Result",width = NULL,
                                    tabPanel("Plot",
                                             div(
                                               fluidRow(
                                                 column(width = 8,
                                                        dropdownButton(
                                                          inputId = "DE_overall_boxplot_introduction",
                                                          label = "Plot Introduction",
                                                          #icon = icon("question"),
                                                          status = "primary",
                                                          circle = FALSE,
                                                          width="500px",
                                                          DE_overall_boxplot_text)),
                                                 column(width = 4,
                                                        dropdownButton(
                                                          inputId = "DE_overall_boxplot_download",
                                                          label = "Download",
                                                          status = "primary",
                                                          icon=icon("download"),
                                                          circle = FALSE,
                                                          width="50px",
                                                          downloadButton(outputId = "DE_overall_boxplot_download_pdf",
                                                                         label = "PDF"),
                                                          downloadButton(outputId = "DE_overall_boxplot_download_svg",
                                                                         label = "SVG"),
                                                          downloadButton(outputId = "DE_overall_boxplot_download_png",
                                                                         label = "PNG")))
                                                 
                                               ),
                                               plotOutput("DE_overall_boxplot_result_plot_show")%>% withSpinner(color="#6c6689")
                                             )
                                    ),
                                    tabPanel("Data",
                                             fluidRow(
                                               column(width = 7,
                                                      dropdownButton(
                                                        inputId = "DE_overall_box_fulldata_download",
                                                        label = "Download Full Data",
                                                        status = "primary",
                                                        icon=icon("download"),
                                                        circle = FALSE,
                                                        width="50px",
                                                        downloadButton(outputId = "DE_overall_box_fulldata_download_csv",
                                                                       label = "CSV"),
                                                        downloadButton(outputId = "DE_overall_box_fulldata_download_txt",
                                                                       label = "TXT"))),
                                               column(width = 5,
                                                      helpText("Please wait as the table take a few seconds to load."))
                                               
                                             ),
                                             uiOutput("DE_overall_box_result_data_panel"))))
                         
                       )
                       
                       
                       
                )
                
              ),
              column(12,class = "footer-container",
                     HTML('<div style="text-align: center;"><p>Copyright © 2023. All rights reserved.</p></div> <div style="text-align: center;">
                     <div style="display:inline-block;width:400px;"></div> </div>'))
              
      ),
      
      
      ##=================================1.DE analysis_immunecell=============================
      
      tabItem(tabName = "DE_immunecell",
              fluidRow( 
                column(width=12,style = "padding-right:40px;padding-left:40px",
                       
                       column(width=12,align="center",style="padding-top:10px",
                              actionBttn(
                                inputId = "DE_immunecell_title",
                                label = h1(class = "pageTitle","Differential Expression Analysis for Immune Cell",
                                           icon(name = "search",lib="glyphicon")), 
                                style = "minimal",
                                color = "danger",
                                size = "lg")),
                       bsModal(id="modal_DE_immunecell_title", title="Introduction", trigger="DE_immunecell_title", 
                               size = "large",DEanalysis_immune_text),
                       
                       radioGroupButtons(
                         inputId = "DE_immunecell_plottype",#width="900px",
                         label = NULL,
                         choices = c("Volcano","Heatmap","Boxplot"),
                         selected = "Heatmap",
                         justified = TRUE,
                         status = "warning"
                       ),
                       
                       conditionalPanel(
                         condition = "input.DE_immunecell_plottype=='Volcano'",
                         shinydashboard::box(width=NULL,
                                             column(width = 12,
                                                    column(width = 5,style = "padding-right:15px;padding-left:0px",
                                                           selectizeInput(
                                                             inputId = "DE_immunecell_vol_dataset",
                                                             label = p("Search, select a dataset:", style="margin: 0 0 5px;",
                                                                       icon(name = "question-sign",lib="glyphicon",id="DE_im_vol_dataset_q")),
                                                             choices = setNames(GSEdatalist$name[which(GSEdatalist$DEimmune==TRUE)],
                                                                                GSEdatalist$samplenum[which(GSEdatalist$DEimmune==TRUE)]),
                                                             selected="HNSCC_GSE72536"
                                                           ),
                                                           bsModal(id="modal_DE_im_vol_dataset_q", title="Dataset Selection", trigger="DE_im_vol_dataset_q", 
                                                                   size = "large",
                                                                   p(class = "submitboxContext","The dataset name is a combination of the corresponding tumor name/cell line and GSE ID."),
                                                                   p(class = "submitboxContext","For example: "),
                                                                   p(class = "submitboxContext","OPSCC_GSE55544 indicates that GSE55544 is a dataset of oropharyngeal squamous cell carcinoma."),
                                                                   DT::datatable(abbreviation,width = "500px",height = "300px",
                                                                                 caption="Table: Abbreviation for cancer/cell line.",
                                                                                 rownames = FALSE,
                                                                                 options = list(
                                                                                   dom = 'tpr',
                                                                                   pageLength = 5,
                                                                                   initComplete = JS(
                                                                                     "function(settings, json) {",
                                                                                     "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                                     "}")
                                                                                   
                                                                                 )
                                                                   ))
                                                    ),
                                                    column(width = 5,
                                                           selectizeInput(
                                                             inputId = "DE_immunecell_volcano_cell",
                                                             label =p("Select, search an immune cell: ",
                                                                      style="margin: 0 0 5px;",
                                                                      icon(name = "question-sign",lib="glyphicon",id="DE_im_vol_immcell_q"),
                                                                      bsTooltip("DE_im_vol_immcell_q", 
                                                                                paste0('<p>The list of immune cell-associated genes was obtained from Charoentong P et al., Rooney MS et al., and Vésteinn Thorsson et al. (available in the "Data-Immune Cells" module).</p>'),
                                                                                "right", trigger="hover",options = list(container = "body"))),
                                                             choices = unique(diff_Immunegenes$`Cell type`), 
                                                             selected ="Type 1 T helper cell",
                                                             multiple = FALSE
                                                           )),
                                                    column(width=2,style="padding-top:25px",
                                                           actionButton("DE_imm_vol_update", 
                                                                        "Visualize",icon = icon('palette')))
                                             ),
                                             column(width = 12,
                                                    DE_IMPA_UI("id_DE_Imm_Vol"))
                                             
                         ),
                         div(id="DE_immunecell_vol_result_sum",
                             tabBox(id="DE_immunecell_vol_result_panel", title="Result",width = NULL,
                                    tabPanel("Plot",
                                             div(
                                               fluidRow(
                                                 column(width = 8,
                                                        dropdownButton(
                                                          inputId = "DE_immunecell_volcano_introduction",
                                                          label = "Plot Introduction",
                                                          #icon = icon("question"),
                                                          status = "primary",
                                                          circle = FALSE,
                                                          width="500px",
                                                          DE_immune_volcano_text)),
                                                 column(width = 4,
                                                        dropdownButton(
                                                          inputId = "DE_immunecell_volcano_download",
                                                          label = "Download",
                                                          status = "primary",
                                                          icon=icon("download"),
                                                          circle = FALSE,
                                                          width="50px",
                                                          downloadButton(outputId = "DE_immunecell_volcano_download_svg",
                                                                         label = "SVG"),
                                                          downloadButton(outputId = "DE_immunecell_volcano_download_pdf",
                                                                         label = "PDF"),
                                                          downloadButton(outputId = "DE_immunecell_volcano_download_png",
                                                                         label = "PNG")))
                                                 
                                               ),
                                               plotOutput("DE_immunecell_volcano_result_plot_show")%>% withSpinner(color="#6c6689")
                                             )
                                    ),
                                    tabPanel("Data",
                                             fluidRow(
                                               column(width = 7,
                                                      dropdownButton(
                                                        inputId = "DE_immunecell_vol_fulldata_download",
                                                        label = "Download Full Data",
                                                        status = "primary",
                                                        icon=icon("download"),
                                                        circle = FALSE,
                                                        width="50px",
                                                        downloadButton(outputId = "DE_immunecell_vol_fulldata_download_csv",
                                                                       label = "CSV"),
                                                        downloadButton(outputId = "DE_immunecell_vol_fulldata_download_txt",
                                                                       label = "TXT"))),
                                               column(width = 5,
                                                      helpText("Please wait as the table take a few seconds to load."))
                                               
                                             ),
                                             uiOutput("DE_immunecell_vol_result_data_panel"))))
                         
                       ),
                       
                       conditionalPanel(
                         condition = "input.DE_immunecell_plottype=='Heatmap'",
                         shinydashboard::box(width=NULL,
                                             column(width = 12,
                                                    column(width = 5,style = "padding-right:15px;padding-left:0px",
                                                           selectizeInput(
                                                             inputId = "DE_immunecell_heat_dataset",
                                                             label = p("Search, select a dataset:", style="margin: 0 0 5px;",
                                                                       icon(name = "question-sign",lib="glyphicon",id="DE_im_heat_dataset_q")),
                                                             choices = setNames(GSEdatalist$name[which(GSEdatalist$DEimmune==TRUE)],
                                                                                GSEdatalist$samplenum[which(GSEdatalist$DEimmune==TRUE)]),
                                                             selected="HNSCC_GSE74927"
                                                           ),
                                                           bsModal(id="modal_DE_im_heat_dataset_q", title="Dataset Selection", trigger="DE_im_heat_dataset_q", 
                                                                   size = "large",
                                                                   p(class = "submitboxContext","The dataset name is a combination of the corresponding tumor name/cell line and GSE ID."),
                                                                   p(class = "submitboxContext","For example: "),
                                                                   p(class = "submitboxContext","OPSCC_GSE55544 indicates that GSE55544 is a dataset of oropharyngeal squamous cell carcinoma."),
                                                                   DT::datatable(abbreviation,width = "500px",height = "300px",
                                                                                 caption="Table: Abbreviation for cancer/cell line.",
                                                                                 rownames = FALSE,
                                                                                 options = list(
                                                                                   dom = 'tpr',
                                                                                   pageLength = 5,
                                                                                   initComplete = JS(
                                                                                     "function(settings, json) {",
                                                                                     "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                                     "}")
                                                                                   
                                                                                 )
                                                                   ))
                                                    ),
                                                    column(width = 5,
                                                           selectizeInput(
                                                             inputId = "DE_immunecell_heatmap_cell",
                                                             label =p("Select, search 1~5 immune cell(s):",
                                                                      style="margin: 0 0 5px;",
                                                                      icon(name = "question-sign",lib="glyphicon",id="DE_im_heat_immcell_q"),
                                                                      bsTooltip("DE_im_heat_immcell_q", 
                                                                                paste0('<p>The list of immune cell-associated genes was obtained from Charoentong P et al., Rooney MS et al., and Vésteinn Thorsson et al. (available in the "Data-Immune Cells" module).</p>'),
                                                                                "right", trigger="hover",options = list(container = "body"))),
                                                             choices = unique(diff_Immunegenes$`Cell type`), 
                                                             selected =c("Effector memeory CD8 T cell","Natural killer cell",
                                                                         "Gamma delta T cell","Activated dendritic cell"),
                                                             multiple = TRUE,
                                                             options = list(placeholder = "Select one immune cell at least!"  ,
                                                                            maxItems = 5)
                                                           )),
                                                    column(width=2,style="padding-top:25px",
                                                           actionButton("DE_imm_heat_update", 
                                                                        "Visualize",icon = icon('palette')))
                                             ),
                                             
                                             column(width = 12,
                                                    DE_IMPA_UI("id_DE_Imm_Heat"))
                                             
                         ),
                         div(id="DE_immunecell_heat_result_sum",
                             tabBox(id="DE_immunecell_heat_result_panel", title="Result",width = NULL,
                                    tabPanel("Plot",
                                             div(
                                               fluidRow(
                                                 column(width = 8,
                                                        dropdownButton(
                                                          inputId = "DE_immunecell_heatmap_introduction",
                                                          label = "Plot Introduction",
                                                          #icon = icon("question"),
                                                          status = "primary",
                                                          circle = FALSE,
                                                          width="500px",
                                                          DE_immune_heatmap_text)),
                                                 column(width = 4,
                                                        dropdownButton(
                                                          inputId = "DE_immunecell_heatmap_download",
                                                          label = "Download",
                                                          status = "primary",
                                                          icon=icon("download"),
                                                          circle = FALSE,
                                                          width="50px",
                                                          downloadButton(outputId = "DE_immunecell_heatmap_download_pdf",
                                                                         label = "PDF"),
                                                          downloadButton(outputId = "DE_immunecell_heatmap_download_svg",
                                                                         label = "SVG"),
                                                          downloadButton(outputId = "DE_immunecell_heatmap_download_png",
                                                                         label = "PNG")))
                                                 
                                               ),
                                               plotOutput("DE_immunecell_heatmap_result_plot_show")%>% withSpinner(color="#6c6689")
                                             )
                                    ),
                                    tabPanel("Data",
                                             fluidRow(
                                               column(width = 7,
                                                      dropdownButton(
                                                        inputId = "DE_immunecell_heat_fulldata_download",
                                                        label = "Download Full Data",
                                                        status = "primary",
                                                        icon=icon("download"),
                                                        circle = FALSE,
                                                        width="50px",
                                                        downloadButton(outputId = "DE_immunecell_heat_fulldata_download_csv",
                                                                       label = "CSV"),
                                                        downloadButton(outputId = "DE_immunecell_heat_fulldata_download_txt",
                                                                       label = "TXT"))),
                                               column(width = 5,
                                                      helpText("Please wait as the table take a few seconds to load."))
                                               
                                             ),
                                             uiOutput("DE_immunecell_heat_result_data_panel"))))
                         
                       ),
                       
                       conditionalPanel(
                         condition = "input.DE_immunecell_plottype=='Boxplot'",
                         shinydashboard::box(width=NULL,
                                             column(width = 12,
                                                    column(width = 5,style = "padding-right:15px;padding-left:0px",
                                                           selectizeInput(
                                                             inputId = "DE_immunecell_box_dataset",
                                                             label = p("Search, select a dataset:", style="margin: 0 0 5px;",
                                                                       icon(name = "question-sign",lib="glyphicon",id="DE_im_box_dataset_q")),
                                                             choices = setNames(GSEdatalist$name[which(GSEdatalist$DEimmune==TRUE)],
                                                                                GSEdatalist$samplenum[which(GSEdatalist$DEimmune==TRUE)]),
                                                             selected="HNSCC_GSE72536"
                                                           ),
                                                           bsModal(id="modal_DE_im_box_dataset_q", title="Dataset Selection", trigger="DE_im_box_dataset_q", 
                                                                   size = "large",
                                                                   p(class = "submitboxContext","The dataset name is a combination of the corresponding tumor name/cell line and GSE ID."),
                                                                   p(class = "submitboxContext","For example: "),
                                                                   p(class = "submitboxContext","OPSCC_GSE55544 indicates that GSE55544 is a dataset of oropharyngeal squamous cell carcinoma."),
                                                                   DT::datatable(abbreviation,width = "500px",height = "300px",
                                                                                 caption="Table: Abbreviation for cancer/cell line.",
                                                                                 rownames = FALSE,
                                                                                 options = list(
                                                                                   dom = 'tpr',
                                                                                   pageLength = 5,
                                                                                   initComplete = JS(
                                                                                     "function(settings, json) {",
                                                                                     "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                                     "}")
                                                                                   
                                                                                 )
                                                                   ))
                                                    ),
                                                    column(width = 5,
                                                           selectizeInput(
                                                             inputId = "DE_immunecell_boxplot_cell",
                                                             label =p("Select, search 1~5 immune cell(s):",
                                                                      style="margin: 0 0 5px;",
                                                                      icon(name = "question-sign",lib="glyphicon",id="DE_im_box_immcell_q"),
                                                                      bsTooltip("DE_im_box_immcell_q", 
                                                                                paste0('<p>The list of immune cell-associated genes was obtained from Charoentong P et al., Rooney MS et al., and Vésteinn Thorsson et al. (available in the "Data-Immune Cells" module).</p>'),
                                                                                "right", trigger="hover",options = list(container = "body"))),
                                                             choices = unique(diff_Immunegenes$`Cell type`), 
                                                             selected =c("Effector memeory CD8 T cell","Natural killer cell",
                                                                         "Gamma delta T cell","Activated dendritic cell"),
                                                             multiple = TRUE,
                                                             options = list(placeholder = "Select one immune cell at least!" ,
                                                                            maxItems = 5)
                                                           )),
                                                    column(width=2,style="padding-top:25px",
                                                           actionButton("DE_imm_box_update", 
                                                                        "Visualize",icon = icon('palette')))
                                             ),
                                             
                                             column(width = 12,
                                                    DE_IMPA_UI("id_DE_Imm_Box"))
                                             
                         ),
                         div(id="DE_immunecell_box_result_sum",
                             tabBox(id="DE_immunecell_box_result_panel", title="Result",width = NULL,
                                    tabPanel("Plot",
                                             div(
                                               fluidRow(
                                                 column(width = 8,
                                                        dropdownButton(
                                                          inputId = "DE_immunecell_boxplot_introduction",
                                                          label = "Plot Introduction",
                                                          #icon = icon("question"),
                                                          status = "primary",
                                                          circle = FALSE,
                                                          width="500px",
                                                          DE_immune_boxplot_text)),
                                                 column(width = 4,
                                                        dropdownButton(
                                                          inputId = "DE_immunecell_boxplot_download",
                                                          label = "Download",
                                                          status = "primary",
                                                          icon=icon("download"),
                                                          circle = FALSE,
                                                          width="50px",
                                                          downloadButton(outputId = "DE_immunecell_boxplot_download_pdf",
                                                                         label = "PDF"),
                                                          downloadButton(outputId = "DE_immunecell_boxplot_download_svg",
                                                                         label = "SVG"),
                                                          downloadButton(outputId = "DE_immunecell_boxplot_download_png",
                                                                         label = "PNG")))
                                                 
                                               ),
                                               plotOutput("DE_immunecell_boxplot_result_plot_show")%>% withSpinner(color="#6c6689")
                                             )
                                    ),
                                    tabPanel("Data",
                                             fluidRow(
                                               column(width = 7,
                                                      dropdownButton(
                                                        inputId = "DE_immunecell_box_fulldata_download",
                                                        label = "Download Full Data",
                                                        status = "primary",
                                                        icon=icon("download"),
                                                        circle = FALSE,
                                                        width="50px",
                                                        downloadButton(outputId = "DE_immunecell_box_fulldata_download_csv",
                                                                       label = "CSV"),
                                                        downloadButton(outputId = "DE_immunecell_box_fulldata_download_txt",
                                                                       label = "TXT"))),
                                               column(width = 5,
                                                      helpText("Please wait as the table take a few seconds to load."))
                                               
                                             ),
                                             uiOutput("DE_immunecell_box_result_data_panel"))))
                         
                       )
                       
                       
                       
                )
                
              ),
              column(12,class = "footer-container",
                     HTML('<div style="text-align: center;"><p>Copyright © 2023. All rights reserved.</p></div> <div style="text-align: center;">
                     <div style="display:inline-block;width:400px;"></div> </div>'))
              
      ),
      
      ##=================================3.DE analysis_pathway=============================
      
      tabItem(tabName = "DE_pathway",
              fluidRow( 
                column(width=12,style = "padding-right:40px;padding-left:40px",
                       
                       column(width=12,align="center",style="padding-top:10px",
                              actionBttn(
                                inputId = "DE_pathway_title",
                                label = h1(class = "pageTitle","Differential Expression Analysis for Pathway",
                                           icon(name = "search",lib="glyphicon")), 
                                style = "minimal",
                                color = "danger",
                                size = "lg")),
                       bsModal(id="modal_DE_pathway_title", title="Introduction", trigger="DE_pathway_title", 
                               size = "large",DEanalysis_pathway_text),
                       
                       radioGroupButtons(
                         inputId = "DE_pathway_plottype",#width="900px",
                         label = NULL,
                         choices = c("Volcano","Heatmap","Boxplot"),
                         selected = "Boxplot",
                         justified = TRUE,
                         status = "warning"
                       ),
                       
                       conditionalPanel(
                         condition = "input.DE_pathway_plottype=='Volcano'",
                         
                         shinydashboard::box(width=NULL,
                                             column(width = 12,
                                                    column(width = 5,style = "padding-right:15px;padding-left:0px",
                                                           selectizeInput(
                                                             inputId = "DE_pathway_vol_dataset",
                                                             label = p("Search, select a dataset:", style="margin: 0 0 5px;",
                                                                       icon(name = "question-sign",lib="glyphicon",id="DE_pa_vol_dataset_q")),
                                                             choices = setNames(GSEdatalist$name[which(GSEdatalist$DEpathway==TRUE)],
                                                                                GSEdatalist$samplenum[which(GSEdatalist$DEpathway==TRUE)]),
                                                             selected="HNSCC_GSE72536"
                                                           ),
                                                           bsModal(id="modal_DE_pa_vol_dataset_q", title="Dataset Selection", trigger="DE_pa_vol_dataset_q", 
                                                                   size = "large",
                                                                   p(class = "submitboxContext","The dataset name is a combination of the corresponding tumor name/cell line and GSE ID."),
                                                                   p(class = "submitboxContext","For example: "),
                                                                   p(class = "submitboxContext","OPSCC_GSE55544 indicates that GSE55544 is a dataset of oropharyngeal squamous cell carcinoma."),
                                                                   DT::datatable(abbreviation,width = "500px",height = "300px",
                                                                                 caption="Table: Abbreviation for cancer/cell line.",
                                                                                 rownames = FALSE,
                                                                                 options = list(
                                                                                   dom = 'tpr',
                                                                                   pageLength = 5,
                                                                                   initComplete = JS(
                                                                                     "function(settings, json) {",
                                                                                     "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                                     "}")
                                                                                   
                                                                                 )
                                                                   ))
                                                    ),
                                                    column(width = 5,
                                                           selectizeInput(
                                                             inputId = "DE_pathway_volcano_pathway",
                                                             label =p("Select, search a pathway:",
                                                                      style="margin: 0 0 5px;",
                                                                      icon(name = "question-sign",lib="glyphicon",id="DE_pa_vol_path_q"),
                                                                      bsTooltip("DE_pa_vol_path_q", 
                                                                                paste0("<p>The pathway-associated gene lists are from the hallmark gene sets, canonical pathways, and ontology gene sets in the MSigDB database.</p>"),
                                                                                "right", trigger="hover",options = list(container = "body"))),
                                                             choices = NULL, 
                                                             multiple = FALSE 
                                                           )),
                                                    column(width=2,style="padding-top:25px",
                                                           actionButton("DE_path_vol_update", 
                                                                        "Visualize",icon = icon('palette')))
                                             ),
                                             
                                             column(width = 12,
                                                    DE_IMPA_UI("id_DE_Path_Vol"))
                                             
                         ),
                         div(id="DE_pathway_vol_result_sum",
                             tabBox(id="DE_pathway_vol_result_panel", title="Result",width = NULL,
                                    tabPanel("Plot",
                                             div(
                                               fluidRow(
                                                 column(width = 8,
                                                        dropdownButton(
                                                          inputId = "DE_pathway_volcano_introduction",
                                                          label = "Plot Introduction",
                                                          #icon = icon("question"),
                                                          status = "primary",
                                                          circle = FALSE,
                                                          width="500px",
                                                          DE_pathway_volcano_text)),
                                                 column(width = 4,
                                                        dropdownButton(
                                                          inputId = "DE_pathway_volcano_download",
                                                          label = "Download",
                                                          status = "primary",
                                                          icon=icon("download"),
                                                          circle = FALSE,
                                                          width="50px",
                                                          downloadButton(outputId = "DE_pathway_volcano_download_pdf",
                                                                         label = "PDF"),
                                                          downloadButton(outputId = "DE_pathway_volcano_download_svg",
                                                                         label = "SVG"),
                                                          downloadButton(outputId = "DE_pathway_volcano_download_png",
                                                                         label = "PNG")))
                                                 
                                               ),
                                               plotOutput("DE_pathway_volcano_result_plot_show")%>% withSpinner(color="#6c6689")
                                             )
                                    ),
                                    tabPanel("Data",
                                             dropdownButton(
                                               inputId = "DE_pathway_vol_fulldata_download",
                                               label = "Download Full Data",
                                               status = "primary",
                                               icon=icon("download"),
                                               circle = FALSE,
                                               width="50px",
                                               downloadButton(outputId = "DE_pathway_vol_fulldata_download_csv",
                                                              label = "CSV"),
                                               downloadButton(outputId = "DE_pathway_vol_fulldata_download_txt",
                                                              label = "TXT")),
                                             helpText("Tip: The data is too large. Please download and check."))))
                         
                       ),
                       
                       conditionalPanel(
                         condition = "input.DE_pathway_plottype=='Heatmap'",
                         shinydashboard::box(width=NULL,
                                             column(width = 12,
                                                    column(width = 5,style = "padding-right:15px;padding-left:0px",
                                                           selectizeInput(
                                                             inputId = "DE_pathway_heat_dataset",
                                                             label = p("Search, select a dataset:", style="margin: 0 0 5px;",
                                                                       icon(name = "question-sign",lib="glyphicon",id="DE_pa_heat_dataset_q")),
                                                             choices = setNames(GSEdatalist$name[which(GSEdatalist$DEpathway==TRUE)],
                                                                                GSEdatalist$samplenum[which(GSEdatalist$DEpathway==TRUE)]),
                                                             selected="HNSCC_GSE74927"
                                                           ),
                                                           bsModal(id="modal_DE_pa_heat_dataset_q", title="Dataset Selection", trigger="DE_pa_heat_dataset_q", 
                                                                   size = "large",
                                                                   p(class = "submitboxContext","The dataset name is a combination of the corresponding tumor name/cell line and GSE ID."),
                                                                   p(class = "submitboxContext","For example: "),
                                                                   p(class = "submitboxContext","OPSCC_GSE55544 indicates that GSE55544 is a dataset of oropharyngeal squamous cell carcinoma."),
                                                                   DT::datatable(abbreviation,width = "500px",height = "300px",
                                                                                 caption="Table: Abbreviation for cancer/cell line.",
                                                                                 rownames = FALSE,
                                                                                 options = list(
                                                                                   dom = 'tpr',
                                                                                   pageLength = 5,
                                                                                   initComplete = JS(
                                                                                     "function(settings, json) {",
                                                                                     "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                                     "}")
                                                                                   
                                                                                 )
                                                                   ))
                                                    ),
                                                    column(width = 5,
                                                           selectizeInput(
                                                             inputId = "DE_pathway_heatmap_pathway",
                                                             label =p("Select, search 1~3 pathway(s):",
                                                                      style="margin: 0 0 5px;",
                                                                      icon(name = "question-sign",lib="glyphicon",id="DE_pa_heat_path_q"),
                                                                      bsTooltip("DE_pa_heat_path_q", 
                                                                                paste0("<p>The pathway-associated gene lists are from the hallmark gene sets, canonical pathways, and ontology gene sets in the MSigDB database.</p>"),
                                                                                "right", trigger="hover",options = list(container = "body"))), 
                                                             choices = NULL, 
                                                             multiple = TRUE,
                                                             options = list(placeholder = "Select one pathway at least!"  ,
                                                                            maxItems = 3)
                                                           )),
                                                    column(width=2,style="padding-top:25px",
                                                           actionButton("DE_path_heat_update", 
                                                                        "Visualize",icon = icon('palette')))
                                             ),
                                             
                                             column(width = 12,
                                                    DE_IMPA_UI("id_DE_Path_Heat"))
                                             
                         ),
                         div(id="DE_pathway_heat_result_sum",
                             tabBox(id="DE_pathway_heat_result_panel", title="Result",width = NULL,
                                    tabPanel("Plot",
                                             div(
                                               fluidRow(
                                                 column(width = 8,
                                                        dropdownButton(
                                                          inputId = "DE_pathway_heatmap_introduction",
                                                          label = "Plot Introduction",
                                                          #icon = icon("question"),
                                                          status = "primary",
                                                          circle = FALSE,
                                                          width="500px",
                                                          DE_pathway_heatmap_text)),
                                                 column(width = 4,
                                                        dropdownButton(
                                                          inputId = "DE_pathway_heatmap_download",
                                                          label = "Download",
                                                          status = "primary",
                                                          icon=icon("download"),
                                                          circle = FALSE,
                                                          width="50px",
                                                          downloadButton(outputId = "DE_pathway_heatmap_download_pdf",
                                                                         label = "PDF"),
                                                          downloadButton(outputId = "DE_pathway_heatmap_download_svg",
                                                                         label = "SVG"),
                                                          downloadButton(outputId = "DE_pathway_heatmap_download_png",
                                                                         label = "PNG")))
                                                 
                                               ),
                                               plotOutput("DE_pathway_heatmap_result_plot_show")%>% withSpinner(color="#6c6689")
                                             )
                                    ),
                                    tabPanel("Data",
                                             dropdownButton(
                                               inputId = "DE_pathway_heat_fulldata_download",
                                               label = "Download Full Data",
                                               status = "primary",
                                               icon=icon("download"),
                                               circle = FALSE,
                                               width="50px",
                                               downloadButton(outputId = "DE_pathway_heat_fulldata_download_csv",
                                                              label = "CSV"),
                                               downloadButton(outputId = "DE_pathway_heat_fulldata_download_txt",
                                                              label = "TXT")),
                                             helpText("Tip: The data is too large. Please download and check."))))
                         
                       ),
                       
                       conditionalPanel(
                         condition = "input.DE_pathway_plottype=='Boxplot'",
                         shinydashboard::box(width=NULL,
                                             column(width = 12,
                                                    column(width = 5,style = "padding-right:15px;padding-left:0px",
                                                           selectizeInput(
                                                             inputId = "DE_pathway_box_dataset",
                                                             label = p("Search, select a dataset:", style="margin: 0 0 5px;",
                                                                       icon(name = "question-sign",lib="glyphicon",id="DE_pa_box_dataset_q")),
                                                             choices = setNames(GSEdatalist$name[which(GSEdatalist$DEpathway==TRUE)],
                                                                                GSEdatalist$samplenum[which(GSEdatalist$DEpathway==TRUE)]),
                                                             selected="HNSCC_GSE72536"
                                                           ),
                                                           bsModal(id="modal_DE_pa_box_dataset_q", title="Dataset Selection", trigger="DE_pa_box_dataset_q", 
                                                                   size = "large",
                                                                   p(class = "submitboxContext","The dataset name is a combination of the corresponding tumor name/cell line and GSE ID."),
                                                                   p(class = "submitboxContext","For example: "),
                                                                   p(class = "submitboxContext","OPSCC_GSE55544 indicates that GSE55544 is a dataset of oropharyngeal squamous cell carcinoma."),
                                                                   DT::datatable(abbreviation,width = "500px",height = "300px",
                                                                                 caption="Table: Abbreviation for cancer/cell line.",
                                                                                 rownames = FALSE,
                                                                                 options = list(
                                                                                   dom = 'tpr',
                                                                                   pageLength = 5,
                                                                                   initComplete = JS(
                                                                                     "function(settings, json) {",
                                                                                     "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                                     "}")
                                                                                   
                                                                                 )
                                                                   ))
                                                    ),
                                                    column(width = 5,
                                                           selectizeInput(
                                                             inputId = "DE_pathway_boxplot_pathway",
                                                             label =p("Select, search a pathway:",
                                                                      style="margin: 0 0 5px;",
                                                                      icon(name = "question-sign",lib="glyphicon",id="DE_pa_box_path_q"),
                                                                      bsTooltip("DE_pa_box_path_q", 
                                                                                paste0("<p>The pathway-associated gene lists are from the hallmark gene sets, canonical pathways, and ontology gene sets in the MSigDB database.</p>"),
                                                                                "right", trigger="hover",options = list(container = "body"))), 
                                                             choices = NULL, 
                                                             multiple = FALSE 
                                                           )),
                                                    column(width=2,style="padding-top:25px",
                                                           actionButton("DE_path_box_update", 
                                                                        "Visualize",icon = icon('palette')))
                                             ),
                                             
                                             column(width = 12,
                                                    DE_IMPA_UI("id_DE_Path_Box"))
                                             
                         ),
                         div(id="DE_pathway_box_result_sum",
                             tabBox(id="DE_pathway_box_result_panel", title="Result",width = NULL,
                                    tabPanel("Plot",
                                             div(
                                               fluidRow(
                                                 column(width = 8,
                                                        dropdownButton(
                                                          inputId = "DE_pathway_boxplot_introduction",
                                                          label = "Plot Introduction",
                                                          #icon = icon("question"),
                                                          status = "primary",
                                                          circle = FALSE,
                                                          width="500px",
                                                          DE_pathway_boxplot_text)),
                                                 column(width = 4,
                                                        dropdownButton(
                                                          inputId = "DE_pathway_boxplot_download",
                                                          label = "Download",
                                                          status = "primary",
                                                          icon=icon("download"),
                                                          circle = FALSE,
                                                          width="50px",
                                                          downloadButton(outputId = "DE_pathway_boxplot_download_pdf",
                                                                         label = "PDF"),
                                                          downloadButton(outputId = "DE_pathway_boxplot_download_svg",
                                                                         label = "SVG"),
                                                          downloadButton(outputId = "DE_pathway_boxplot_download_png",
                                                                         label = "PNG")))
                                                 
                                               ),
                                               plotOutput("DE_pathway_boxplot_result_plot_show")%>% withSpinner(color="#6c6689")
                                             )
                                    ),
                                    tabPanel("Data",
                                             dropdownButton(
                                               inputId = "DE_pathway_box_fulldata_download",
                                               label = "Download Full Data",
                                               status = "primary",
                                               icon=icon("download"),
                                               circle = FALSE,
                                               width="50px",
                                               downloadButton(outputId = "DE_pathway_box_fulldata_download_csv",
                                                              label = "CSV"),
                                               downloadButton(outputId = "DE_pathway_box_fulldata_download_txt",
                                                              label = "TXT")),
                                             helpText("Tip: The data is too large. Please download and check.")
                                    )))
                         
                       )
                       
                       
                       
                )
                
              ),
              column(12,class = "footer-container",
                     HTML('<div style="text-align: center;"><p>Copyright © 2023. All rights reserved.</p></div> <div style="text-align: center;">
                     <div style="display:inline-block;width:400px;"></div> </div>'))
              
      ),
      
      ##=================================7.imm_infli_GSE=============================
      tabItem(tabName = "imm_infli_ImmuneCells",
              
              
              fluidRow(
                
                column(width=12,style = "padding-right:40px;padding-left:40px",
                       
                       column(width=12,align="center",style="padding-top:10px",
                              actionBttn(
                                inputId = "imm_infli_GSE_title",
                                label = h1(class = "pageTitle","Immune Infiltration Analysis",
                                           icon(name = "search",lib="glyphicon")), 
                                style = "minimal",
                                color = "danger",
                                size = "lg")),
                       bsModal(id="modal_imm_infli_GSE_title", title="Introduction", trigger="imm_infli_GSE_title", 
                               size = "large",imm_infli_GSE_text),
                       
                       radioGroupButtons(
                         inputId = "imm_infli_GSE_plottype",
                         label = NULL,
                         choices = c("Stacked plot","Heatmap","Boxplot","Multi-datasets"),
                         justified = TRUE,
                         status = "warning",
                         selected = "Multi-datasets"
                       ),
                       
                       conditionalPanel(
                         condition = "input.imm_infli_GSE_plottype=='Stacked plot'",
                         
                         shinydashboard::box(width=NULL,
                                             column(width = 5,offset = 1,
                                                    #h4(class = "boxTitle","Parameters of Stacked plot"),
                                                    selectizeInput(
                                                      inputId = "imm_infli_GSE_sta_dataset",
                                                      label = p("Search, select a dataset:", 
                                                                style="margin: 0 0 5px;",
                                                                icon(name = "question-sign",lib="glyphicon",id="imm_infli_GSE_sta_dataset_q")),
                                                      #choices = GSEdatalist$name[which(GSEdatalist$imm_infli==TRUE)],
                                                      choices= setNames(GSEdatalist$name[which(GSEdatalist$imm_infli==TRUE)],
                                                                        GSEdatalist$samplenum[which(GSEdatalist$imm_infli==TRUE)]),
                                                      selected="HNSCC_GSE76532"
                                                    ),
                                                    
                                                    bsModal(id="modal_imm_infli_GSE_sta_dataset_q", title="Dataset Selection", trigger="imm_infli_GSE_sta_dataset_q", 
                                                            size = "large",
                                                            p(class = "submitboxContext","The dataset name is a combination of the corresponding tumor name/cell line and GSE ID."),
                                                            p(class = "submitboxContext","For example: "),
                                                            p(class = "submitboxContext","OPSCC_GSE55544 indicates that GSE55544 is a dataset of oropharyngeal squamous cell carcinoma."),
                                                            DT::datatable(abbreviation,width = "500px",height = "300px",
                                                                          caption="Table: Abbreviation for cancer/cell line.",
                                                                          rownames = FALSE,
                                                                          options = list(
                                                                            dom = 'tpr',
                                                                            pageLength = 5,
                                                                            initComplete = JS(
                                                                              "function(settings, json) {",
                                                                              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                              "}")
                                                                            
                                                                          )
                                                            )),
                                                    
                                                    selectInput(
                                                      inputId = "imm_infli_GSE_sta_method",
                                                      label = p("Which algorithm are you interested:", 
                                                                style="margin: 0 0 5px;",
                                                                icon(name = "question-sign",lib="glyphicon",id="imm_infli_GSE_sta_method_q")),
                                                      choices =c("CIBERSORT","CIBERSORT_abs","EPIC","quanTIseq"),
                                                      selected = "CIBERSORT_abs"
                                                    ),
                                                    
                                                    bsModal(id="modal_imm_infli_GSE_sta_method_q", title="Algorithm Selection", trigger="imm_infli_GSE_sta_method_q", 
                                                            size = "small",
                                                            p(class = "submitboxContext",
                                                              "1. The radio buttons let users select the method used to estimate the immune cells' infiltration levels.",br(),
                                                              "2. Due to the distinct principles, the methods that can be used for intrasample comparisons between cell types are CIBERSORT, CIBERSORT absolute mode, EPIC, and quanTIseq."),
                                                            DT::datatable(imm_imfli_algorithm,width = "200px",height = "300px",
                                                                          caption="Table: More about algorithms.",
                                                                          rownames = FALSE,
                                                                          escape=FALSE,
                                                                          options = list(
                                                                            dom = 't',
                                                                            initComplete = JS(
                                                                              "function(settings, json) {",
                                                                              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                              "}"))))
                                                    
                                             ),
                                             
                                             column(width = 6,
                                                    column(width = 10,
                                                           br(),br(),br(),br(),br()),
                                                    column(width = 3,
                                                           actionButton('imm_infli_GSE_sta_update', 
                                                                        "Visualize",icon = icon('palette'))
                                                    ),
                                                    
                                             ),
                                             
                         ),
                         
                         div(id="imm_infli_GSE_sta_result_sum",
                             shinydashboard::tabBox(id="imm_infli_GSE_sta_result_panel", title="Result",width = NULL,
                                                    tabPanel("Plot",
                                                             div(
                                                               fluidRow(
                                                                 column(width = 8,
                                                                        dropdownButton(
                                                                          inputId = "imm_infli_GSE_stacking_introduction",
                                                                          label = "Plot Introduction",
                                                                          #icon = icon("question"),
                                                                          status = "primary",
                                                                          circle = FALSE,
                                                                          width="500px",
                                                                          ImmInfli_GSE_stacking_text)),
                                                                 column(width = 4,
                                                                        dropdownButton(
                                                                          inputId = "imm_infli_GSE_stacking_download",
                                                                          label = "Download",
                                                                          status = "primary",
                                                                          icon=icon("download"),
                                                                          circle = FALSE,
                                                                          width="50px",
                                                                          downloadButton(outputId = "imm_infli_GSE_stacking_download_pdf",
                                                                                         label = "PDF"),
                                                                          downloadButton(outputId = "imm_infli_GSE_stacking_download_svg",
                                                                                         label = "SVG"),
                                                                          downloadButton(outputId = "imm_infli_GSE_stacking_download_png",
                                                                                         label = "PNG")))
                                                                 
                                                               ),
                                                               plotOutput("imm_infli_GSE_stacking_result_plot_show")%>% withSpinner(color="#6c6689")
                                                             )),
                                                    tabPanel("Data",
                                                             fluidRow(
                                                               column(width = 7,
                                                                      dropdownButton(
                                                                        inputId = "imm_infli_GSE_sta_fulldata_download",
                                                                        label = "Download Data",
                                                                        status = "primary",
                                                                        icon=icon("download"),
                                                                        circle = FALSE,
                                                                        width="50px",
                                                                        downloadButton(outputId = "imm_infli_GSE_sta_fulldata_download_csv",
                                                                                       label = "CSV"),
                                                                        downloadButton(outputId = "imm_infli_GSE_sta_fulldata_download_txt",
                                                                                       label = "TXT"))),
                                                               column(width = 5,
                                                                      helpText("Please wait as the table take a few seconds to load."))
                                                               
                                                             ),
                                                             uiOutput("imm_infli_GSE_sta_result_data_panel"))))
                         
                         
                         
                       ),
                       
                       conditionalPanel(
                         condition = "input.imm_infli_GSE_plottype=='Heatmap'",
                         
                         shinydashboard::box(width=NULL,
                                             column(width = 5,offset = 1,
                                                    #h4(class = "boxTitle","Parameters of Heatmap"),
                                                    selectizeInput(
                                                      inputId = "imm_infli_GSE_heat_dataset",
                                                      label = p("Search, select a dataset:", 
                                                                style="margin: 0 0 5px;",
                                                                icon(name = "question-sign",lib="glyphicon",id="imm_infli_GSE_heat_dataset_q")),
                                                      #choices = GSEdatalist$name[which(GSEdatalist$imm_infli==TRUE)],
                                                      choices= setNames(GSEdatalist$name[which(GSEdatalist$imm_infli==TRUE)],
                                                                        GSEdatalist$samplenum[which(GSEdatalist$imm_infli==TRUE)]),
                                                      selected="HNSCC_GSE6791"
                                                    ),
                                                    
                                                    bsModal(id="modal_imm_infli_GSE_heat_dataset_q", title="Dataset Selection", trigger="imm_infli_GSE_heat_dataset_q", 
                                                            size = "large",
                                                            p(class = "submitboxContext","The dataset name is a combination of the corresponding tumor name/cell line and GSE ID."),
                                                            p(class = "submitboxContext","For example: "),
                                                            p(class = "submitboxContext","OPSCC_GSE55544 indicates that GSE55544 is a dataset of oropharyngeal squamous cell carcinoma."),
                                                            DT::datatable(abbreviation,width = "500px",height = "300px",
                                                                          caption="Table: Abbreviation for cancer/cell line.",
                                                                          rownames = FALSE,
                                                                          options = list(
                                                                            dom = 'tpr',
                                                                            pageLength = 5,
                                                                            initComplete = JS(
                                                                              "function(settings, json) {",
                                                                              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                              "}")
                                                                            
                                                                          )
                                                            )),
                                                    
                                                    selectInput(
                                                      inputId = "imm_infli_GSE_heat_method",
                                                      label = p("Which algorithm are you interested:", 
                                                                style="margin: 0 0 5px;",
                                                                icon(name = "question-sign",lib="glyphicon",id="imm_infli_GSE_heat_method_q")),
                                                      choices =NULL
                                                    ),
                                                    
                                                    bsModal(id="modal_imm_infli_GSE_heat_method_q", title="Algorithm Selection", trigger="imm_infli_GSE_heat_method_q", 
                                                            size = "small",
                                                            p(class = "submitboxContext",
                                                              "1. The radio buttons let users select the method used to estimate the immune cells' infiltration levels.",br(),
                                                              "2. Due to the distinct principles, the methods that can be used for intersample comparisons between the same cell types are CIBERSORT absolute mode, MCP-counter (or mMCP-counter for mice), EPIC, quanTIseq, TIMER, and xCell."),
                                                            DT::datatable(imm_imfli_algorithm,width = "200px",height = "300px",
                                                                          caption="Table: More about algorithms.",
                                                                          rownames = FALSE,
                                                                          escape=FALSE,
                                                                          options = list(
                                                                            dom = 't',
                                                                            initComplete = JS(
                                                                              "function(settings, json) {",
                                                                              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                              "}"))))
                                                    
                                             ),
                                             
                                             column(width = 6,
                                                    column(width = 10,
                                                           #br(),
                                                           shinydashboard::box(title =h4(class = "boxTitle","Color Option"),
                                                                               width=NULL,status="info",collapsible = T,collapsed=F, 
                                                                               pickerInput(
                                                                                 inputId = "imm_infli_GSE_heatmap_color",
                                                                                 label = "Select colorscale:", 
                                                                                 choices=c("light Red-Blue","Red-Blue","Purple-Green",
                                                                                           "Pink-Green","Brown-BlueyGreen","Red-Yellow-Blue","Green-Yellow")
                                                                               )
                                                           )),
                                                    column(width = 3,
                                                           actionButton('imm_infli_GSE_heat_update', 
                                                                        "Visualize",icon = icon('palette'))
                                                    )
                                                    
                                             ),
                                             
                                             
                         ),
                         
                         div(id="imm_infli_GSE_heat_result_sum",
                             shinydashboard::tabBox(id="imm_infli_GSE_heat_result_panel", title="Result",width = NULL,
                                                    tabPanel("Plot",
                                                             div(
                                                               fluidRow(
                                                                 column(width = 8,
                                                                        dropdownButton(
                                                                          inputId = "imm_infli_GSE_heatmap_introduction",
                                                                          label = "Plot Introduction",
                                                                          #icon = icon("question"),
                                                                          status = "primary",
                                                                          circle = FALSE,
                                                                          width="500px",
                                                                          ImmInfli_GSE_heatmap_text)),
                                                                 column(width = 4,
                                                                        dropdownButton(
                                                                          inputId = "imm_infli_GSE_heatmap_download",
                                                                          label = "Download",
                                                                          status = "primary",
                                                                          icon=icon("download"),
                                                                          circle = FALSE,
                                                                          width="50px",
                                                                          downloadButton(outputId = "imm_infli_GSE_heatmap_download_pdf",
                                                                                         label = "PDF"),
                                                                          downloadButton(outputId = "imm_infli_GSE_heatmap_download_svg",
                                                                                         label = "SVG"),
                                                                          downloadButton(outputId = "imm_infli_GSE_heatmap_download_png",
                                                                                         label = "PNG")))
                                                                 
                                                               ),
                                                               plotOutput("imm_infli_GSE_heatmap_result_plot_show")%>% withSpinner(color="#6c6689")
                                                             )),
                                                    tabPanel("Data",
                                                             fluidRow(
                                                               column(width = 7,
                                                                      dropdownButton(
                                                                        inputId = "imm_infli_GSE_heat_fulldata_download",
                                                                        label = "Download Data",
                                                                        status = "primary",
                                                                        icon=icon("download"),
                                                                        circle = FALSE,
                                                                        width="50px",
                                                                        downloadButton(outputId = "imm_infli_GSE_heat_fulldata_download_csv",
                                                                                       label = "CSV"),
                                                                        downloadButton(outputId = "imm_infli_GSE_heat_fulldata_download_txt",
                                                                                       label = "TXT"))),
                                                               column(width = 5,
                                                                      helpText("Please wait as the table take a few seconds to load."))
                                                               
                                                             ),
                                                             uiOutput("imm_infli_GSE_heat_result_data_panel"))))
                         
                         
                         
                       ),
                       
                       conditionalPanel(
                         condition = "input.imm_infli_GSE_plottype=='Boxplot'",
                         
                         shinydashboard::box(width=NULL,
                                             column(width = 5,offset = 1,
                                                    #h4(class = "boxTitle","Parameters of Boxplot"),
                                                    selectizeInput(
                                                      inputId = "imm_infli_GSE_box_dataset",
                                                      label = p("Search, select a dataset:", 
                                                                style="margin: 0 0 5px;",
                                                                icon(name = "question-sign",lib="glyphicon",id="imm_infli_GSE_box_dataset_q")),
                                                      #choices = GSEdatalist$name[which(GSEdatalist$imm_infli==TRUE)],
                                                      choices= setNames(GSEdatalist$name[which(GSEdatalist$imm_infli==TRUE)],
                                                                        GSEdatalist$samplenum[which(GSEdatalist$imm_infli==TRUE)]),
                                                      selected="HNSCC_GSE6791"
                                                    ),
                                                    
                                                    bsModal(id="modal_imm_infli_GSE_box_dataset_q", title="Dataset Selection", trigger="imm_infli_GSE_box_dataset_q", 
                                                            size = "large",
                                                            p(class = "submitboxContext","The dataset name is a combination of the corresponding tumor name/cell line and GSE ID."),
                                                            p(class = "submitboxContext","For example: "),
                                                            p(class = "submitboxContext","OPSCC_GSE55544 indicates that GSE55544 is a dataset of oropharyngeal squamous cell carcinoma."),
                                                            DT::datatable(abbreviation,width = "500px",height = "300px",
                                                                          caption="Table: Abbreviation for cancer/cell line.",
                                                                          rownames = FALSE,
                                                                          options = list(
                                                                            dom = 'tpr',
                                                                            pageLength = 5,
                                                                            initComplete = JS(
                                                                              "function(settings, json) {",
                                                                              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                              "}")
                                                                            
                                                                          )
                                                            )),
                                                    
                                                    selectInput(
                                                      inputId = "imm_infli_GSE_box_method",
                                                      label = p("Which algorithm are you interested:", 
                                                                style="margin: 0 0 5px;",
                                                                icon(name = "question-sign",lib="glyphicon",id="imm_infli_GSE_box_method_q")),
                                                      choices =NULL
                                                    ),
                                                    
                                                    bsModal(id="modal_imm_infli_GSE_box_method_q", title="Algorithm Selection", trigger="imm_infli_GSE_box_method_q", 
                                                            size = "small",
                                                            p(class = "submitboxContext",
                                                              "1. The radio buttons let users select the method used to estimate the immune cells' infiltration levels.",br(),
                                                              "2. Due to the distinct principles, the methods that can be used for intersample comparisons between the same cell types are CIBERSORT absolute mode, MCP-counter (or mMCP-counter for mice), EPIC, quanTIseq, TIMER, and xCell."),
                                                            DT::datatable(imm_imfli_algorithm,width = "200px",height = "300px",
                                                                          caption="Table: More about algorithms.",
                                                                          rownames = FALSE,
                                                                          escape=FALSE,
                                                                          options = list(
                                                                            dom = 't',
                                                                            initComplete = JS(
                                                                              "function(settings, json) {",
                                                                              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                              "}"))))
                                                    
                                             ),
                                             
                                             column(width = 6,
                                                    column(width = 10,
                                                           #br(),
                                                           radioButtons(
                                                             inputId = "imm_infli_GSE_boxplot_type",
                                                             label = p("What to display:",style="margin: 0 0 5px;"), 
                                                             choices = c("All immune cells", "Search, select 1~5 immune cell(s):"),
                                                             selected = "All immune cells"
                                                           ),
                                                           conditionalPanel(condition = "input.imm_infli_GSE_boxplot_type=='Search, select 1~5 immune cell(s):'",
                                                                            selectizeInput(
                                                                              inputId = "imm_infli_GSE_boxplot_immunecell",
                                                                              label = NULL, 
                                                                              choices = NULL, 
                                                                              multiple = TRUE,
                                                                              options = list(placeholder = "Select one immune cell at least!" ,
                                                                                             maxItems = 5)
                                                                            )),
                                                           shinydashboard::box(title =h4(class = "boxTitle","Color Option"),
                                                                               width=NULL,status="info",collapsible = T,collapsed=T, 
                                                                               fluidRow(
                                                                                 column(width=5,
                                                                                        colorPickr(
                                                                                          inputId = "imm_infli_GSE_boxplot_color_p",label = "HPV Positive:",selected = "#E69F00",
                                                                                          theme = "monolith",update ="change",
                                                                                          interaction = list(hex= FALSE,rgba = FALSE,input = FALSE,save = FALSE,clear = FALSE)) ),
                                                                                 column(width=5,offset = 1,
                                                                                        colorPickr(
                                                                                          inputId = "imm_infli_GSE_boxplot_color_n",label = "HPV Negative:",selected ="#999999",
                                                                                          theme = "monolith",update ="change",
                                                                                          interaction = list(hex= FALSE,rgba = FALSE,input = FALSE,save = FALSE,clear = FALSE))),
                                                                               )                
                                                           )
                                                    ),
                                                    column(width = 3,
                                                           actionButton('imm_infli_GSE_box_update', 
                                                                        "Visualize",icon = icon('palette'))
                                                    ),
                                                    
                                             ),
                                             
                                             
                         ),
                         
                         div(id="imm_infli_GSE_box_result_sum",
                             shinydashboard::tabBox(id="imm_infli_GSE_box_result_panel", title="Result",width = NULL,
                                                    tabPanel("Plot",
                                                             div(
                                                               fluidRow(
                                                                 column(width = 8,
                                                                        dropdownButton(
                                                                          inputId = "imm_infli_GSE_boxplot_introduction",
                                                                          label = "Plot Introduction",
                                                                          #icon = icon("question"),
                                                                          status = "primary",
                                                                          circle = FALSE,
                                                                          width="500px",
                                                                          ImmInfli_GSE_boxplot_text)),
                                                                 column(width = 4,
                                                                        dropdownButton(
                                                                          inputId = "imm_infli_GSE_boxplot_download",
                                                                          label = "Download",
                                                                          status = "primary",
                                                                          icon=icon("download"),
                                                                          circle = FALSE,
                                                                          width="50px",
                                                                          downloadButton(outputId = "imm_infli_GSE_boxplot_download_pdf",
                                                                                         label = "PDF"),
                                                                          downloadButton(outputId = "imm_infli_GSE_boxplot_download_svg",
                                                                                         label = "SVG"),
                                                                          downloadButton(outputId = "imm_infli_GSE_boxplot_download_png",
                                                                                         label = "PNG")))
                                                                 
                                                               ),
                                                               plotOutput("imm_infli_GSE_boxplot_result_plot_show")%>% withSpinner(color="#6c6689")
                                                             )),
                                                    tabPanel("Data",
                                                             fluidRow(
                                                               column(width = 7,
                                                                      dropdownButton(
                                                                        inputId = "imm_infli_GSE_box_fulldata_download",
                                                                        label = "Download Data",
                                                                        status = "primary",
                                                                        icon=icon("download"),
                                                                        circle = FALSE,
                                                                        width="50px",
                                                                        downloadButton(outputId = "imm_infli_GSE_box_fulldata_download_csv",
                                                                                       label = "CSV"),
                                                                        downloadButton(outputId = "imm_infli_GSE_box_fulldata_download_txt",
                                                                                       label = "TXT"))),
                                                               column(width = 5,
                                                                      helpText("Please wait as the table take a few seconds to load."))
                                                               
                                                             ),
                                                             uiOutput("imm_infli_GSE_box_result_data_panel"))))
                         
                         
                         
                       ),
                       
                       conditionalPanel(
                         condition = "input.imm_infli_GSE_plottype=='Multi-datasets'",
                         shinydashboard::box(width=NULL,
                                             column(width = 5, offset=1,
                                                    #h4(class = "boxTitle","Parameters"),
                                                    selectInput(
                                                      inputId = "imm_infli_immcell_method",
                                                      label = p("Which algorithm are you interested:", 
                                                                style="margin: 0 0 5px;",
                                                                icon(name = "question-sign",lib="glyphicon",id="imm_infli_immcell_method_q")),
                                                      choices =c("CIBERSORT_abs","MCPcounter","mMCPcounter","EPIC","xCell","quanTIseq","TIMER"),
                                                      selected="MCPcounter"
                                                    ),
                                                    
                                                    bsModal(id="modal_imm_infli_immcell_method_q", title="Algorithm Selection", trigger="imm_infli_immcell_method_q", 
                                                            size = "small",
                                                            p(class = "submitboxContext",
                                                              "1. The radio buttons let users select the method used to estimate the immune cells' infiltration levels.",br(),
                                                              "2. Due to the distinct principles, the methods that can be used for intersample comparisons between the same cell types are CIBERSORT absolute mode, MCP-counter (or mMCP-counter for mice), EPIC, quanTIseq, TIMER, and xCell."),
                                                            DT::datatable(imm_imfli_algorithm,width = "200px",height = "300px",
                                                                          caption="Table: More about algorithms.",
                                                                          rownames = FALSE,
                                                                          escape=FALSE,
                                                                          options = list(
                                                                            dom = 't',
                                                                            initComplete = JS(
                                                                              "function(settings, json) {",
                                                                              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                              "}")))),
                                                    
                                                    selectizeInput(
                                                      inputId = "imm_infli_immcell_dataset",
                                                      label = p("Search, select 1~5 dataset(s):", 
                                                                style="margin: 0 0 5px;",
                                                                icon(name = "question-sign",lib="glyphicon",id="imm_infli_immcell_dataset_q")),
                                                      choices = NULL,
                                                      multiple = TRUE,
                                                      options = list(placeholder = "Select one dataset at least!" ,
                                                                     maxItems = 5)
                                                    ),
                                                    
                                                    bsModal(id="modal_imm_infli_immcell_dataset_q", title="Dataset Selection", trigger="imm_infli_immcell_dataset_q", 
                                                            size = "large",
                                                            p(class = "submitboxContext","The dataset name is a combination of the corresponding tumor name/cell line and GSE ID."),
                                                            p(class = "submitboxContext","For example: "),
                                                            p(class = "submitboxContext","OPSCC_GSE55544 indicates that GSE55544 is a dataset of oropharyngeal squamous cell carcinoma."),
                                                            DT::datatable(abbreviation,width = "500px",height = "300px",
                                                                          caption="Table: Abbreviation for cancer/cell line.",
                                                                          rownames = FALSE,
                                                                          options = list(
                                                                            dom = 'tpr',
                                                                            pageLength = 5,
                                                                            initComplete = JS(
                                                                              "function(settings, json) {",
                                                                              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                              "}")
                                                                            
                                                                          )
                                                            ))
                                             ),
                                             column(width=5,
                                                    #br(),
                                                    selectizeInput(
                                                      inputId = "imm_infli_immcell_boxplot_immunecell",
                                                      label = p("Search, select an immune cell:",style="margin: 0 0 5px;"), 
                                                      choices = NULL, 
                                                      multiple = FALSE
                                                    ),
                                                    
                                                    shinydashboard::box(title =h4(class = "boxTitle","Color Option"),
                                                                        width=NULL,status="info",collapsible = T,collapsed=T, 
                                                                        fluidRow(
                                                                          column(width=5,
                                                                                 colorPickr(
                                                                                   inputId = "imm_infli_immcell_boxplot_color_p",label = "HPV Positive:",selected = "#E69F00",
                                                                                   theme = "monolith",update ="change",
                                                                                   interaction = list(hex= FALSE,rgba = FALSE,input = FALSE,save = FALSE,clear = FALSE)) ),
                                                                          column(width=5,offset = 1,
                                                                                 colorPickr(
                                                                                   inputId = "imm_infli_immcell_boxplot_color_n",label = "HPV Negative:",selected ="#999999",
                                                                                   theme = "monolith",update ="change",
                                                                                   interaction = list(hex= FALSE,rgba = FALSE,input = FALSE,save = FALSE,clear = FALSE))),
                                                                        )               
                                                    ),
                                                    actionButton('imm_infli_immcell_update', 
                                                                 "Visualize",icon = icon('palette')),
                                                    )
                         ),
                         div(id="imm_infli_immcell_result_smu",
                             shinydashboard::tabBox(id="imm_infli_immcell_result_panel", title="Result",width = NULL,
                                                    tabPanel("Plot",
                                                             div(
                                                               fluidRow(
                                                                 column(width = 8,
                                                                        dropdownButton(
                                                                          inputId = "imm_infli_immcell_boxplot_introduction",
                                                                          label = "Plot Introduction",
                                                                          #icon = icon("question"),
                                                                          status = "primary",
                                                                          circle = FALSE,
                                                                          width="500px",
                                                                          ImmInfli_cell_boxplot_text)),
                                                                 column(width = 4,
                                                                        dropdownButton(
                                                                          inputId = "imm_infli_immcell_boxplot_download",
                                                                          label = "Download",
                                                                          status = "primary",
                                                                          icon=icon("download"),
                                                                          circle = FALSE,
                                                                          width="50px",
                                                                          downloadButton(outputId = "imm_infli_immcell_boxplot_download_pdf",
                                                                                         label = "PDF"),
                                                                          downloadButton(outputId = "imm_infli_immcell_boxplot_download_svg",
                                                                                         label = "SVG"),
                                                                          downloadButton(outputId = "imm_infli_immcell_boxplot_download_png",
                                                                                         label = "PNG")))
                                                                 
                                                               ),
                                                               plotOutput("imm_infli_immcell_result_plot_panel")%>% withSpinner(color="#6c6689"))
                                                    ))
                         )
                         
                       )
                       
                       
                       
                )
                
                
              ),
              column(12,class = "footer-container",
                     HTML('<div style="text-align: center;"><p>Copyright © 2023. All rights reserved.</p></div> <div style="text-align: center;">
                     <div style="display:inline-block;width:400px;"></div> </div>'))
              
              
      ),
      
      ##=================================8.imm_infli_meta=============================
      tabItem(tabName = "imm_infli_meta",
              fluidRow( #Row 2 : introduction and option
                column(width=12,align="center",style="padding-top:10px",
                       actionBttn(
                         inputId = "imm_infli_meta_title",
                         label = h1(class = "pageTitle","Meta Analysis for Immune Cells",
                                    icon(name = "search",lib="glyphicon")), 
                         style = "minimal",
                         color = "danger",
                         size = "lg")),
                bsModal(id="modal_imm_infli_meta_title", title="Introduction", trigger="imm_infli_meta_title", 
                        size = "large",imm_infli_meta_text),
                
                column(width=12,style = "padding-right:40px;padding-left:40px",
                       shinydashboard::box(width=NULL,
                                           column(width = 12,
                                                  column(width = 5,style = "padding-right:15px;padding-left:0px",
                                                         selectInput(
                                                           inputId = "imm_infli_forest_method",
                                                           label = p("Which algorithm are you interested:", 
                                                                     style="margin: 0 0 5px;",
                                                                     icon(name = "question-sign",lib="glyphicon",id="imm_infli_forest_method_q")),
                                                           choices =c("CIBERSORT_abs","MCPcounter","mMCPcounter","EPIC","xCell","quanTIseq","TIMER"),
                                                           selected="CIBERSORT_abs"
                                                         ),
                                                         
                                                         bsModal(id="modal_imm_infli_forest_method_q", title="Algorithm Selection", trigger="imm_infli_forest_method_q", 
                                                                 size = "small",
                                                                 p(class = "submitboxContext",
                                                                   "1. The radio buttons let users select the method used to estimate the immune cells' infiltration levels.",br(),
                                                                   "2. Due to the distinct principles, the methods that can be used for intersample comparisons between the same cell types are CIBERSORT absolute mode, MCP-counter (or mMCP-counter for mice), EPIC, quanTIseq, TIMER, and xCell."),
                                                                 DT::datatable(imm_imfli_algorithm,width = "200px",height = "300px",
                                                                               caption="Table: More about algorithms.",
                                                                               rownames = FALSE,
                                                                               escape=FALSE,
                                                                               options = list(
                                                                                 dom = 't',
                                                                                 initComplete = JS(
                                                                                   "function(settings, json) {",
                                                                                   "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                                   "}"))))
                                                  ),
                                                  column(width = 5,
                                                         selectizeInput(
                                                           inputId = "imm_infli_forest_immunecell",
                                                           label = p("Search, select an immune cell:",style="margin: 0 0 5px;"), 
                                                           choices = NULL, multiple = FALSE)),
                                                  column(width = 2,style="padding-top:25px",
                                                         actionButton('imm_infli_forest_update', 
                                                                      "Visualize",icon = icon('palette')))
                                           ),
                                           
                                           column(width = 12,
                                                  shinydashboard::box(title =h4(class = "boxTitle","Customize"),
                                                                      width=NULL,status="primary",collapsible = T,collapsed=T,
                                                                      
                                                                      column(width = 5,style = "padding-right:15px;padding-left:0px",
                                                                             selectInput(
                                                                               inputId = "imm_infli_forest_model",
                                                                               label = p("Meta-analysis model:", 
                                                                                         style="margin: 0 0 5px;",
                                                                                         icon(name = "question-sign",lib="glyphicon",id="imm_infli_forest_model_q"),
                                                                                         bsTooltip("imm_infli_forest_model_q", 
                                                                                                   "Choose fixed effect or random effect models.",
                                                                                                   "right", trigger="hover",options = list(container = "body"))),
                                                                               choices =c("Fixed effects model","Random effects model","Both"),
                                                                               selected = "Random effects model")
                                                                      ),
                                                                      
                                                                      column(width = 5,
                                                                             selectInput(
                                                                               inputId = "imm_infli_forest_sm",
                                                                               label = p("Summary Measure:", 
                                                                                         style="margin: 0 0 5px;",
                                                                                         icon(name = "question-sign",lib="glyphicon",id="imm_infli_forest_sm_q")),
                                                                               
                                                                               choices =c("MD","SMD","ROM"),
                                                                               selected = "SMD"),
                                                                             
                                                                             bsModal(id="modal_imm_infli_forest_sm_q", title="Summary Measure", 
                                                                                     trigger="imm_infli_forest_sm_q", 
                                                                                     size = "small",
                                                                                     p(class = "submitboxContext",
                                                                                       "A character string indicating which summary measure is to be used for pooling of studies.",br(),
                                                                                       "MD: mean difference.",br(),"SMD: standardised mean difference.",br(),"ROM: ratio of means.")
                                                                             ),
                                                                             checkboxInput(
                                                                               inputId = "imm_infli_forest_subgroup",
                                                                               label = p("Subgroup by Diease.",
                                                                                         style="margin: 0 0 5px;",
                                                                                         icon(name = "question-sign",lib="glyphicon",id="imm_infli_forest_subgroup_q"),
                                                                                         bsTooltip("imm_infli_forest_subgroup_q", 
                                                                                                   "Whether to conduct a meta-analysis of disease subgroups.",
                                                                                                   "right", trigger="hover",options = list(container = "body"))),
                                                                               value = TRUE)
                                                                      )
                                                                      
                                                  )
                                                  
                                           )
                                           
                                           
                                           
                       ),
                       
                       div(id="imm_infli_forest_result_smu",
                           shinydashboard::tabBox(id="imm_infli_forest_result_panel", title="Result",width = NULL,
                                                  tabPanel("Plot",
                                                           div(
                                                             fluidRow(
                                                               column(width = 8,
                                                                      dropdownButton(
                                                                        inputId = "imm_infli_forest_introduction",
                                                                        label = "Plot Introduction",
                                                                        #icon = icon("question"),
                                                                        status = "primary",
                                                                        circle = FALSE,
                                                                        width="500px",
                                                                        forest_plot_text)),
                                                               column(width = 4,
                                                                      dropdownButton(
                                                                        inputId = "imm_infli_forest_download",
                                                                        label = "Download",
                                                                        status = "primary",
                                                                        icon=icon("download"),
                                                                        circle = FALSE,
                                                                        width="50px",
                                                                        downloadButton(outputId = "imm_infli_forest_download_pdf",
                                                                                       label = "PDF"),
                                                                        downloadButton(outputId = "imm_infli_forest_download_svg",
                                                                                       label = "SVG"),
                                                                        downloadButton(outputId = "imm_infli_forest_download_png",
                                                                                       label = "PNG")))
                                                               
                                                             ),
                                                             plotOutput("imm_infli_forest_result_plot_panel")%>% withSpinner(color="#6c6689"))
                                                  ),
                                                  tabPanel("Data",
                                                           dropdownButton(
                                                             inputId = "imm_infli_forest_fulldata_download",
                                                             label = "Download Full Data",
                                                             status = "primary",
                                                             icon=icon("download"),
                                                             circle = FALSE,
                                                             width="50px",
                                                             downloadButton(outputId = "imm_infli_forest_fulldata_download_csv",
                                                                            label = "CSV"),
                                                             downloadButton(outputId = "imm_infli_forest_fulldata_download_txt",
                                                                            label = "TXT")),
                                                           uiOutput("imm_infli_forest_result_data_panel"))
                           )
                       )
                       
                )
                
                
              ),
              column(12,class = "footer-container",
                     HTML('<div style="text-align: center;"><p>Copyright © 2023. All rights reserved.</p></div> <div style="text-align: center;">
                     <div style="display:inline-block;width:400px;"></div> </div>'))
              
              
      ),
      
      
      ##=================================9.ClusterProfiler=============================
      tabItem(tabName = "ClusterProfiler",
              
              fluidRow( #Row 2 : introduction and option
                #align="center",
                column(width=12,style = "padding-right:40px;padding-left:40px",
                       
                       column(width=12,align="center",style="padding-top:10px",
                              actionBttn(
                                inputId = "enrich_cluster_title",
                                label = h1(class = "pageTitle","Pathway Enrichment",icon(name = "search",lib="glyphicon")), 
                                style = "minimal",
                                color = "danger",
                                size = "lg")),
                       bsModal(id="modal_enrich_cluster_title", title="Introduction", trigger="enrich_cluster_title", 
                               size = "large",enrich_cluster_oragsea_text),
                       
                       radioGroupButtons(
                         inputId = "enrich_cluster_type",#width="900px",
                         label = NULL,
                         choices = c("Over Representation Analysis(ORA)","Gene Set Enrichment Analysis(GSEA)"),
                         justified = TRUE,
                         status = "warning"
                       ),
                       conditionalPanel(
                         condition ="input.enrich_cluster_type=='Over Representation Analysis(ORA)'" ,
                         shinydashboard::box(width=NULL,
                                             column(width = 5, offset=1,
                                                    #h4(class = "boxTitle","Parameters"),
                                                    selectizeInput(
                                                      inputId = "enrich_cluster_ora_dataset",
                                                      label =p("Search, select a dataset:", style="margin: 0 0 5px;",
                                                               icon(name = "question-sign",lib="glyphicon",id="enrich_cluster_ora_dataset_q")),
                                                      #choices = GSEdatalist$name[which(GSEdatalist$clusterprofiler==TRUE)],
                                                      choices= setNames(GSEdatalist$name[which(GSEdatalist$clusterprofiler==TRUE)],
                                                                        GSEdatalist$samplenum[which(GSEdatalist$clusterprofiler==TRUE)]),
                                                      selected="HNSCC_GSE74927"
                                                    ),
                                                    
                                                    bsModal(id="modal_enrich_cluster_ora_dataset_q", title="Dataset Selection", trigger="enrich_cluster_ora_dataset_q", 
                                                            size = "large",
                                                            p(class = "submitboxContext","The dataset name is a combination of the corresponding tumor name/cell line and GSE ID."),
                                                            p(class = "submitboxContext","For example: "),
                                                            p(class = "submitboxContext","OPSCC_GSE55544 indicates that GSE55544 is a dataset of oropharyngeal squamous cell carcinoma."),
                                                            DT::datatable(abbreviation,width = "500px",height = "300px",
                                                                          caption="Table: Abbreviation for cancer/cell line.",
                                                                          rownames = FALSE,
                                                                          options = list(
                                                                            dom = 'tpr',
                                                                            pageLength = 5,
                                                                            initComplete = JS(
                                                                              "function(settings, json) {",
                                                                              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                              "}")
                                                                            
                                                                          )
                                                            )),
                                                    
                                                    selectInput(
                                                      inputId = "enrich_cluster_ora_pathwayset",
                                                      label = p("Pick genesets for enrichment:",style="margin: 0 0 5px;",), 
                                                      choices = c("GO(Gene Ontology gene sets)",
                                                                  "KEGG(Kyoto Encyclopedia of Genes and Genomes)",
                                                                  "H(hallmark gene sets)",
                                                                  "CP(Canonical pathways)",
                                                                  "C5(ontology gene sets)")
                                                    )
                                             ),
                                             column(width = 5,
                                                    br(),
                                                    radioButtons( inputId = "enrich_cluster_ora_path_num", 
                                                                  label = p("Choose the pathways to display:",
                                                                            style="margin: 0 0 5px;",
                                                                            icon(name = "question-sign",lib="glyphicon",id="enrich_cluster_ora_path_num_q"),
                                                                            bsTooltip("enrich_cluster_ora_path_num_q", 
                                                                                      paste0("<p>Top20: Extract pathways within the top20 of enrichment score with p.adjust<0.05.</p>",
                                                                                             "<p>Customize: Manually select your pathways of interest.</p>"),
                                                                                      "right", trigger="hover",options = list(container = "body"))),
                                                                  choices = c("Top20","Customize"),
                                                                  selected = "Top20"
                                                    ),
                                                    
                                                    conditionalPanel(condition = "input.enrich_cluster_ora_path_num=='Customize'",
                                                                     selectizeInput(
                                                                       inputId = "enrich_cluster_ora_path_category",
                                                                       label=NULL,
                                                                       choices = NULL, 
                                                                       multiple = TRUE,
                                                                       options = list(maxItems = 20)
                                                                     )
                                                    ),
                                                    actionButton('enrich_cluster_ora_update', 
                                                                 "Visualize",icon = icon('palette')),
                                                    
                                                    
                                             )
                         ),
                         div(id="enrich_cluster_ora_result_smu", 
                             shinydashboard::tabBox(id="enrich_cluster_ora_result_panel", title="Result",width = NULL,
                                                    tabPanel("Bar Plot",
                                                             div(
                                                               fluidRow(
                                                                 column(width = 8,
                                                                        dropdownButton(
                                                                          inputId = "enrich_cluster_ora_barplot_introduction",
                                                                          label = "Plot Introduction",
                                                                          #icon = icon("question"),
                                                                          status = "primary",
                                                                          circle = FALSE,
                                                                          cluster_ora_bar_text)),
                                                                 column(width = 4,
                                                                        dropdownButton(
                                                                          inputId = "enrich_cluster_ora_barplot_download",
                                                                          label = "Download",
                                                                          status = "primary",
                                                                          icon=icon("download"),
                                                                          circle = FALSE,
                                                                          width="50px",
                                                                          downloadButton(outputId = "enrich_cluster_ora_barplot_download_pdf",
                                                                                         label = "PDF"),
                                                                          downloadButton(outputId = "enrich_cluster_ora_barplot_download_svg",
                                                                                         label = "SVG"),
                                                                          downloadButton(outputId = "enrich_cluster_ora_barplot_download_png",
                                                                                         label = "PNG")))
                                                                 
                                                               ),
                                                               plotOutput("enrich_cluster_ora_result_barplot_show")%>% withSpinner(color="#6c6689"))
                                                    ),        
                                                    tabPanel("Dot plot",
                                                             div(
                                                               fluidRow(
                                                                 column(width = 8,
                                                                        dropdownButton(
                                                                          inputId = "enrich_cluster_ora_dotplot_introduction",
                                                                          label = "Plot Introduction",
                                                                          #icon = icon("question"),
                                                                          status = "primary",
                                                                          circle = FALSE,
                                                                          cluster_ora_dot_text)),
                                                                 column(width = 4,
                                                                        dropdownButton(
                                                                          inputId = "enrich_cluster_ora_dotplot_download",
                                                                          label = "Download",
                                                                          status = "primary",
                                                                          icon=icon("download"),
                                                                          circle = FALSE,
                                                                          width="50px",
                                                                          downloadButton(outputId = "enrich_cluster_ora_dotplot_download_pdf",
                                                                                         label = "PDF"),
                                                                          downloadButton(outputId = "enrich_cluster_ora_dotplot_download_svg",
                                                                                         label = "SVG"),
                                                                          downloadButton(outputId = "enrich_cluster_ora_dotplot_download_png",
                                                                                         label = "PNG")))
                                                                 
                                                               ),
                                                               plotOutput("enrich_cluster_ora_result_dotplot_show")%>% withSpinner(color="#6c6689")) 
                                                    ),
                                                    
                                                    tabPanel("Enrichment Map",
                                                             div(
                                                               fluidRow(
                                                                 column(width = 8,
                                                                        dropdownButton(
                                                                          inputId = "enrich_cluster_ora_enrichmentmap_introduction",
                                                                          label = "Plot Introduction",
                                                                          #icon = icon("question"),
                                                                          status = "primary",
                                                                          circle = FALSE,
                                                                          cluster_ora_map_text)),
                                                                 column(width = 4,
                                                                        dropdownButton(
                                                                          inputId = "enrich_cluster_ora_enrichmentmap_download",
                                                                          label = "Download",
                                                                          status = "primary",
                                                                          icon=icon("download"),
                                                                          circle = FALSE,
                                                                          width="50px",
                                                                          downloadButton(outputId = "enrich_cluster_ora_enrichmentmap_download_pdf",
                                                                                         label = "PDF"),
                                                                          downloadButton(outputId = "enrich_cluster_ora_enrichmentmap_download_svg",
                                                                                         label = "SVG"),
                                                                          downloadButton(outputId = "enrich_cluster_ora_enrichmentmap_download_png",
                                                                                         label = "PNG")))
                                                                 
                                                               ),
                                                               plotOutput("enrich_cluster_ora_result_enrichmentmap_show")%>% withSpinner(color="#6c6689"))
                                                    ),
                                                    
                                                    tabPanel("Data",
                                                             dropdownButton(
                                                               inputId = "enrich_cluster_ora_fulldata_download",
                                                               label = "Download Full Data",
                                                               status = "primary",
                                                               icon=icon("download"),
                                                               circle = FALSE,
                                                               width="50px",
                                                               downloadButton(outputId = "enrich_cluster_ora_fulldata_download_csv",
                                                                              label = "CSV"),
                                                               downloadButton(outputId = "enrich_cluster_ora_fulldata_download_txt",
                                                                              label = "TXT")),
                                                             uiOutput("enrich_cluster_ora_result_data_panel")))
                         )
                       ),
                       
                       conditionalPanel(
                         condition ="input.enrich_cluster_type=='Gene Set Enrichment Analysis(GSEA)'" ,
                         shinydashboard::box(width=NULL,
                                             column(width = 5,offset = 1,
                                                    #h4(class = "boxTitle","Parameters"),
                                                    selectizeInput(
                                                      inputId = "enrich_cluster_gsea_dataset",
                                                      label = p("Search, select a dataset:",style="margin: 0 0 5px;",
                                                                icon(name = "question-sign",lib="glyphicon",id="enrich_cluster_gsea_dataset_q")), 
                                                      #choices = GSEdatalist$name[which(GSEdatalist$clusterprofiler==TRUE)],
                                                      choices= setNames(GSEdatalist$name[which(GSEdatalist$clusterprofiler==TRUE)],
                                                                        GSEdatalist$samplenum[which(GSEdatalist$clusterprofiler==TRUE)]),
                                                      selected="HNSCC_GSE74927"
                                                    ),
                                                    
                                                    bsModal(id="modal_enrich_cluster_gsea_dataset_q", title="Dataset Selection", trigger="enrich_cluster_gsea_dataset_q", 
                                                            size = "large",
                                                            p(class = "submitboxContext","The dataset name is a combination of the corresponding tumor name/cell line and GSE ID."),
                                                            p(class = "submitboxContext","For example: "),
                                                            p(class = "submitboxContext","OPSCC_GSE55544 indicates that GSE55544 is a dataset of oropharyngeal squamous cell carcinoma."),
                                                            DT::datatable(abbreviation,width = "500px",height = "300px",
                                                                          caption="Table: Abbreviation for cancer/cell line.",
                                                                          rownames = FALSE,
                                                                          options = list(
                                                                            dom = 'tpr',
                                                                            pageLength = 5,
                                                                            initComplete = JS(
                                                                              "function(settings, json) {",
                                                                              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                              "}")
                                                                            
                                                                          )
                                                            )),
                                                    
                                                    selectInput(
                                                      inputId = "enrich_cluster_gsea_pathwayset",
                                                      label = p("Pick genesets for enrichment:",style="margin: 0 0 5px;"), 
                                                      choices = c("GO(Gene Ontology gene sets)",
                                                                  "KEGG(Kyoto Encyclopedia of Genes and Genomes)",
                                                                  "H(hallmark gene sets)",
                                                                  "CP(Canonical pathways)",
                                                                  "C5(ontology gene sets)"),
                                                      selected = "H(hallmark gene sets)"
                                                    ),
                                                    
                                             ),
                                             column(width = 5,
                                                    br(),
                                                    radioButtons( inputId = "enrich_cluster_gsea_path_num", 
                                                                  label = p("Choose the pathways to display:",
                                                                            style="margin: 0 0 5px;",
                                                                            icon(name = "question-sign",lib="glyphicon",id="enrich_cluster_gsea_path_num_q"),
                                                                            bsTooltip("enrich_cluster_gsea_path_num_q", 
                                                                                      paste0("<p>Top20: Extract pathways within the top20 of enrichment score with p.adjust<0.05.</p>",
                                                                                             "<p>Customize: Manually select your pathways of interest.</p>"),
                                                                                      "right", trigger="hover",options = list(container = "body"))),
                                                                  choices = c("Top20","Customize"),
                                                                  selected = "Top20"),
                                                    
                                                    conditionalPanel(condition = "input.enrich_cluster_gsea_path_num=='Customize'",
                                                                     selectizeInput(
                                                                       inputId = "enrich_cluster_gsea_path_category",
                                                                       label=NULL,
                                                                       choices = NULL, 
                                                                       multiple = TRUE,
                                                                       options = list(maxItems = 20)
                                                                     )
                                                    ),
                                                    
                                                    actionButton('enrich_cluster_gsea_update', 
                                                                 "Visualize",icon = icon('palette')),
                                                    
                                                    
                                             )
                         ),
                         div(id="enrich_cluster_gsea_result_smu",
                             shinydashboard::tabBox(id="enrich_cluster_gsea_result_panel", title="Result",width = NULL,
                                                    tabPanel("Bar Plot",
                                                             div(
                                                               fluidRow(
                                                                 column(width = 8,
                                                                        dropdownButton(
                                                                          inputId = "enrich_cluster_gsea_barplot_introduction",
                                                                          label = "Plot Introduction",
                                                                          #icon = icon("question"),
                                                                          status = "primary",
                                                                          circle = FALSE,
                                                                          cluster_gsea_bar_text)),
                                                                 column(width = 4,
                                                                        dropdownButton(
                                                                          inputId = "enrich_cluster_gsea_barplot_download",
                                                                          label = "Download",
                                                                          status = "primary",
                                                                          icon=icon("download"),
                                                                          circle = FALSE,
                                                                          width="50px",
                                                                          downloadButton(outputId = "enrich_cluster_gsea_barplot_download_pdf",
                                                                                         label = "PDF"),
                                                                          downloadButton(outputId = "enrich_cluster_gsea_barplot_download_svg",
                                                                                         label = "SVG"),
                                                                          downloadButton(outputId = "enrich_cluster_gsea_barplot_download_png",
                                                                                         label = "PNG")))
                                                                 
                                                               ),
                                                               plotOutput("enrich_cluster_gsea_result_barplot_show")%>% withSpinner(color="#6c6689"))
                                                    ),
                                                    
                                                    tabPanel("Dot Plot",
                                                             div(
                                                               fluidRow(
                                                                 column(width = 8,
                                                                        dropdownButton(
                                                                          inputId = "enrich_cluster_gsea_dotplot_introduction",
                                                                          label = "Plot Introduction",
                                                                          #icon = icon("question"),
                                                                          status = "primary",
                                                                          circle = FALSE,
                                                                          cluster_gsea_dot_text)),
                                                                 column(width = 4,
                                                                        dropdownButton(
                                                                          inputId = "enrich_cluster_gsea_dotplot_download",
                                                                          label = "Download",
                                                                          status = "primary",
                                                                          icon=icon("download"),
                                                                          circle = FALSE,
                                                                          width="50px",
                                                                          downloadButton(outputId = "enrich_cluster_gsea_dotplot_download_pdf",
                                                                                         label = "PDF"),
                                                                          downloadButton(outputId = "enrich_cluster_gsea_dotplot_download_svg",
                                                                                         label = "SVG"),
                                                                          downloadButton(outputId = "enrich_cluster_gsea_dotplot_download_png",
                                                                                         label = "PNG")))
                                                                 
                                                               ),
                                                               plotOutput("enrich_cluster_gsea_result_dotplot_show")%>% withSpinner(color="#6c6689"))
                                                    ),
                                                    
                                                    tabPanel("Enrichment Map",
                                                             div(
                                                               fluidRow(
                                                                 column(width = 8,
                                                                        dropdownButton(
                                                                          inputId = "enrich_cluster_gsea_enrichmentmap_introduction",
                                                                          label = "Plot Introduction",
                                                                          #icon = icon("question"),
                                                                          status = "primary",
                                                                          circle = FALSE,
                                                                          cluster_gsea_map_text)),
                                                                 column(width = 4,
                                                                        dropdownButton(
                                                                          inputId = "enrich_cluster_gsea_enrichmentmap_download",
                                                                          label = "Download",
                                                                          status = "primary",
                                                                          icon=icon("download"),
                                                                          circle = FALSE,
                                                                          width="50px",
                                                                          downloadButton(outputId = "enrich_cluster_gsea_enrichmentmap_download_pdf",
                                                                                         label = "PDF"),
                                                                          downloadButton(outputId = "enrich_cluster_gsea_enrichmentmap_download_svg",
                                                                                         label = "SVG"),
                                                                          downloadButton(outputId = "enrich_cluster_gsea_enrichmentmap_download_png",
                                                                                         label = "PNG")))
                                                                 
                                                               ),
                                                               plotOutput("enrich_cluster_gsea_result_enrichmentmap_show")%>% withSpinner(color="#6c6689"))
                                                    ),
                                                    
                                                    tabPanel("Ridgeline Plot",
                                                             div(
                                                               fluidRow(
                                                                 column(width = 8,
                                                                        dropdownButton(
                                                                          inputId = "enrich_cluster_gsea_ridgeline_introduction",
                                                                          label = "Plot Introduction",
                                                                          #icon = icon("question"),
                                                                          status = "primary",
                                                                          circle = FALSE,
                                                                          cluster_gsea_ridgeline_text)),
                                                                 column(width = 4,
                                                                        dropdownButton(
                                                                          inputId = "enrich_cluster_gsea_ridgeline_download",
                                                                          label = "Download",
                                                                          status = "primary",
                                                                          icon=icon("download"),
                                                                          circle = FALSE,
                                                                          width="50px",
                                                                          downloadButton(outputId = "enrich_cluster_gsea_ridgeline_download_pdf",
                                                                                         label = "PDF"),
                                                                          downloadButton(outputId = "enrich_cluster_gsea_ridgeline_download_svg",
                                                                                         label = "SVG"),
                                                                          downloadButton(outputId = "enrich_cluster_gsea_ridgeline_download_png",
                                                                                         label = "PNG")))
                                                                 
                                                               ),
                                                               plotOutput("enrich_cluster_gsea_result_ridgeline_show")%>% withSpinner(color="#6c6689"))
                                                    ),
                                                    
                                                    tabPanel("GSEA Plot & Data",
                                                             fluidRow(
                                                               column(width = 6,
                                                                      fluidRow(
                                                                        column(width = 2,
                                                                               h4(class = "boxTitle","Data")),
                                                                        column(width = 8,
                                                                               dropdownButton(
                                                                                 inputId = "enrich_cluster_gsea_fulldata_download",
                                                                                 label = "Download Full Data",
                                                                                 status = "primary",
                                                                                 icon=icon("download"),
                                                                                 circle = FALSE,
                                                                                 width="50px",
                                                                                 downloadButton(outputId = "enrich_cluster_gsea_fulldata_download_csv",
                                                                                                label = "CSV"),
                                                                                 downloadButton(outputId = "enrich_cluster_gsea_fulldata_download_txt",
                                                                                                label = "TXT")))
                                                                        
                                                                      ),
                                                                      dataTableOutput("enrich_cluster_gsea_result_gsea_data_show")
                                                               ),
                                                               column(width = 6,
                                                                      fluidRow(
                                                                        column(width = 4,
                                                                               h4(class = "boxTitle","GSEA plot")),
                                                                        column(width = 4,
                                                                               dropdownButton(
                                                                                 inputId = "enrich_cluster_gsea_gseaplot_introduction",
                                                                                 label = "Plot Introduction",
                                                                                 #icon = icon("question"),
                                                                                 status = "primary",
                                                                                 circle = FALSE,
                                                                                 width="250px",
                                                                                 cluster_gsea_gseaplot_text)),
                                                                        column(width = 4,
                                                                               dropdownButton(
                                                                                 inputId = "enrich_cluster_gsea_gseaplot_download",
                                                                                 label = "Download",
                                                                                 status = "primary",
                                                                                 icon=icon("download"),
                                                                                 circle = FALSE,
                                                                                 width="50px",
                                                                                 downloadButton(outputId = "enrich_cluster_gsea_gseaplot_download_pdf",
                                                                                                label = "PDF"),
                                                                                 downloadButton(outputId = "enrich_cluster_gsea_gseaplot_download_svg",
                                                                                                label = "SVG"),
                                                                                 downloadButton(outputId = "enrich_cluster_gsea_gseaplot_download_png",
                                                                                                label = "PNG")))
                                                                        
                                                                      ),
                                                                      jqui_resizable(plotOutput("enrich_cluster_gsea_result_gseaplot_show", width = '400px', height = '400px'))%>% withSpinner(color="#6c6689")
                                                               )
                                                             )
                                                             
                                                    )
                             )
                         )
                       )
                       
                       
                )
                
              ),
              column(12,class = "footer-container",
                     HTML('<div style="text-align: center;"><p>Copyright © 2023. All rights reserved.</p></div> <div style="text-align: center;">
                     <div style="display:inline-block;width:400px;"></div> </div>'))
              
              
      ),
      ##=================================10.pathview=============================
      tabItem(tabName = "Pathview",
              
              fluidRow( #Row 2 : introduction and option
                
                column(width=12,align="center",style="padding-top:10px",
                       actionBttn(
                         inputId = "enrich_pathview_title",
                         label = h1(class = "pageTitle","Pathview",icon(name = "search",lib="glyphicon")), 
                         style = "minimal",
                         color = "danger",
                         size = "lg")),
                bsModal(id="modal_enrich_pathview_title", title="Introduction", trigger="enrich_pathview_title", 
                        size = "large",enrich_pathview_text),
                
                column(width=12,style = "padding-right:40px;padding-left:40px",
                       
                       shinydashboard::box(width=NULL,
                                           column(width = 5,offset = 1,
                                                  #h4(class = "boxTitle","Parameters"),
                                                  selectizeInput(
                                                    inputId = "enrich_pathview_dataset",
                                                    label =p("Search, select a dataset:",
                                                             style="margin: 0 0 5px;",
                                                             icon(name = "question-sign",lib="glyphicon",id="enrich_pathview_dataset_q")),
                                                    #choices = GSEdatalist$name[which(GSEdatalist$pathview==TRUE)],
                                                    choices= setNames(GSEdatalist$name[which(GSEdatalist$pathview==TRUE)],
                                                                      GSEdatalist$samplenum[which(GSEdatalist$pathview==TRUE)]),
                                                    selected="OPSCC_GSE55544"
                                                  ),
                                                  
                                                  bsModal(id="modal_enrich_pathview_dataset_q", title="Dataset Selection", trigger="enrich_pathview_dataset_q", 
                                                          size = "large",
                                                          p(class = "submitboxContext","The dataset name is a combination of the corresponding tumor name/cell line and GSE ID."),
                                                          p(class = "submitboxContext","For example: "),
                                                          p(class = "submitboxContext","OPSCC_GSE55544 indicates that GSE55544 is a dataset of oropharyngeal squamous cell carcinoma."),
                                                          DT::datatable(abbreviation,width = "500px",height = "300px",
                                                                        caption="Table: Abbreviation for cancer/cell line.",
                                                                        rownames = FALSE,
                                                                        options = list(
                                                                          dom = 'tpr',
                                                                          pageLength = 5,
                                                                          initComplete = JS(
                                                                            "function(settings, json) {",
                                                                            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                            "}")
                                                                          
                                                                        )
                                                          )),
                                                  
                                                  selectizeInput(inputId = "enrich_pathview_pathway_name",
                                                                 label=p("Search,select a KEGG pathway：",style="margin: 0 0 5px;"),
                                                                 choices = NULL
                                                  )
                                           ),
                                           column(width = 5,
                                                  br(),br(),br(),br(),br(),
                                                  actionButton('enrich_pathview_update', 
                                                               "Visualize",icon = icon('palette')),
                                                  
                                                  
                                           )
                       ),
                       
                       div(id="enrich_pathview_result_smu",
                           shinydashboard::tabBox(id="enrich_pathview_result_panel", title="Result",width = NULL,
                                                  tabPanel("Plot",
                                                           div(
                                                             fluidRow(
                                                               column(width = 8,
                                                                      dropdownButton(
                                                                        inputId = "enrich_pathview_plot_introduction",
                                                                        label = "Plot Introduction",
                                                                        #icon = icon("question"),
                                                                        status = "primary",
                                                                        circle = FALSE,
                                                                        enrichment_pathviewplot_text)),
                                                               column(width = 4,
                                                                      dropdownButton(
                                                                        inputId = "enrich_pathview_plot_download",
                                                                        label = "Download",
                                                                        status = "primary",
                                                                        icon=icon("download"),
                                                                        circle = FALSE,
                                                                        width="50px",
                                                                        downloadButton(outputId = "enrich_pathview_plot_download_png",
                                                                                       label = "PNG")))
                                                               
                                                             ),
                                                             imageOutput("enrich_pathview_result_plot_panel")%>% withSpinner(color="#6c6689"))
                                                  ))
                       )
                       
                )
                
              ),
              column(12,class = "footer-container",
                     HTML('<div style="text-align: center;"><p>Copyright © 2023. All rights reserved.</p></div> <div style="text-align: center;">
                     <div style="display:inline-block;width:400px;"></div> </div>'))
              
              
      ),
      
      ##=================================11.ssgsea=============================
      tabItem(tabName = "ssGSEA",
              
              fluidRow( #Row 2 : introduction and option
                
                column(width=12,align="center",style="padding-top:10px",
                       actionBttn(
                         inputId = "enrich_ssgsea_title",
                         label = h1(class = "pageTitle","Single Sample Gene Set Enrichment Analysis",
                                    icon(name = "search",lib="glyphicon")), 
                         style = "minimal",
                         color = "danger",
                         size = "lg")),
                bsModal(id="modal_enrich_ssgsea_title", title="Introduction", trigger="enrich_ssgsea_title", 
                        size = "large",enrich_ssgsea_text),
                
                column(width=12,style = "padding-right:40px;padding-left:40px",
                       
                       shinydashboard::box(width=NULL,
                                           column(width = 5,offset = 1,
                                                  #h4(class = "boxTitle","Parameters"),
                                                  selectizeInput(
                                                    inputId = "enrich_ssgsea_dataset",
                                                    label =p("Search, select a dataset:", style="margin: 0 0 5px;",
                                                             icon(name = "question-sign",lib="glyphicon",id="enrich_ssgsea_dataset_q")),  
                                                    #choices = GSEdatalist$name[which(GSEdatalist$ssgsea==TRUE)],
                                                    choices= setNames(GSEdatalist$name[which(GSEdatalist$ssgsea==TRUE)],
                                                                      GSEdatalist$samplenum[which(GSEdatalist$ssgsea==TRUE)]),
                                                    selected="HNSCC_GSE74927"
                                                  ),
                                                  
                                                  bsModal(id="modal_enrich_ssgsea_dataset_q", title="Dataset Selection", trigger="enrich_ssgsea_dataset_q", 
                                                          size = "large",
                                                          p(class = "submitboxContext","The dataset name is a combination of the corresponding tumor name/cell line and GSE ID."),
                                                          p(class = "submitboxContext","For example: "),
                                                          p(class = "submitboxContext","OPSCC_GSE55544 indicates that GSE55544 is a dataset of oropharyngeal squamous cell carcinoma."),
                                                          DT::datatable(abbreviation,width = "500px",height = "300px",
                                                                        caption="Table: Abbreviation for cancer/cell line.",
                                                                        rownames = FALSE,
                                                                        options = list(
                                                                          dom = 'tpr',
                                                                          pageLength = 5,
                                                                          initComplete = JS(
                                                                            "function(settings, json) {",
                                                                            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                            "}")
                                                                          
                                                                        )
                                                          )),
                                                  
                                                  selectizeInput(
                                                    inputId = "enrich_ssgsea_pathway",
                                                    label =  p("Search,select 1~20 pathway(s):",
                                                               style="margin: 0 0 5px;",
                                                               icon(name = "question-sign",lib="glyphicon",id="enrich_ssgsea_pathway_q"),
                                                               bsTooltip("enrich_ssgsea_pathway_q", 
                                                                         paste0("<p>Only GO terms, KEGG pathways and partial molecular signature databases are provided, including hallmark gene sets (H), GO gene sets (C5) and canonical pathways (CP).</p>"),
                                                                         "right", trigger="hover",options = list(container = "body"))
                                                    ),
                                                    choices = NULL, 
                                                    multiple = TRUE,
                                                    options = list(placeholder = "Select one pathway at least!" ,
                                                                   maxItems = 20)
                                                  )
                                           ),
                                           column(width = 5,
                                                  
                                                  shinydashboard::box(title =h4(class = "boxTitle","Color Option"),
                                                                      width=NULL,status="info",collapsible = T,collapsed=T, 
                                                                      pickerInput(
                                                                        inputId = "enrich_ssgsea_heatmap_color",
                                                                        label = "Select colorscale:", 
                                                                        choices=c("light Red-Blue","Red-Blue","Purple-Green",
                                                                                  "Pink-Green","Brown-BlueyGreen","Red-Yellow-Blue","Green-Yellow")
                                                                      )              
                                                  ),
                                                  
                                                  actionButton('enrich_ssgsea_update', 
                                                               "Visualize",icon = icon('palette')),
                                                  
                                                  
                                           )
                       ),
                       div(id="enrich_ssgsea_result_smu",
                           shinydashboard::tabBox(id="enrich_ssgsea_result_panel", title="Result",width = NULL,
                                                  tabPanel("Plot",
                                                           div(
                                                             fluidRow(
                                                               column(width = 8,
                                                                      dropdownButton(
                                                                        inputId = "enrich_ssgsea_plot_introduction",
                                                                        label = "Plot Introduction",
                                                                        #icon = icon("question"),
                                                                        status = "primary",
                                                                        circle = FALSE,
                                                                        enrichment_ssgsea_text)),
                                                               column(width = 4,
                                                                      dropdownButton(
                                                                        inputId = "enrich_ssgsea_plot_download",
                                                                        label = "Download",
                                                                        status = "primary",
                                                                        icon=icon("download"),
                                                                        circle = FALSE,
                                                                        width="50px",
                                                                        downloadButton(outputId = "enrich_ssgsea_plot_download_pdf",
                                                                                       label = "PDF"),
                                                                        downloadButton(outputId = "enrich_ssgsea_plot_download_svg",
                                                                                       label = "SVG"),
                                                                        downloadButton(outputId = "enrich_ssgsea_plot_download_png",
                                                                                       label = "PNG")))
                                                               
                                                             ),
                                                             plotOutput("enrich_ssgsea_result_plot_panel")%>% withSpinner(color="#6c6689"))
                                                  ),
                                                  
                                                  tabPanel("Data",
                                                           dropdownButton(
                                                             inputId = "enrich_ssgsea_fulldata_download",
                                                             label = "Download Data",
                                                             status = "primary",
                                                             icon=icon("download"),
                                                             circle = FALSE,
                                                             width="50px",
                                                             downloadButton(outputId = "enrich_ssgsea_fulldata_download_csv",
                                                                            label = "CSV"),
                                                             downloadButton(outputId = "enrich_ssgsea_fulldata_download_txt",
                                                                            label = "TXT")),
                                                           dataTableOutput("enrich_ssgsea_result_data_panel"))))
                       
                       
                )
                
              ),
              column(12,class = "footer-container",
                     HTML('<div style="text-align: center;"><p>Copyright © 2023. All rights reserved.</p></div> <div style="text-align: center;">
                     <div style="display:inline-block;width:400px;"></div> </div>'))
              
              
      ),
      
      ##=================================5.Gene vs Gene/Pathway=============================
      tabItem(tabName = "genepathimmcor",
              
              fluidRow( 
                column(width=12,align="center",style="padding-top:10px",
                       actionBttn(
                         inputId = "corrScatter_title",
                         label = h1(class = "pageTitle","Correlation Analysis between Genes/Pathways/Immune cells",
                                    icon(name = "search",lib="glyphicon")), 
                         style = "minimal",
                         color = "danger",
                         size = "lg")),
                bsModal(id="modal_corrScatter_title", title="Introduction", trigger="corrScatter_title", 
                        size = "large",corrScatter_text),
                
                column(width=12,style = "padding-right:40px;padding-left:40px",
                       
                       shinydashboard::box(width=NULL,
                                           
                                           column(width = 12,
                                                  column(width = 5,offset = 1,
                                                         selectizeInput(
                                                           inputId = "corrScatter_dataset",
                                                           label =p("Search, select a dataset:",style="margin: 0 0 5px;",
                                                                    icon(name = "question-sign",lib="glyphicon",id="corrScatter_dataset_q")), 
                                                           #choices = GSEdatalist$name,
                                                           choices= setNames(GSEdatalist$name,GSEdatalist$samplenum),
                                                           selected="OPSCC_GSE55542"),
                                                         
                                                         bsModal(id="modal_corrScatter_dataset_q", title="Dataset Selection", trigger="corrScatter_dataset_q", 
                                                                 size = "large",
                                                                 p(class = "submitboxContext","The dataset name is a combination of the corresponding tumor name/cell line and GSE ID."),
                                                                 p(class = "submitboxContext","For example: "),
                                                                 p(class = "submitboxContext","OPSCC_GSE55544 indicates that GSE55544 is a dataset of oropharyngeal squamous cell carcinoma."),
                                                                 DT::datatable(abbreviation,width = "500px",height = "300px",
                                                                               caption="Table: Abbreviation for cancer/cell line.",
                                                                               rownames = FALSE,
                                                                               options = list(
                                                                                 dom = 'tpr',
                                                                                 pageLength = 5,
                                                                                 initComplete = JS(
                                                                                   "function(settings, json) {",
                                                                                   "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                                   "}")
                                                                                 
                                                                               )
                                                                 )),
                                                         
                                                         radioButtons( inputId = "corrScatter_status_single", 
                                                                       label =  p("Select the sample range for analysis：",style="margin: 0 0 5px;"),
                                                                       choices = c("HPV_Positive","HPV_Negative","All"),
                                                                       inline = TRUE),
                                                         radioButtons( inputId = "corrScatter_method_single", 
                                                                       label =  p("Select a method for correlation:",style="margin: 0 0 5px;"),
                                                                       choices = c("Pearson","Spearman"),
                                                                       inline = TRUE),
                                                         actionButton('corrScatter_update', 
                                                                      "Visualize",icon = icon('palette'))
                                                         
                                                  ),
                                                  column(width = 5,
                                                         radioButtons( inputId = "corrScatter_xaxis_type", 
                                                                       label =  p("Select, search an item (X axis):", style="margin: 0 0 5px;",
                                                                                  icon(name = "question-sign",lib="glyphicon",id="corrScatter_xaxis_type_q"),
                                                                                  bsTooltip("corrScatter_xaxis_type_q", 
                                                                                            paste0("<p>You can select gene expression, pathway ssGSEA scores, and immune cell infiltration scores for correlation analysis.</p>"),
                                                                                            "right", trigger="hover",options = list(container = "body")) ),
                                                                       choices = c("Gene","Pathway","Immune Cell"),
                                                                       inline = TRUE),
                                                         
                                                         selectizeInput(
                                                           inputId = "corrScatter_xaxis_item",multiple = FALSE,
                                                           label =NULL,choices = NULL),
                                                         
                                                         radioButtons( inputId = "corrScatter_yaxis_type", 
                                                                       label =  p("Select, search an item (Y axis):", style="margin: 0 0 5px;",
                                                                                  icon(name = "question-sign",lib="glyphicon",id="corrScatter_yaxis_type_q"),
                                                                                  bsTooltip("corrScatter_yaxis_type_q", 
                                                                                            paste0("<p>You can select gene expression, pathway ssGSEA scores, and immune cell infiltration scores for correlation analysis.</p>"),
                                                                                            "right", trigger="hover",options = list(container = "body")) ),
                                                                       choices = c("Gene","Pathway","Immune Cell"),
                                                                       inline = TRUE),
                                                         
                                                         selectizeInput(
                                                           inputId = "corrScatter_yaxis_item",multiple = FALSE,
                                                           label =NULL, choices = NULL)
                                                  )
                                                  
                                           ),
                                           column(width = 12,
                                                  
                                                  shinydashboard::box(title =h4(class = "boxTitle","Customize"),
                                                                      width=NULL,status="danger",collapsible = T,collapsed=T,
                                                                      
                                                                      column(width = 5,offset = 1,style="padding-right:15px;padding-left:15px",
                                                                             conditionalPanel(
                                                                               condition ="input.corrScatter_status_single=='All'",
                                                                               fluidRow(
                                                                                 column(width=5,
                                                                                        colorPickr(
                                                                                          inputId = "corrScatter_status_color_p",label = "HPV Positive:",selected = "#E69F00",
                                                                                          theme = "monolith",update ="change",
                                                                                          interaction = list(hex= FALSE,rgba = FALSE,input = FALSE,save = FALSE,clear = FALSE)) ),
                                                                                 column(width=5,offset = 1,
                                                                                        colorPickr(
                                                                                          inputId = "corrScatter_status_color_n",label = "HPV Negative:",selected ="#999999",
                                                                                          theme = "monolith",update ="change",
                                                                                          interaction = list(hex= FALSE,rgba = FALSE,input = FALSE,save = FALSE,clear = FALSE)))
                                                                               )
                                                                             ),
                                                                             
                                                                             conditionalPanel(
                                                                               condition ="input.corrScatter_status_single=='HPV_Positive'| input.corrScatter_status_single=='HPV_Negative' ",
                                                                               fluidRow(
                                                                                 column(width=5,
                                                                                        colorPickr(
                                                                                          inputId = "corrScatter_status_color_single",label =NULL,selected = "#E69F00",
                                                                                          theme = "monolith",update ="change",
                                                                                          interaction = list(hex= FALSE,rgba = FALSE,input = FALSE,save = FALSE,clear = FALSE)) )
                                                                               )
                                                                             )
                                                                             
                                                                      )
                                                                      
                                                  )
                                                  
                                           )
                                           
                                           
                                           
                                           
                       ),
                       div(id="corrScatter_result_smu",
                           shinydashboard::tabBox(id="corrScatter_result_panel", title="Result",width = NULL,
                                                  tabPanel("Plot",
                                                           div(id="corrScatter_result_plot",
                                                               fluidRow(
                                                                 column(width = 8,
                                                                        dropdownButton(
                                                                          inputId = "corrScatter_plot_introduction",
                                                                          label = "Plot Introduction",
                                                                          #icon = icon("question"),
                                                                          status = "primary",
                                                                          circle = FALSE,
                                                                          corrScatter_plot_text)),
                                                                 column(width = 4,
                                                                        dropdownButton(
                                                                          inputId = "corrScatter_plot_download",
                                                                          label = "Download",
                                                                          status = "primary",
                                                                          icon=icon("download"),
                                                                          circle = FALSE,
                                                                          width="50px",
                                                                          downloadButton(outputId = "corrScatter_plot_download_pdf",
                                                                                         label = "PDF"),
                                                                          downloadButton(outputId = "corrScatter_plot_download_svg",
                                                                                         label = "SVG"),
                                                                          downloadButton(outputId = "corrScatter_plot_download_png",
                                                                                         label = "PNG")))
                                                                 
                                                               ),
                                                               plotOutput("corrScatter_result_plot_panel")%>% withSpinner(color="#6c6689"))
                                                  )
                           ))
                       
                       
                       
                       
                       
                )
                
              ),
              column(12,class = "footer-container",
                     HTML('<div style="text-align: center;"><p>Copyright © 2023. All rights reserved.</p></div> <div style="text-align: center;">
                     <div style="display:inline-block;width:400px;"></div> </div>'))
              
              
      ),

      
      
      ##=================================6.Multigenes=============================
      tabItem(tabName = "Multigenes",
              
              fluidRow( #Row 2 : introduction and option
                #align="center",
                column(width=12,style = "padding-right:40px;padding-left:40px",
                       
                       column(width=12,align="center",style="padding-top:10px",
                              actionBttn(
                                inputId = "multi_cor_title",
                                label = h1(class = "pageTitle","Correlation Analysis Among Genes",
                                           icon(name = "search",lib="glyphicon")), 
                                style = "minimal",
                                color = "danger",
                                size = "lg")),
                       bsModal(id="modal_multi_cor_title", title="Introduction", trigger="multi_cor_title", 
                               size = "large",corr_multi_text),
                       
                       radioGroupButtons(
                         inputId ="multi_cor_type",
                         label = NULL,
                         choices = c("corrHeatmap",
                                     "corrBubble"),
                         justified = TRUE,
                         status = "warning"
                       ),
                       
                       
                       conditionalPanel(
                         condition="input.multi_cor_type=='corrHeatmap'",
                         shinydashboard::box(width=NULL,
                                             column(width = 5,offset=1,
                                                    #h4(class = "boxTitle","Parameters"),
                                                    selectizeInput(
                                                      inputId = "multi_cor_heat_dataset",
                                                      label =p("Search, select a dataset:",style="margin: 0 0 5px;", 
                                                               icon(name = "question-sign",lib="glyphicon",id="multi_cor_heat_dataset_q")), 
                                                      #choices = GSEdatalist$name,
                                                      choices= setNames(GSEdatalist$name,GSEdatalist$samplenum),
                                                      selected="HNSCC_GSE72536"
                                                    ),
                                                    
                                                    bsModal(id="modal_multi_cor_heat_dataset_q", title="Dataset Selection", trigger="multi_cor_heat_dataset_q", 
                                                            size = "large",
                                                            p(class = "submitboxContext","The dataset name is a combination of the corresponding tumor name/cell line and GSE ID."),
                                                            p(class = "submitboxContext","For example: "),
                                                            p(class = "submitboxContext","OPSCC_GSE55544 indicates that GSE55544 is a dataset of oropharyngeal squamous cell carcinoma."),
                                                            DT::datatable(abbreviation,width = "500px",height = "300px",
                                                                          caption="Table: Abbreviation for cancer/cell line.",
                                                                          rownames = FALSE,
                                                                          options = list(
                                                                            dom = 'tpr',
                                                                            pageLength = 5,
                                                                            initComplete = JS(
                                                                              "function(settings, json) {",
                                                                              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                              "}")
                                                                            
                                                                          )
                                                            )),
                                                    
                                                    prettyRadioButtons(inputId ="multi_cor_heat_status",
                                                                       label =p("Select the sample range for analysis：",style="margin: 0 0 5px;"),  
                                                                       choices = c("HPV_Positive","HPV_Negative","All"),
                                                                       inline = TRUE            
                                                    ),
                                                    
                                                    prettyRadioButtons(
                                                      inputId = "multi_cor_heat_method",
                                                      label = p("Select a method for correlation",style="margin: 0 0 5px;"), 
                                                      choices = c("pearson", "spearman"),
                                                      selected = "pearson",
                                                      inline = TRUE
                                                    )
                                             ),
                                             column(width =6,
                                                    column(width = 10,
                                                           br(),
                                                           selectizeInput(
                                                             inputId = "multi_cor_heat_genes",
                                                             label = p("Select, search 5~15 gene symbol:",style="margin: 0 0 5px;"), 
                                                             choices = NULL,
                                                             multiple = TRUE, 
                                                             options = list(placeholder = "Select 5 genes at least!" ,
                                                                            maxItems = 15)
                                                           ),
                                                           shinydashboard::box(title =h4(class = "boxTitle","Color Option"),
                                                                               width=NULL,status="info",collapsible = T,collapsed=T, 
                                                                               pickerInput(
                                                                                 inputId = "multi_cor_heatmap_color",
                                                                                 label = "Select colorscale:", 
                                                                                 choices=c("Red-Blue","Purple-Green",
                                                                                           "Pink-Green","Brown-BlueyGreen","Red-Yellow-Blue")
                                                                               )                  
                                                           )
                                                    ),
                                                    
                                                    column(width = 3,
                                                           actionButton('multi_cor_heat_update', 
                                                                        "Visualize",icon = icon('palette'))
                                                    ),
                                                    
                                                    
                                                    
                                             )
                         ),
                         div(id="multi_cor_heat_result_smu",
                             shinydashboard::tabBox(id="multi_cor_heat_result_panel", title="Result",width = NULL,
                                                    tabPanel("Plot",
                                                             uiOutput("multi_cor_heat_result_plot_panel")
                                                    )))
                         
                         
                       ),
                       
                       conditionalPanel(
                         condition="input.multi_cor_type=='corrBubble'",
                         shinydashboard::box(width=NULL,
                                             column(width = 5,offset=1,
                                                    #h4(class = "boxTitle","Parameters"),
                                                    selectizeInput(
                                                      inputId = "multi_cor_bub_dataset",
                                                      label =p("Search, select a dataset:",style="margin: 0 0 5px;", 
                                                               icon(name = "question-sign",lib="glyphicon",id="multi_cor_bub_dataset_q")), 
                                                      #choices = GSEdatalist$name,
                                                      choices= setNames(GSEdatalist$name,GSEdatalist$samplenum),
                                                      selected="HNSCC_GSE72536"
                                                    ),
                                                    
                                                    bsModal(id="modal_multi_cor_bub_dataset_q", title="Dataset Selection", trigger="multi_cor_bub_dataset_q", 
                                                            size = "large",
                                                            p(class = "submitboxContext","The dataset name is a combination of the corresponding tumor name/cell line and GSE ID."),
                                                            p(class = "submitboxContext","For example: "),
                                                            p(class = "submitboxContext","OPSCC_GSE55544 indicates that GSE55544 is a dataset of oropharyngeal squamous cell carcinoma."),
                                                            DT::datatable(abbreviation,width = "500px",height = "300px",
                                                                          caption="Table: Abbreviation for cancer/cell line.",
                                                                          rownames = FALSE,
                                                                          options = list(
                                                                            dom = 'tpr',
                                                                            pageLength = 5,
                                                                            initComplete = JS(
                                                                              "function(settings, json) {",
                                                                              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                              "}")
                                                                            
                                                                          )
                                                            )),
                                                    
                                                    prettyRadioButtons(inputId ="multi_cor_bub_status",
                                                                       label =p("Select the sample range for analysis：",style="margin: 0 0 5px;"),  
                                                                       choices = c("HPV_Positive","HPV_Negative","All"),
                                                                       inline = TRUE            
                                                    ),
                                                    
                                                    prettyRadioButtons(
                                                      inputId = "multi_cor_bub_method",
                                                      label = p("Select a method for correlation",style="margin: 0 0 5px;"), 
                                                      choices = c("pearson", "spearman"),
                                                      selected = "pearson",
                                                      inline = TRUE
                                                    )
                                             ),
                                             column(width =6,
                                                    column(width = 10,
                                                           br(),
                                                           
                                                           selectizeInput(
                                                             inputId = "multi_cor_bub_gene1",
                                                             label = p("Select, search 5~15 gene symbols for X-axis:",style="margin: 0 0 5px;"), 
                                                             choices = NULL,
                                                             multiple = TRUE, 
                                                             options = list(placeholder = "Select 5 genes at least!" ,
                                                                            maxItems = 15)
                                                           ),
                                                           selectizeInput(
                                                             inputId = "multi_cor_bub_gene2",
                                                             label = p("Select, search 5~15 gene symbols for Y-axis:",style="margin: 0 0 5px;"), 
                                                             choices = NULL,
                                                             multiple = TRUE, 
                                                             options = list(placeholder = "Select 5 genes at least!" ,
                                                                            maxItems = 15)
                                                           )
                                                    ),
                                                    
                                                    column(width = 3,
                                                           actionButton('multi_cor_bub_update', 
                                                                        "Visualize",icon = icon('palette'))
                                                    ),
                                                    
                                                    
                                                    
                                             )
                         ),
                         div(id="multi_cor_bub_result_smu",
                             shinydashboard::tabBox(id="multi_cor_bub_result_panel", title="Result",width = NULL,
                                                    tabPanel("Plot",
                                                             uiOutput("multi_cor_bub_result_plot_panel")
                                                    )))
                       )
                       
                )
                
              ),
              column(12,class = "footer-container",
                     HTML('<div style="text-align: center;"><p>Copyright © 2023. All rights reserved.</p></div> <div style="text-align: center;">
                     <div style="display:inline-block;width:400px;"></div> </div>'))
              
              
      ),
      
      
      ##=================================4.Coexpression=============================
      tabItem(tabName = "Coexpression",
              
              fluidRow( 
                column(width=12,align="center",style="padding-top:10px",
                       actionBttn(
                         inputId = "coexp_title",
                         label = h1(class = "pageTitle","Co-Expression",
                                    icon(name = "search",lib="glyphicon")), 
                         style = "minimal",
                         color = "danger",
                         size = "lg")),
                bsModal(id="modal_coexp_title", title="Introduction", trigger="coexp_title", 
                        size = "large",coexp_text),
                
                column(width=12,style = "padding-right:40px;padding-left:40px",
                       
                       shinydashboard::box(width=NULL,
                                           column(width = 5,offset=1,
                                                  #h4(class = "boxTitle","Parameters"),
                                                  
                                                  radioButtons( inputId = "coexp_selective_species", 
                                                                label =  p("Select the species:",style="margin: 0 0 5px;"),
                                                                choices = c("Homo sapiens","Mus musculus"),
                                                                inline = TRUE,selected = "Homo sapiens"),
                                                  
                                                  selectizeInput(
                                                    inputId = "coexp_gene_single",
                                                    label =p("Search, select a key gene symbol:", style="margin: 0 0 5px;",
                                                             icon(name = "question-sign",lib="glyphicon",id="coexp_gene_single_q"),
                                                             bsTooltip("coexp_gene_single_q", 
                                                                       paste0("<p>Explore the correlation between this master gene and other genes.</p>"),
                                                                       "right", trigger="hover",options = list(container = "body")) ),  
                                                    choices = NULL
                                                  ),
                                                  
                                                  radioButtons( inputId = "coexp_status_single", 
                                                                label =  p("Select the sample range for analysis：",style="margin: 0 0 5px;"),
                                                                choices = c("HPV_Positive","HPV_Negative","All"),
                                                                inline = TRUE)
                                           ),
                                           
                                           column(width = 5,
                                                  br(),
                                                  radioButtons( inputId = "coexp_method_single", 
                                                                label =  p("Select a method for correlation:",style="margin: 0 0 5px;"),
                                                                choices = c("Pearson","Spearman"),
                                                                inline = TRUE),
                                                  selectizeInput(
                                                    inputId = "coexp_specific_genes",
                                                    label = p("Correlation with the key gene：",style="margin: 0 0 5px;",
                                                              icon(name = "question-sign",lib="glyphicon",id="coexp_selective_button_q"),
                                                              bsTooltip("coexp_selective_button_q", 
                                                                        paste0("<p>Manually select your genes of interest.</p>"),
                                                                        "right", trigger="hover",options = list(container = "body"))), 
                                                    choices = NULL, multiple = TRUE,
                                                    options = list(placeholder = "Select one gene at least!" ,
                                                                   maxItems = 20)),
                                                  
                                                  shinydashboard::box(title =h4(class = "boxTitle","Color Option"),
                                                                      width=NULL,status="info",collapsible = T,collapsed=T, 
                                                                      pickerInput(
                                                                        inputId = "coexp_heatmap_color",
                                                                        label = "Select colorscale:", 
                                                                        choices=c("light Red-Blue","Red-Blue","Purple-Green",
                                                                                  "Pink-Green","Brown-BlueyGreen","Red-Yellow-Blue","Green-Yellow"),
                                                                        selected = "light Red-Blue"
                                                                      )              
                                                  ),
                                                  
                                                  actionButton('coexp_update', 
                                                               "Visualize",icon = icon('palette')),
                                                  
                                           )
                       ),
                       div(id="coexp_result_smu",
                           shinydashboard::tabBox(id="coexp_result_panel", title="Result",width = NULL,
                                                  tabPanel("Plot",
                                                           div(
                                                             fluidRow(
                                                               column(width = 8,
                                                                      dropdownButton(
                                                                        inputId = "coexp_plot_introduction",
                                                                        label = "Plot Introduction",
                                                                        #icon = icon("question"),
                                                                        status = "primary",
                                                                        circle = FALSE,
                                                                        coexp_heatmap_text)),
                                                               column(width = 4,
                                                                      dropdownButton(
                                                                        inputId = "coexp_plot_download",
                                                                        label = "Download",
                                                                        status = "primary",
                                                                        icon=icon("download"),
                                                                        circle = FALSE,
                                                                        width="50px",
                                                                        downloadButton(outputId = "coexp_plot_download_pdf",
                                                                                       label = "PDF"),
                                                                        downloadButton(outputId = "coexp_plot_download_svg",
                                                                                       label = "SVG"),
                                                                        downloadButton(outputId = "coexp_plot_download_png",
                                                                                       label = "PNG")))
                                                               
                                                             ),
                                                             plotOutput("coexp_result_plot_panel")%>% withSpinner(color="#6c6689"))
                                                  ),
                                                  tabPanel("Data",
                                                           dropdownButton(
                                                             inputId = "coexpression_datatable_download",
                                                             label = "Download Data",
                                                             status = "primary",
                                                             icon=icon("download"),
                                                             circle = FALSE,
                                                             width="50px",
                                                             downloadButton(outputId = "coexpression_datatable_download_csv",
                                                                            label = "CSV"),
                                                             downloadButton(outputId = "coexpression_datatable_download_txt",
                                                                            label = "TXT")),
                                                           uiOutput("coexpression_datatable"))
                           ))
                       
                       
                )
                
              ),
              column(12,class = "footer-container",
                     HTML('<div style="text-align: center;"><p>Copyright © 2023. All rights reserved.</p></div> <div style="text-align: center;">
                     <div style="display:inline-block;width:400px;"></div> </div>'))
              
              
      ),
      
      ##=================================5.corMetaAnalysis=============================
      tabItem(tabName = "corMetaAnalysis",
              
              fluidRow( 
                column(width=12,align="center",style="padding-top:10px",
                       actionBttn(
                         inputId = "corrMeta_title",
                         label = h1(class = "pageTitle","Meta analysis for correlation",
                                    icon(name = "search",lib="glyphicon")), 
                         style = "minimal",
                         color = "danger",
                         size = "lg")),
                bsModal(id="modal_corrMeta_title", title="Introduction", trigger="corrMeta_title", 
                        size = "large",corrMeta_text),
                
                column(width=12,style = "padding-right:40px;padding-left:40px",
                       
                       shinydashboard::box(width=NULL,
                                           
                                           column(width = 12,
                                                  column(width = 5,offset = 1,
                                                         radioButtons( inputId = "corrMeta_selective_species", 
                                                                       label =  p("Select the species:",style="margin: 0 0 5px;"),
                                                                       choices = c("Homo sapiens","Mus musculus"),
                                                                       inline = TRUE,selected = "Homo sapiens"),
                                                         radioButtons( inputId = "corrMeta_method_single", 
                                                                       label =  p("Select a method for correlation:",style="margin: 0 0 5px;"),
                                                                       choices = c("Pearson","Spearman"),
                                                                       inline = TRUE),
                                                         radioButtons( inputId = "corrMeta_status_single", 
                                                                       label =  p("Select the sample range for analysis：",style="margin: 0 0 5px;"),
                                                                       choices = c("HPV_Positive","HPV_Negative","All"),
                                                                       inline = TRUE),
                                                         
                                                         actionButton('corrMeta_update', 
                                                                      "Visualize",icon = icon('palette'))
                                                         
                                                  ),
                                                  column(width = 5,
                                                         radioButtons( inputId = "corrMeta_xaxis_type", 
                                                                       label =  p("Select, search an item (X axis):", style="margin: 0 0 5px;",
                                                                                  icon(name = "question-sign",lib="glyphicon",id="corrMeta_xaxis_type_q"),
                                                                                  bsTooltip("corrMeta_xaxis_type_q", 
                                                                                            paste0("<p>You can select gene expression, pathway ssGSEA scores, and immune cell infiltration scores for correlation analysis.</p>"),
                                                                                            "right", trigger="hover",options = list(container = "body")) ),
                                                                       choices = c("Gene","Pathway","Immune Cell"),
                                                                       inline = TRUE),
                                                         
                                                         selectizeInput(
                                                           inputId = "corrMeta_xaxis_item",multiple = FALSE,
                                                           label =NULL,choices = NULL),
                                                         
                                                         radioButtons( inputId = "corrMeta_yaxis_type", 
                                                                       label =  p("Select, search an item (Y axis):", style="margin: 0 0 5px;",
                                                                                  icon(name = "question-sign",lib="glyphicon",id="corrMeta_yaxis_type_q"),
                                                                                  bsTooltip("corrMeta_yaxis_type_q", 
                                                                                            paste0("<p>You can select gene expression, pathway ssGSEA scores, and immune cell infiltration scores for correlation analysis.</p>"),
                                                                                            "right", trigger="hover",options = list(container = "body")) ),
                                                                       choices = c("Gene","Pathway","Immune Cell"),
                                                                       inline = TRUE),
                                                         
                                                         selectizeInput(
                                                           inputId = "corrMeta_yaxis_item",multiple = FALSE,
                                                           label =NULL, choices = NULL)
                                                  )
                                                  
                                           ),
                                           column(width = 12,
                                                  
                                                  shinydashboard::box(title =h4(class = "boxTitle","Customize"),
                                                                      width=NULL,status="danger",collapsible = T,collapsed=T,
                                                                      
                                                                      column(width = 5,offset = 1,style="padding-right:15px;padding-left:15px",
                                                                             selectInput(
                                                                               inputId = "corrMeta_forest_model",
                                                                               label = p("Meta-analysis model:", 
                                                                                         style="margin: 0 0 5px;",
                                                                                         icon(name = "question-sign",lib="glyphicon",id="corrMeta_forest_model_q"),
                                                                                         bsTooltip("corrMeta_forest_model_q", 
                                                                                                   "Choose fixed effect or random effect models.",
                                                                                                   "right", trigger="hover",options = list(container = "body"))),
                                                                               choices =c("Fixed effects model","Random effects model","Both"),
                                                                               selected = "Random effects model")
                                                                      ),
                                                                      
                                                                      column(width = 5,
                                                                             radioButtons(inputId = "corrMeta_forest_sm",
                                                                                          label = p("Summary Measure:", 
                                                                                                    style="margin: 0 0 5px;",
                                                                                                    icon(name = "question-sign",lib="glyphicon",id="corrMeta_forest_sm_q")),
                                                                                          choices =c("COR","ZCOR"),selected = "ZCOR",
                                                                                          inline = TRUE),
                                                                             
                                                                             bsModal(id="modal_corrMeta_forest_sm_q", title="Summary Measure", 
                                                                                     trigger="corrMeta_forest_sm_q", 
                                                                                     size = "small",
                                                                                     p(class = "submitboxContext",
                                                                                       "A character string indicating which summary measure is to be used for pooling of studies.",br(),
                                                                                       "COR: Untransformed correlations.",br(),"ZCOR: Fisher's z transformed correlations.")
                                                                             ),
                                                                             checkboxInput(
                                                                               inputId = "corrMeta_forest_subgroup",
                                                                               label = p("Subgroup by Diease.",
                                                                                         style="margin: 0 0 5px;",
                                                                                         icon(name = "question-sign",lib="glyphicon",id="corrMeta_forest_subgroup_q"),
                                                                                         bsTooltip("corrMeta_forest_subgroup_q", 
                                                                                                   "Whether to conduct a meta-analysis of disease subgroups.",
                                                                                                   "right", trigger="hover",options = list(container = "body"))),
                                                                               value = TRUE)
                                                                      )
                                                                      
                                                  )
                                                  
                                           )
                                           
                                           
                                           
                                           
                       ),
                       div(id="corrMeta_result_smu",
                           shinydashboard::tabBox(id="corrMeta_result_panel", title="Result",width = NULL,
                                                  tabPanel("Plot",
                                                           div(id="corrMeta_result_plot_pos",
                                                               fluidRow(
                                                                 column(width = 8,
                                                                        dropdownButton(
                                                                          inputId = "corrMeta_plot_introduction_pos",
                                                                          label = "Plot Introduction",
                                                                          #icon = icon("question"),
                                                                          status = "primary",
                                                                          circle = FALSE,
                                                                          forest_plot_text)),
                                                                 column(width = 4,
                                                                        dropdownButton(
                                                                          inputId = "corrMeta_plot_download_pos",
                                                                          label = "Download",
                                                                          status = "primary",
                                                                          icon=icon("download"),
                                                                          circle = FALSE,
                                                                          width="50px",
                                                                          downloadButton(outputId = "corrMeta_plot_download_pdf_pos",
                                                                                         label = "PDF"),
                                                                          downloadButton(outputId = "corrMeta_plot_download_svg_pos",
                                                                                         label = "SVG"),
                                                                          downloadButton(outputId = "corrMeta_plot_download_png_pos",
                                                                                         label = "PNG")))
                                                                 
                                                               ),
                                                               uiOutput("corrMeta_result_posui")),
                                                           br(),
                                                           div(id="corrMeta_result_plot_neg",
                                                               fluidRow(
                                                                 column(width = 8,
                                                                        dropdownButton(
                                                                          inputId = "corrMeta_plot_introduction_neg",
                                                                          label = "Plot Introduction",
                                                                          #icon = icon("question"),
                                                                          status = "primary",
                                                                          circle = FALSE,
                                                                          forest_plot_text)),
                                                                 column(width = 4,
                                                                        dropdownButton(
                                                                          inputId = "corrMeta_plot_download_neg",
                                                                          label = "Download",
                                                                          status = "primary",
                                                                          icon=icon("download"),
                                                                          circle = FALSE,
                                                                          width="50px",
                                                                          downloadButton(outputId = "corrMeta_plot_download_pdf_neg",
                                                                                         label = "PDF"),
                                                                          downloadButton(outputId = "corrMeta_plot_download_svg_neg",
                                                                                         label = "SVG"),
                                                                          downloadButton(outputId = "corrMeta_plot_download_png_neg",
                                                                                         label = "PNG")))
                                                                 
                                                               ),
                                                               uiOutput("corrMeta_result_negui"))
                                                  ),
                                                  tabPanel("Data",
                                                           uiOutput("corrMeta_datatable"))
                           ))
                       
                       
                       
                       
                       
                )
                
              ),
              column(12,class = "footer-container",
                     HTML('<div style="text-align: center;"><p>Copyright © 2023. All rights reserved.</p></div> <div style="text-align: center;">
                     <div style="display:inline-block;width:400px;"></div> </div>'))
              
              
      ),
      
      
      ##====================Data
      tabItem(tabName = "data_for_immunecell_related",
              fluidRow( 
                column(width=12,style = "padding-right:40px;padding-left:40px",
                       column(width=12,align="center",style="padding-top:10px",
                              actionBttn(
                                inputId = "data_immunecell_title",
                                label = h1(class = "pageTitle","Immune cells related genes",
                                           icon(name = "search",lib="glyphicon")), 
                                style = "minimal",
                                color = "danger",
                                size = "lg")),
                       bsModal(id="modal_data_immunecell_title", title="Introduction", trigger="data_immunecell_title", 
                               size = "large",data_immunecell_relatedgene_text),
                       
                       column(width=12,align="center",style="padding-top:10px",
                              dataTableOutput("data_immunecell_genes_df",width = "900px")),
                       column(width=12,style="padding-top:10px",
                              h5('Tip: We provided two macrophage related gene sets from different sources. 
                              "Macrophage_ Rooney" indicated that the gene set is from Rooney.
                              "Macrophage_ Charoentong" indicated that the gene set is from Charoentong.'))
                       
                ),
                
                column(width = 3,
                       downloadButton(outputId = "data_immunecell_genes_download_csv",
                                      style="margin-top: 10px",
                                      label = "Download full data(CSV)"))
              ),
              column(12,class = "footer-container",
                     HTML('<div style="text-align: center;"><p>Copyright © 2023. All rights reserved.</p></div> <div style="text-align: center;">
                     <div style="display:inline-block;width:400px;"></div> </div>'))
              
              
      ),
      
      tabItem(tabName ="data_for_datastes",
              fluidRow( 
                column(width=12,style = "padding-right:40px;padding-left:40px",
                       column(width=12,align="center",style="padding-top:10px",
                              actionBttn(
                                inputId = "data_datasets_title",
                                label = h1(class = "pageTitle","Dataset Browser",
                                           icon(name = "search",lib="glyphicon")), 
                                style = "minimal",
                                color = "danger",
                                size = "lg"))
                ),
                bsModal(id="modal_data_datasets_title", title="Introduction", trigger="data_datasets_title", 
                        size = "large",datasetbrowser_text),
                
                column(width=10,offset=1,
                       #style="padding-top:10px",
                       column(width=3,
                              pickerInput(inputId = "data_species",
                                          label = p("Species:",style="margin: 0 0 5px;"),
                                          choices =c("Homo sapiens","Mus musculus"),multiple=T,selected = NULL,
                                          options = list(`none-selected-text` = "Please make your choice"))
                       ),
                       column(width = 3,
                              pickerInput(inputId = "data_classification",
                                          label = p("Classification:",style="margin: 0 0 5px;"),
                                          choices =c("Tissue","Cell line","Peripheral blood"),multiple=T,selected = NULL,
                                          options = list(`none-selected-text` = "Please make your choice"))),
                       column(width = 3,
                              pickerInput(inputId = "data_cance_type",
                                          label = p("Cancer Type:",style="margin: 0 0 5px;"),
                                          choices =c("CC","HNSCC","OPSCC","cSCC","Rb","SCLC","VSCC","DPA"),
                                          multiple=T,selected = NULL,
                                          options = list(`none-selected-text` = "Please make your choice"),
                                          choicesOpt = list(subtext =c("Cervical Cancer","Head and Neck Squamous Cell Carcinoma",
                                                                       "Oropharyngeal Squamous Cell Carcinoma","Cutaneous Squamous Cell Carcinoma",
                                                                       "RetinoBlastoma","Small Cell Lung Cancer",
                                                                       "Vulvar squamous cell carcinoma","Digital Papillary Adenocarcinoma")))),
                       column(width = 3,
                              downloadButton(outputId = "data_datasets_browser_csv",
                                             label = "Download full data(CSV)"))
                       
                ),
                
                column(width=12,align="center",style = "padding-top:10px;",
                       DT::dataTableOutput("data_datasets_wholedataframe",width = "900px")
                ),
                
                column(width=12,align="center",style = "padding-top:10px;",
                       DT::dataTableOutput("data_datasets_detailsdataframe",width = "900px"))
                
              ),
              column(12,class = "footer-container",
                     HTML('<div style="text-align: center;"><p>Copyright © 2023. All rights reserved.</p></div> <div style="text-align: center;">
                     <div style="display:inline-block;width:400px;"></div> </div>'))
              
              
      ),
      
      tabItem(tabName ="data_for_clinical",
              fluidRow( 
                column(width=12,style = "padding-right:40px;padding-left:40px",
                       column(width=12,align="center",style="padding-top:10px",
                              actionBttn(
                                inputId = "data_clinical_title",
                                label = h1(class = "pageTitle","Clincal Data",
                                           icon(name = "search",lib="glyphicon")), 
                                style = "minimal",
                                color = "danger",
                                size = "lg"))
                ),
                bsModal(id="modal_data_clinical_title", title="Introduction", trigger="data_clinical_title", 
                        size = "large",data_clinical_text),
                
                column(width=10,offset=1,
                       #style="padding-top:10px",
                       column(width=5,
                              selectizeInput(
                                inputId = "data_clinical_dataset",
                                label = p("Search, select a dataset:", style="margin: 0 0 5px;",
                                          icon(name = "question-sign",lib="glyphicon",id="data_clinical_dataset_q")),
                                #choices = GSEdatalist$name[which(GSEdatalist$Clinical==TRUE)],
                                choices= setNames(GSEdatalist$name[which(GSEdatalist$Clinical==TRUE)],
                                                  GSEdatalist$samplenum[which(GSEdatalist$Clinical==TRUE)]),
                                selected="HNSCC_GSE65858"
                              ),
                              bsModal(id="modal_data_clinical_dataset_q", title="Dataset Selection", trigger="data_clinical_dataset_q", 
                                      size = "large",
                                      p(class = "submitboxContext","The dataset name is a combination of the corresponding tumor name/cell line and GSE ID."),
                                      p(class = "submitboxContext","For example: "),
                                      p(class = "submitboxContext","OPSCC_GSE55544 indicates that GSE55544 is a dataset of oropharyngeal squamous cell carcinoma."),
                                      DT::datatable(abbreviation,width = "500px",height = "300px",
                                                    caption="Table: Abbreviation for cancer/cell line.",
                                                    rownames = FALSE,
                                                    options = list(
                                                      dom = 'tpr',
                                                      pageLength = 5,
                                                      initComplete = JS(
                                                        "function(settings, json) {",
                                                        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                        "}")
                                                      
                                                    )
                                      ))
                       ),
                       column(width = 3,style="margin-top:25px",
                              downloadButton(outputId = "data_clinical_csv",
                                             label = "Download full data(CSV)"))
                       
                ),
                
                column(width=12,align="center",style = "padding-top:10px;",
                       DT::dataTableOutput("data_clinical_show_panel",width = "900px")
                )
                
              ),
              column(12,class = "footer-container",
                     HTML('<div style="text-align: center;"><p>Copyright © 2023. All rights reserved.</p></div> <div style="text-align: center;">
                     <div style="display:inline-block;width:400px;"></div> </div>'))
              
              
      ),
      
      ##===========FQA
      tabItem(tabName = "question_panel",
              fluidRow(
                column(width=12,align="center",style="padding-top:10px",
                       actionBttn(
                         inputId = "question_panel_title",
                         label = h1(class = "pageTitle","Frequently Asked Question (FAQ) Answering"), 
                         style = "minimal",
                         color = "danger",
                         size = "lg")),
                bsModal(id="modal_question_panel_title", title="Introduction", trigger="question_panel_title", 
                        size = "large",question_panel_title_text),
                column(width=12,style = "padding-right:25px;padding-left:25px",
                       bsCollapse(id = "document_for_question", #open = "1.What is HPV_shiny?",
                                  bsCollapsePanel("1. What is HPVTIMER?", 
                                                  answer1 , style = "primary"),
                                  bsCollapsePanel("2. What are the sources of the datasets and the inclusion criteria?", 
                                                  answer2, style = "primary"),
                                  bsCollapsePanel("3. How was the raw data processed?", 
                                                  answer3, style = "primary"),
                                  bsCollapsePanel("4. What R packages are used in HPVTIMER?", 
                                                  answer4,
                                                  div(style='width:600px;overflow-y:hidden; overflow-x: scroll',
                                                      DT::datatable(answer4_packages_list ,width = "500px",height = "300px",
                                                                    #style = 'bootstrap', class = 'table-bordered',
                                                                    rownames = FALSE,
                                                                    extensions = c('Scroller'),
                                                                    options = list(
                                                                      dom='trp',
                                                                      pageLength = 10,
                                                                      scrollY = "300px")
                                                      )),
                                                  style = "primary"),
                                  bsCollapsePanel("5. Which datasets are available for each analysis module?", 
                                                  answer5, style = "primary"),
                                  bsCollapsePanel("6. Abbreviation", 
                                                  div(
                                                    DT::datatable(abbreviation,width = "500px",height = "300px",
                                                                  caption="Table: Abbreviation for cancer/cell line.",
                                                                  rownames = FALSE,
                                                                  options = list(
                                                                    dom = 'tpr',
                                                                    pageLength = 5,
                                                                    initComplete = JS(
                                                                      "function(settings, json) {",
                                                                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                      "}")
                                                                    
                                                                  )
                                                    ))
                                                  , style = "primary"),
                                  bsCollapsePanel('7. Why are there different significance results displayed in the "Data" tab of the Differential expression analysis module? Why do heatmaps and box plots use the significance results of the Wilcoxon Test?', 
                                                  answer7, style = "primary"),
                                  bsCollapsePanel('8. Why are some significantly different genes marked as "ns" in the box plots/heatmaps of the Differential expression analysis module?', 
                                                  answer8, style = "primary")
                       )
                )
              ),
              column(12,class = "footer-container",
                     HTML('<div style="text-align: center;"><p>Copyright © 2023. All rights reserved.</p></div> <div style="text-align: center;">
                     <div style="display:inline-block;width:400px;"></div> </div>'))
              
              
      ),
      
      ##===========contact
      tabItem(tabName = "contact_panel",
              fluidRow(
                column(width=12,align="center",style="padding-top:10px",
                       actionBttn(
                         inputId = "contact_panel_title",
                         label = h1(class = "pageTitle","Contact"), 
                         style = "minimal",
                         color = "danger",
                         size = "lg")),
                column(width=12,style = "padding-right:25px;padding-left:25px",
                       shinydashboard::box(width=NULL,
                                           p("Should you have any questions, please feel free to contact us."),
                                           p("Peng Luo:",tags$a("luopeng@smu.edu.cn",href="mailto:luopeng@smu.edu.cn")),
                                           p("Liying Liu:",tags$a("smuliuliying@i.smu.edu.cn",href="mailto:smuliuliying@i.smu.edu.cn")),
                                           p("Yanan Xie:",tags$a("xieyanan@i.smu.edu.cn",href="mailto:xieyanan@i.smu.edu.cn")),
                                           p("Hong Yang:",tags$a("smuyanghong@i.smu.edu.cn",href="mailto:smuyanghong@i.smu.edu.cn")),
                                           br(),
                                           p("Our lab has a long-standing interest in cancer biomedical research and bioinformatics. We recently developed several other Shiny web tools focusing on solving various scientific questions."),
                                           p(tags$a("CAMOIP",href="http://www.camoip.net",target="_blank"),
                                             ":A Web Server for Comprehensive Analysis on Multi-omics of Immunotherapy in Pan-cancer. doi:",
                                             tags$a("10.1093/bib/bbac129",href="https://doi.org/10.1093/bib/bbac129",target="_blank")
                                           ),
                                           p(tags$a("Onlinemeta",href="https://smuonco.Shinyapps.io/Onlinemeta/",target="_blank"),
                                             ": A Web Server For Meta-Analysis Based On R-shiny.  doi:",
                                             tags$a("10.1101/2022.04.13.488126",href="https://doi.org/10.1101/2022.04.13.488126",target="_blank")
                                           ),
                                           
                                           p(tags$a("PanCanSurvPlot",href="https://smuonco.shinyapps.io/PanCanSurvPlot/",target="_blank"),
                                             ":A Large-scale Pan-cancer Survival Analysis Web Application. doi:",
                                             tags$a("10.1101/2022.12.25.521884",href="https://doi.org/10.1101/2022.12.25.521884" ,target="_blank")
                                           )
                                           
                       )
                )
              ),
              fluidRow(
                column(width=12,align="center",style="padding-top:10px",
                       actionBttn(
                         inputId = "comment_panel_title",
                         label = h1(class = "pageTitle","Comment Box"), 
                         style = "minimal",
                         color = "danger",
                         size = "lg")),
                column(width=12,style = "padding-right:25px;padding-left:25px",
                       shinydashboard::box(width=NULL,
                                           textInput(inputId = "comment_contact",
                                                     label = "Name/Email (optional)", width = "30%"),
                                           textAreaInput(inputId = "comment_text", 
                                                         label = p("Comment",span("*", class = "mandatory_star")), 
                                                         placeholder = "Enter your comment here", 
                                                         width = "60%", height = "100px"),
                                           column(width=8,style = "padding-right:0px;padding-left:0px",
                                                  column(width=9,style = "padding-right:0px;padding-left:0px",
                                                         uiOutput("comment_file_input")),
                                                  column(width = 2,style = "padding-top:20px; padding-left:20px",
                                                         actionButton("comment_file_clear","Clear"))),
                                           column(width = 12,style = "padding-right:0px;padding-left:0px",
                                                  actionButton("comment_submit","Submit Comment",
                                                               icon = icon("envelope")))
                                           
                       )
                )
              ),
              column(12,class = "footer-container",
                     HTML('<div style="text-align: center;"><p>Copyright © 2023. All rights reserved.</p></div> <div style="text-align: center;">
                     <div style="display:inline-block;width:400px;"></div> </div>'))
              
      )
      
      
    )
  )
)
