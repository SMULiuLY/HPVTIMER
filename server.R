

function(input,output,session) {
  runjs('
        var el2 = document.querySelector(".skin-blue");
        el2.className = "skin-blue sidebar-mini";
        ')
  
  ##==============home
  output$home_slick_output <- renderSlickR({
    #imgs <- list.files("www/Data/show_merge_pic/", pattern=".png", full.names = TRUE)
    x <- slickR(slick_intro_plot,slideType = "img",
                slideId = 'slick_intro_plot_id',
                height = 600,
                width = '100%')  + 
      settings(dots = FALSE)
    #,autoplay = TRUE, autoplaySpeed = 3000
    
    #y <- slickR(slick_intro_text,slideType = 'p',
    #            slideId = 'slick_intro_text_id',
    #            height = 150,
    #            width = '80%')+ settings(arrows = FALSE)  
    
    #x %synch% y
    
  })
  
  ####==========================================DE Overall 1 Volcano
  
  #DEall_UI_Server("id_DE_All_Vol","Volcano")
  
  DE_overall_vol_nrDEG<-reactive({
    if(input$DE_overall_vol_dataset!=""){
      load(paste("www/Data/2.Different_expression_genes/",input$DE_overall_vol_dataset,".Rdata",sep=""))
      get("nrDEG") 
    }
  })
  
  
  observeEvent(input$DE_overall_vol_dataset,{
    if (input$DE_overall_vol_dataset=="") {
      showFeedbackDanger(
        inputId = "DE_overall_vol_dataset",
        text = "Please select a dataset!",
      )
    } else {
      hideFeedback("DE_overall_vol_dataset")
    }
    
  })
  
  DE_overall_volcano_genes<-SelectGene_Server("id_DE_All_Vol")
  
  DE_all_vol_update_requirement<-reactive({
    (input$DE_overall_vol_dataset!="" & !is.null(DE_overall_volcano_genes()))
  })
  
  observe({
    toggleState(id ="DE_all_vol_update",
                condition =DE_all_vol_update_requirement() )
  })
  
  
  DE_overall_volcano_result<-DEAll_Server("id_DE_All_Vol","Volcano",
                                          reactive(input$DE_overall_vol_dataset))  %>%
    bindEvent(input$DE_all_vol_update)
  
  shinyjs::hide(id ="DE_overall_vol_result_sum")
  observeEvent(input$DE_all_vol_update, {
    shinyjs::show(id ="DE_overall_vol_result_sum")
  })
  
  output$DE_overall_volcano_result_plot_show<-renderPlot({
    DE_overall_volcano_result()$plot
  })
  
  output$DE_overall_vol_result_data_panel<- renderUI({
    DT::datatable(DE_overall_volcano_result()$data,
                  caption =paste("Table: Differentially Expressed Genes for",
                                 input$DE_overall_vol_dataset,sep = " "),
                  #rownames = FALSE,
                  extensions=c('Responsive'),
                  options = list(
                    dom = 'ftipr',
                    pageLength = 10,
                    responsive = TRUE,
                    columnDefs = 
                      list(list(className = 'dt-center', 
                                targets = "_all")),
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                      "}")
                  ))
  })
  
  ##download
  output$DE_overall_volcano_download_svg<- downloadHandler(
    filename = function(){
      paste("DEanalysis_overall_Volcano_",input$DE_overall_vol_dataset,".svg",sep="")
    },
    content = function(file){
      svg(file)
      print(DE_overall_volcano_result()$plot)
      dev.off()
    }
  )
  
  output$DE_overall_volcano_download_pdf<- downloadHandler(
    filename = function(){
      paste("DEanalysis_overall_Volcano_",input$DE_overall_vol_dataset,".pdf",sep="")
    },
    content = function(file){
      pdf(file)
      print(DE_overall_volcano_result()$plot)
      dev.off()
    }
  )
  
  output$DE_overall_volcano_download_png<- downloadHandler(
    filename = function(){
      paste("DEanalysis_overall_Volcano_",input$DE_overall_vol_dataset,".png",sep="")
    },
    content = function(file){
      png(file)
      print(DE_overall_volcano_result()$plot)
      dev.off()
    }
  )
  
  output$DE_overall_vol_fulldata_download_csv<-downloadHandler(
    filename = function(){
      paste("DEG_",input$DE_overall_vol_dataset,".csv",sep = "")
    },
    content = function(file){
      sep<-","
      write.table(DE_overall_volcano_result()$data,file,sep=sep,row.names =TRUE)
    }
  )
  
  output$DE_overall_vol_fulldata_download_txt<-downloadHandler(
    filename = function(){
      paste("DEG_",input$DE_overall_vol_dataset,".txt",sep = "")
    },
    content = function(file){
      sep<-" "
      write.table(DE_overall_volcano_result()$data,file,sep=sep,row.names = TRUE)
    }
  )
  
  
  
  ####==========================================DE Overall 2 Heatmap
  
  #DEall_UI_Server("id_DE_All_Heat","Heatmap")
  
  DE_overall_heat_nrDEG<-reactive({
    if(input$DE_overall_heat_dataset!=""){
      load(paste("www/Data/2.Different_expression_genes/",input$DE_overall_heat_dataset,".Rdata",sep=""))
      get("nrDEG") 
    }
  })
  
  observeEvent(input$DE_overall_heat_dataset,{
    if (input$DE_overall_heat_dataset=="") {
      showFeedbackDanger(
        inputId = "DE_overall_heat_dataset",
        text = "Please select a dataset!",
      )
    } else {
      hideFeedback("DE_overall_heat_dataset")
    }
    
  })
  
  DE_overall_heatmap_genes<-SelectGene_Server("id_DE_All_Heat")
  
  DE_all_heat_update_requirement<-reactive({
    (input$DE_overall_heat_dataset!="" & !is.null(DE_overall_heatmap_genes()))
  })
  
  observe({
    toggleState(id ="DE_all_heat_update",
                condition =DE_all_heat_update_requirement() )
  })
  
  DE_overall_heatmap_result<-DEAll_Server("id_DE_All_Heat","Heatmap",
                                          reactive(input$DE_overall_heat_dataset)) %>%
    bindEvent(input$DE_all_heat_update)
  
  shinyjs::hide(id ="DE_overall_heat_result_sum")
  observeEvent(input$DE_all_heat_update, {
    shinyjs::show(id ="DE_overall_heat_result_sum")
  })
  
  
  output$DE_overall_heatmap_result_plot_show<-renderPlot({
    DE_overall_heatmap_result()$plot
  },height = 600)
  
  output$DE_overall_heat_result_data_panel<-renderUI({
    DT::datatable(DE_overall_heatmap_result()$data,
                  caption =paste("Table: Differentially Expressed Genes for",
                                 input$DE_overall_heat_dataset,sep = " "),
                  #rownames = FALSE,
                  extensions=c('Responsive'),
                  options = list(
                    dom = 'ftipr',
                    pageLength = 10,
                    responsive = TRUE,
                    columnDefs = 
                      list(list(className = 'dt-center', 
                                targets = "_all")),
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                      "}")
                  ))
    
  })
  
  ##download
  output$DE_overall_heatmap_download_svg<- downloadHandler(
    filename = function(){
      paste("DEanalysis_overall_Heatmap_",input$DE_overall_heat_dataset,".svg",sep="")
    },
    content = function(file){
      svg(file,height=10, width=15)
      print(DE_overall_heatmap_result()$plot)
      dev.off()
    }
  )
  
  output$DE_overall_heatmap_download_pdf<- downloadHandler(
    filename = function(){
      paste("DEanalysis_overall_Heatmap_",input$DE_overall_heat_dataset,".pdf",sep="")
    },
    content = function(file){
      pdf(file,height=10, width=15)
      print(DE_overall_heatmap_result()$plot)
      dev.off()
    }
  )
  
  output$DE_overall_heatmap_download_png<- downloadHandler(
    filename = function(){
      paste("DEanalysis_overall_Heatmap_",input$DE_overall_heat_dataset,".png",sep="")
    },
    content = function(file){
      png(file,width = 800, height = 600, units = "px")
      print(DE_overall_heatmap_result()$plot)
      dev.off()
    }
  )
  
  output$DE_overall_heat_fulldata_download_csv<-downloadHandler(
    filename = function(){
      paste("DEG_",input$DE_overall_heat_dataset,".csv",sep = "")
    },
    content = function(file){
      sep<-","
      write.table(DE_overall_heatmap_result()$data,file,sep=sep,row.names =TRUE)
    }
  )
  
  output$DE_overall_heat_fulldata_download_txt<-downloadHandler(
    filename = function(){
      paste("DEG_",input$DE_overall_heat_dataset,".txt",sep = "")
    },
    content = function(file){
      sep<-" "
      write.table(DE_overall_heatmap_result()$data,file,sep=sep,row.names = TRUE)
    }
  )
  
  
  ####==========================================DE Overall 3 Boxplot
  
  #DEall_UI_Server("id_DE_All_Box","Boxplot")
  
  DE_overall_box_nrDEG<-reactive({
    if(input$DE_overall_box_dataset!=""){
      load(paste("www/Data/2.Different_expression_genes/",input$DE_overall_box_dataset,".Rdata",sep=""))
      get("nrDEG") 
    }
  })
  
  observeEvent(input$DE_overall_box_dataset,{
    if (input$DE_overall_box_dataset=="") {
      showFeedbackDanger(
        inputId = "DE_overall_box_dataset",
        text = "Please select a dataset!",
      )
    } else {
      hideFeedback("DE_overall_box_dataset")
    }
    
  })
  
  DE_overall_boxplot_genes<-SelectGene_Server("id_DE_All_Box")
  
  DE_all_box_update_requirement<-reactive({
    (input$DE_overall_box_dataset!="" & !is.null(DE_overall_boxplot_genes()))
  })
  
  observe({
    toggleState(id ="DE_all_box_update",
                condition =DE_all_box_update_requirement() )
  })
  
  DE_overall_boxplot_result<-DEAll_Server("id_DE_All_Box","Boxplot",
                                          reactive(input$DE_overall_box_dataset)) %>%
    bindEvent(input$DE_all_box_update)
  
  shinyjs::hide(id ="DE_overall_box_result_sum")
  observeEvent(input$DE_all_box_update, {
    shinyjs::show(id ="DE_overall_box_result_sum")
  })
  
  output$DE_overall_boxplot_result_plot_show<-renderPlot({
    DE_overall_boxplot_result()$plot
  },height = 600)
  
  output$DE_overall_box_result_data_panel<-renderUI({
    DT::datatable(DE_overall_boxplot_result()$data,
                  caption =paste("Table: Differentially Expressed Genes for",
                                 input$DE_overall_box_dataset,sep = " "),
                  #rownames = FALSE,
                  extensions=c('Responsive'),
                  options = list(
                    dom = 'ftipr',
                    pageLength = 10,
                    responsive = TRUE,
                    columnDefs = 
                      list(list(className = 'dt-center', 
                                targets = "_all")),
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                      "}")
                  ))
    
  })
  
  ##download
  output$DE_overall_boxplot_download_svg<- downloadHandler(
    filename = function(){
      paste("DEanalysis_overall_Boxplot_",input$DE_overall_box_dataset,".svg",sep="")
    },
    content = function(file){
      svg(file,height=8, width=12)
      print(DE_overall_boxplot_result()$plot)
      dev.off()
    }
  )
  
  output$DE_overall_boxplot_download_pdf<- downloadHandler(
    filename = function(){
      paste("DEanalysis_overall_Boxplot_",input$DE_overall_box_dataset,".pdf",sep="")
    },
    content = function(file){
      pdf(file,height=8, width=12)
      print(DE_overall_boxplot_result()$plot)
      dev.off()
    }
  )
  
  output$DE_overall_boxplot_download_png<- downloadHandler(
    filename = function(){
      paste("DEanalysis_overall_Boxplot_",input$DE_overall_box_dataset,".png",sep="")
    },
    content = function(file){
      png(file,width = 850, height = 600, units = "px")
      print(DE_overall_boxplot_result()$plot)
      dev.off()
    }
  )
  
  output$DE_overall_box_fulldata_download_csv<-downloadHandler(
    filename = function(){
      paste("DEG_",input$DE_overall_box_dataset,".csv",sep = "")
    },
    content = function(file){
      sep<-","
      write.table(DE_overall_boxplot_result()$data,file,sep=sep,row.names =TRUE)
    }
  )
  
  output$DE_overall_box_fulldata_download_txt<-downloadHandler(
    filename = function(){
      paste("DEG_",input$DE_overall_box_dataset,".txt",sep = "")
    },
    content = function(file){
      sep<-" "
      write.table(DE_overall_boxplot_result()$data,file,sep=sep,row.names = TRUE)
    }
  )
  
  
  ####==========================================DE Immunecell 1 Volcano
  observeEvent(input$DE_immunecell_vol_dataset,{
    if (input$DE_immunecell_vol_dataset=="") {
      showFeedbackDanger(
        inputId = "DE_immunecell_vol_dataset",
        text = "Please select a dataset!",
      )
    } else {
      hideFeedback("DE_immunecell_vol_dataset")
    }
    
  })
  
  observeEvent(input$DE_immunecell_volcano_cell,{
    if (input$DE_immunecell_volcano_cell=="") {
      showFeedbackDanger(
        inputId = "DE_immunecell_volcano_cell",
        text = "Please select a dataset!",
      )
    } else {
      hideFeedback("DE_immunecell_volcano_cell")
    }
    
  })
  
  DE_imm_vol_update_requirement<-reactive({
    (input$DE_immunecell_vol_dataset!="" & input$DE_immunecell_volcano_cell!="")
  })
  
  observe({
    toggleState(id ="DE_imm_vol_update",
                condition =DE_imm_vol_update_requirement() )
  })
  
  DE_immunecell_volcano_result<-DEImm_Server("id_DE_Imm_Vol","Volcano",
                                             reactive(input$DE_immunecell_vol_dataset),reactive(input$DE_immunecell_volcano_cell))  %>%
    bindEvent(input$DE_imm_vol_update)
  
  shinyjs::hide(id ="DE_immunecell_vol_result_sum")
  observeEvent(input$DE_imm_vol_update, {
    shinyjs::show(id ="DE_immunecell_vol_result_sum")
  })
  
  output$DE_immunecell_volcano_result_plot_show<-renderPlot({
    input$DE_imm_vol_update
    DE_immunecell_volcano_result()$plot
  })
  
  output$DE_immunecell_vol_result_data_panel<-renderUI({
    input$DE_imm_vol_update
    DT::datatable(DE_immunecell_volcano_result()$data,
                  caption =paste("Table: Differentially Expressed Genes for",
                                 input$DE_immunecell_vol_dataset,sep = " "),
                  rownames = FALSE,
                  extensions=c('Responsive'),
                  options = list(
                    dom = 'ftipr',
                    pageLength = 10,
                    responsive = TRUE,
                    columnDefs = 
                      list(list(className = 'dt-center', 
                                targets = "_all")),
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                      "}")
                  ))
    
  })
  
  
  ##download
  output$DE_immunecell_volcano_download_svg<- downloadHandler(
    filename = function(){
      paste("DEanalysis_Immunecell_Volcano_",input$DE_immunecell_vol_dataset,".svg",sep="")
    },
    content = function(file){
      svg(file)
      print(DE_immunecell_volcano_result()$plot)
      dev.off()
    }
  )
  
  output$DE_immunecell_volcano_download_pdf<- downloadHandler(
    filename = function(){
      paste("DEanalysis_Immunecell_Volcano_",input$DE_immunecell_vol_dataset,".pdf",sep="")
    },
    content = function(file){
      pdf(file)
      print(DE_immunecell_volcano_result()$plot)
      dev.off()
    }
  )
  
  output$DE_immunecell_volcano_download_png<- downloadHandler(
    filename = function(){
      paste("DEanalysis_Immunecell_Volcano_",input$DE_immunecell_vol_dataset,".png",sep="")
    },
    content = function(file){
      png(file)
      print(DE_immunecell_volcano_result()$plot)
      dev.off()
    }
  )
  
  output$DE_immunecell_vol_fulldata_download_csv<-downloadHandler(
    filename = function(){
      paste("DEG_",input$DE_immunecell_vol_dataset,".csv",sep = "")
    },
    content = function(file){
      sep<-","
      write.table(DE_immunecell_volcano_result()$data,file,sep=sep,row.names =FALSE)
    }
  )
  
  output$DE_immunecell_vol_fulldata_download_txt<-downloadHandler(
    filename = function(){
      paste("DEG_",input$DE_immunecell_vol_dataset,".txt",sep = "")
    },
    content = function(file){
      sep<-" "
      write.table(DE_immunecell_volcano_result()$data,file,sep=sep,row.names = FALSE)
    }
  )
  
  ####==========================================DE Immunecell 2 Heatmap
  observeEvent(input$DE_immunecell_heat_dataset,{
    if (input$DE_immunecell_heat_dataset=="") {
      showFeedbackDanger(
        inputId = "DE_immunecell_heat_dataset",
        text = "Please select a dataset!",
      )
    } else {
      hideFeedback("DE_immunecell_heat_dataset")
    }
    
  })
  
  observeEvent(input$DE_immunecell_heatmap_cell,{
    if(length(input$DE_immunecell_heatmap_cell)==5){
      showFeedbackSuccess(
        inputId = "DE_immunecell_heatmap_cell",
        text = "The maximum is 5!",
      )
    }else {
      hideFeedback("DE_immunecell_heatmap_cell")
    }
    
  })
  
  
  DE_imm_heat_update_requirement<-reactive({
    (input$DE_immunecell_heat_dataset!="" & length(input$DE_immunecell_heatmap_cell)!=0)
  })
  
  observe({
    toggleState(id ="DE_imm_heat_update",
                condition =DE_imm_heat_update_requirement() )
  })
  
  
  DE_immunecell_heatmap_result<-DEImm_Server("id_DE_Imm_Heat","Heatmap",
                                             reactive(input$DE_immunecell_heat_dataset),reactive(input$DE_immunecell_heatmap_cell)) %>%
    bindEvent(input$DE_imm_heat_update)
  
  
  shinyjs::hide(id ="DE_immunecell_heat_result_sum")
  observeEvent(input$DE_imm_heat_update, {
    shinyjs::show(id ="DE_immunecell_heat_result_sum")
  })
  
  output$DE_immunecell_heatmap_result_plot_show<-renderPlot({
    DE_immunecell_heatmap_result()$plot
  },height = 600)
  
  output$DE_immunecell_heat_result_data_panel<-renderUI({
    DT::datatable(DE_immunecell_heatmap_result()$data,
                  caption =paste("Table: Differentially Expressed Genes for",
                                 input$DE_immunecell_heat_dataset,sep = " "),
                  rownames = FALSE,
                  extensions=c('Responsive'),
                  options = list(
                    dom = 'ftipr',
                    pageLength = 10,
                    responsive = TRUE,
                    columnDefs = 
                      list(list(className = 'dt-center', 
                                targets = "_all")),
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                      "}")
                  ))
    
  })
  
  ##download
  output$DE_immunecell_heatmap_download_svg<- downloadHandler(
    filename = function(){
      paste("DEanalysis_Immunecell_Heatmap_",input$DE_immunecell_heat_dataset,".svg",sep="")
    },
    content = function(file){
      svg(file,height=10, width=15)
      print(DE_immunecell_heatmap_result()$plot)
      dev.off()
    }
  )
  
  output$DE_immunecell_heatmap_download_pdf<- downloadHandler(
    filename = function(){
      paste("DEanalysis_Immunecell_Heatmap_",input$DE_immunecell_heat_dataset,".pdf",sep="")
    },
    content = function(file){
      pdf(file,height=10, width=15)
      print(DE_immunecell_heatmap_result()$plot)
      dev.off()
    }
  )
  
  output$DE_immunecell_heatmap_download_png<- downloadHandler(
    filename = function(){
      paste("DEanalysis_Immunecell_Heatmap_",input$DE_immunecell_heat_dataset,".png",sep="")
    },
    content = function(file){
      png(file,width = 800, height = 600, units = "px")
      print(DE_immunecell_heatmap_result()$plot)
      dev.off()
    }
  )
  
  output$DE_immunecell_heat_fulldata_download_csv<-downloadHandler(
    filename = function(){
      paste("DEG_",input$DE_immunecell_heat_dataset,".csv",sep = "")
    },
    content = function(file){
      sep<-","
      write.table(DE_immunecell_heatmap_result()$data,file,sep=sep,row.names =FALSE)
    }
  )
  
  output$DE_immunecell_heat_fulldata_download_txt<-downloadHandler(
    filename = function(){
      paste("DEG_",input$DE_immunecell_heat_dataset,".txt",sep = "")
    },
    content = function(file){
      sep<-" "
      write.table(DE_immunecell_heatmap_result()$data,file,sep=sep,row.names = FALSE)
    }
  )
  
  ####==========================================DE Immunecell 3 Boxplot
  observeEvent(input$DE_immunecell_box_dataset,{
    if (input$DE_immunecell_box_dataset=="") {
      showFeedbackDanger(
        inputId = "DE_immunecell_box_dataset",
        text = "Please select a dataset!",
      )
    } else {
      hideFeedback("DE_immunecell_box_dataset")
    }
    
  })
  
  observeEvent(input$DE_immunecell_boxplot_cell,{
    if(length(input$DE_immunecell_boxplot_cell)==5){
      showFeedbackSuccess(
        inputId = "DE_immunecell_boxplot_cell",
        text = "The maximum is 5!",
      )
    }else {
      hideFeedback("DE_immunecell_boxplot_cell")
    }
    
  })
  
  DE_imm_box_update_requirement<-reactive({
    (input$DE_immunecell_box_dataset!="" & length(input$DE_immunecell_boxplot_cell)!=0)
  })
  
  observe({
    toggleState(id ="DE_imm_box_update",
                condition =DE_imm_box_update_requirement() )
  })
  
  
  DE_immunecell_boxplot_result<-DEImm_Server("id_DE_Imm_Box","Boxplot",
                                             reactive(input$DE_immunecell_box_dataset),reactive(input$DE_immunecell_boxplot_cell)) %>%
    bindEvent(input$DE_imm_box_update)
  
  
  shinyjs::hide(id ="DE_immunecell_box_result_sum")
  observeEvent(input$DE_imm_box_update, {
    shinyjs::show(id ="DE_immunecell_box_result_sum")
  })
  
  output$DE_immunecell_boxplot_result_plot_show<-renderPlot({
    DE_immunecell_boxplot_result()$plot
  },height = 600)
  
  output$DE_immunecell_box_result_data_panel<-renderUI({
    DT::datatable(DE_immunecell_boxplot_result()$data,
                  caption =paste("Table: Differentially Expressed Genes for",
                                 input$DE_immunecell_box_dataset,sep = " "),
                  rownames = FALSE,
                  extensions=c('Responsive'),
                  options = list(
                    dom = 'ftipr',
                    pageLength = 10,
                    responsive = TRUE,
                    columnDefs = 
                      list(list(className = 'dt-center', 
                                targets = "_all")),
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                      "}")
                  ))
    
  })
  
  ##download
  output$DE_immunecell_boxplot_download_svg<- downloadHandler(
    filename = function(){
      paste("DEanalysis_Immunecell_Boxplot_",input$DE_immunecell_box_dataset,".svg",sep="")
    },
    content = function(file){
      svg(file,height=8, width=12)
      print(DE_immunecell_boxplot_result()$plot)
      dev.off()
    }
  )
  
  output$DE_immunecell_boxplot_download_pdf<- downloadHandler(
    filename = function(){
      paste("DEanalysis_Immunecell_Boxplot_",input$DE_immunecell_box_dataset,".pdf",sep="")
    },
    content = function(file){
      pdf(file,height=8, width=12)
      print(DE_immunecell_boxplot_result()$plot)
      dev.off()
    }
  )
  
  output$DE_immunecell_boxplot_download_png<- downloadHandler(
    filename = function(){
      paste("DEanalysis_Immunecell_Boxplot_",input$DE_immunecell_box_dataset,".png",sep="")
    },
    content = function(file){
      png(file,width = 850, height = 600, units = "px")
      print(DE_immunecell_boxplot_result()$plot)
      dev.off()
    }
  )
  
  output$DE_immunecell_box_fulldata_download_csv<-downloadHandler(
    filename = function(){
      paste("DEG_",input$DE_immunecell_box_dataset,".csv",sep = "")
    },
    content = function(file){
      sep<-","
      write.table(DE_immunecell_boxplot_result()$data,file,sep=sep,row.names =FALSE)
    }
  )
  
  output$DE_immunecell_box_fulldata_download_txt<-downloadHandler(
    filename = function(){
      paste("DEG_",input$DE_immunecell_box_dataset,".txt",sep = "")
    },
    content = function(file){
      sep<-" "
      write.table(DE_immunecell_boxplot_result()$data,file,sep=sep,row.names = FALSE)
    }
  )
  
  
  ####==========================================DE Pathway 1 Volcano
  
  updateSelectizeInput(session, "DE_pathway_volcano_pathway", 
                       choices = unique(diff_pathway_human$gs_name),
                       selected=c("HALLMARK_APICAL_JUNCTION"),
                       server = TRUE)
  
  observeEvent(input$DE_pathway_vol_dataset,{
    if (input$DE_pathway_vol_dataset=="") {
      showFeedbackDanger(
        inputId = "DE_pathway_vol_dataset",
        text = "Please select a dataset!",
      )
    } else {
      hideFeedback("DE_pathway_vol_dataset")
    }
  })
  
  DE_path_vol_update_requirement<-reactive({
    (input$DE_pathway_vol_dataset!="" & input$DE_pathway_volcano_pathway!="")
  })
  
  observe({
    toggleState(id ="DE_path_vol_update",
                condition =DE_path_vol_update_requirement() )
  })
  
  
  DE_pathway_volcano_result<-DEPath_Server("id_DE_Path_Vol","Volcano",
                                           reactive(input$DE_pathway_vol_dataset),reactive(input$DE_pathway_volcano_pathway)) %>%
    bindEvent(input$DE_path_vol_update)
  
  
  shinyjs::hide(id ="DE_pathway_vol_result_sum")
  observeEvent(input$DE_path_vol_update, {
    shinyjs::show(id ="DE_pathway_vol_result_sum")
  })
  
  
  output$DE_pathway_volcano_result_plot_show<-renderPlot({
    DE_pathway_volcano_result()
  })
  
  ##download
  output$DE_pathway_volcano_download_svg<- downloadHandler(
    filename = function(){
      paste("DEanalysis_pathway_Volcano_",input$DE_pathway_vol_dataset,".svg",sep="")
    },
    content = function(file){
      svg(file)
      print(DE_pathway_volcano_result())
      dev.off()
    }
  )
  
  output$DE_pathway_volcano_download_pdf<- downloadHandler(
    filename = function(){
      paste("DEanalysis_pathway_Volcano_",input$DE_pathway_vol_dataset,".pdf",sep="")
    },
    content = function(file){
      pdf(file)
      print(DE_pathway_volcano_result())
      dev.off()
    }
  )
  
  output$DE_pathway_volcano_download_png<- downloadHandler(
    filename = function(){
      paste("DEanalysis_pathway_Volcano_",input$DE_pathway_vol_dataset,".png",sep="")
    },
    content = function(file){
      png(file)
      print(DE_pathway_volcano_result())
      dev.off()
    }
  )
  
  output$DE_pathway_vol_fulldata_download_csv<-downloadHandler(
    filename = function(){
      paste("DEG_",input$DE_pathway_vol_dataset,".csv",sep = "")
    },
    content = function(file){
      
      DE_pathway_volcano_result_data<-DEPath_Data("id_DE_Path_Vol",
                                                  reactive(input$DE_pathway_vol_dataset)) 
      sep<-","
      write.table(DE_pathway_volcano_result_data(),file,sep=sep,row.names =FALSE)
    }
  )
  
  output$DE_pathway_vol_fulldata_download_txt<-downloadHandler(
    filename = function(){
      paste("DEG_",input$DE_pathway_vol_dataset,".txt",sep = "")
    },
    content = function(file){
      DE_pathway_volcano_result_data<-DEPath_Data("id_DE_Path_Vol",
                                                  reactive(input$DE_pathway_vol_dataset))
      sep<-" "
      write.table(DE_pathway_volcano_result_data(),file,sep=sep,row.names = FALSE)
    }
  )
  
  ####==========================================DE Pathway 2 Heatmap
  
  updateSelectizeInput(session, "DE_pathway_heatmap_pathway", 
                       choices = unique(diff_pathway_human$gs_name),
                       selected=c("HP_GENU_VALGUM","GOBP_INTERLEUKIN_8_PRODUCTION"),
                       server = TRUE)
  
  observeEvent(input$DE_pathway_heat_dataset,{
    if (input$DE_pathway_heat_dataset=="") {
      showFeedbackDanger(
        inputId = "DE_pathway_heat_dataset",
        text = "Please select a dataset!",
      )
    } else {
      hideFeedback("DE_pathway_heat_dataset")
    }
  })
  
  observeEvent(input$DE_pathway_heatmap_pathway,{
    if(length(input$DE_pathway_heatmap_pathway)==3){
      showFeedbackSuccess(
        inputId = "DE_pathway_heatmap_pathway",
        text = "The maximum is 3!",
      )
    }else {
      hideFeedback("DE_pathway_heatmap_pathway")
    }
    
  })
  
  DE_path_heat_update_requirement<-reactive({
    (input$DE_pathway_heat_dataset!="" & length(input$DE_pathway_heatmap_pathway)!=0)
  })
  
  observe({
    toggleState(id ="DE_path_heat_update",
                condition =DE_path_heat_update_requirement() )
  })
  
  
  DE_pathway_heatmap_result<-DEPath_Server("id_DE_Path_Heat","Heatmap",
                                           reactive(input$DE_pathway_heat_dataset),reactive(input$DE_pathway_heatmap_pathway))  %>%
    bindEvent(input$DE_path_heat_update)
  
  
  shinyjs::hide(id ="DE_pathway_heat_result_sum")
  observeEvent(input$DE_path_heat_update, {
    shinyjs::show(id ="DE_pathway_heat_result_sum")
  })
  
  
  output$DE_pathway_heatmap_result_plot_show<-renderPlot({
    DE_pathway_heatmap_result()
  },height = 700,width = 900)
  
  ##download
  output$DE_pathway_heatmap_download_svg<- downloadHandler(
    filename = function(){
      paste("DEanalysis_pathway_Heatmap_",input$DE_pathway_heat_dataset,".svg",sep="")
    },
    content = function(file){
      svg(file,height=10, width=15)
      print(DE_pathway_heatmap_result())
      dev.off()
    }
  )
  
  output$DE_pathway_heatmap_download_pdf<- downloadHandler(
    filename = function(){
      paste("DEanalysis_pathway_Heatmap_",input$DE_pathway_heat_dataset,".pdf",sep="")
    },
    content = function(file){
      pdf(file,height=10, width=15)
      print(DE_pathway_heatmap_result())
      dev.off()
    }
  )
  
  output$DE_pathway_heatmap_download_png<- downloadHandler(
    filename = function(){
      paste("DEanalysis_pathway_Heatmap_",input$DE_pathway_heat_dataset,".png",sep="")
    },
    content = function(file){
      png(file,width = 800, height = 600, units = "px")
      print(DE_pathway_heatmap_result())
      dev.off()
    }
  )
  
  output$DE_pathway_heat_fulldata_download_csv<-downloadHandler(
    filename = function(){
      paste("DEG_",input$DE_pathway_heat_dataset,".csv",sep = "")
    },
    content = function(file){
      
      DE_pathway_heatmap_result_data<-DEPath_Data("id_DE_Path_Heat",
                                                  reactive(input$DE_pathway_heat_dataset)) 
      sep<-","
      write.table(DE_pathway_heatmap_result_data(),file,sep=sep,row.names =FALSE)
    }
  )
  
  output$DE_pathway_heat_fulldata_download_txt<-downloadHandler(
    filename = function(){
      paste("DEG_",input$DE_pathway_heat_dataset,".txt",sep = "")
    },
    content = function(file){
      DE_pathway_heatmap_result_data<-DEPath_Data("id_DE_Path_Heat", 
                                                  reactive(input$DE_pathway_heat_dataset))
      sep<-" "
      write.table(DE_pathway_heatmap_result_data(),file,sep=sep,row.names = FALSE)
    }
  )
  
  ####==========================================DE Pathway 3 Boxplot
  
  updateSelectizeInput(session, "DE_pathway_boxplot_pathway", 
                       choices =  unique(diff_pathway_human$gs_name),
                       selected=c("HALLMARK_ADIPOGENESIS"),
                       server = TRUE)
  
  observeEvent(input$DE_pathway_box_dataset,{
    if (input$DE_pathway_box_dataset=="") {
      showFeedbackDanger(
        inputId = "DE_pathway_box_dataset",
        text = "Please select a dataset!",
      )
    } else {
      hideFeedback("DE_pathway_box_dataset")
    }
  })
  
  
  DE_path_box_update_requirement<-reactive({
    (input$DE_pathway_box_dataset!="" & input$DE_pathway_boxplot_pathway!="")
  })
  
  observe({
    toggleState(id ="DE_path_box_update",
                condition =DE_path_box_update_requirement() )
  })
  
  
  DE_pathway_boxplot_result<-DEPath_Server("id_DE_Path_Box","Boxplot",
                                           reactive(input$DE_pathway_box_dataset),reactive(input$DE_pathway_boxplot_pathway))  %>%
    bindEvent(input$DE_path_box_update)
  
  shinyjs::hide(id ="DE_pathway_box_result_sum")
  observeEvent(input$DE_path_box_update, {
    shinyjs::show(id ="DE_pathway_box_result_sum")
  })
  
  
  output$DE_pathway_boxplot_result_plot_show<-renderPlot({
    DE_pathway_boxplot_result()
  },height = 600)
  
  ##download
  output$DE_pathway_boxplot_download_svg<- downloadHandler(
    filename = function(){
      paste("DEanalysis_pathway_Boxplot_",input$DE_pathway_box_dataset,".svg",sep="")
    },
    content = function(file){
      svg(file,height=8, width=12)
      print(DE_pathway_boxplot_result())
      dev.off()
    }
  )
  
  output$DE_pathway_boxplot_download_pdf<- downloadHandler(
    filename = function(){
      paste("DEanalysis_pathway_Boxplot_",input$DE_pathway_box_dataset,".pdf",sep="")
    },
    content = function(file){
      pdf(file,height=8, width=12)
      print(DE_pathway_boxplot_result())
      dev.off()
    }
  )
  
  output$DE_pathway_boxplot_download_png<- downloadHandler(
    filename = function(){
      paste("DEanalysis_pathway_Boxplot_",input$DE_pathway_box_dataset,".png",sep="")
    },
    content = function(file){
      png(file,width = 850, height = 600, units = "px")
      print(DE_pathway_boxplot_result())
      dev.off()
    }
  )
  
  output$DE_pathway_box_fulldata_download_csv<-downloadHandler(
    filename = function(){
      paste("DEG_",input$DE_pathway_box_dataset,".csv",sep = "")
    },
    content = function(file){
      
      DE_pathway_boxplot_result_data<-DEPath_Data("id_DE_Path_Box", 
                                                  reactive(input$DE_pathway_box_dataset))
      sep<-","
      write.table(DE_pathway_boxplot_result_data(),file,sep=sep,row.names =FALSE)
    }
  )
  
  output$DE_pathway_box_fulldata_download_txt<-downloadHandler(
    filename = function(){
      paste("DEG_",input$DE_pathway_box_dataset,".txt",sep = "")
    },
    content = function(file){
      DE_pathway_boxplot_result_data<-DEPath_Data("id_DE_Path_Box",
                                                  reactive(input$DE_pathway_box_dataset))
      sep<-" "
      write.table(DE_pathway_boxplot_result_data(),file,sep=sep,row.names = FALSE)
    }
  )
  
  ##=================================7.imm_infli_GSE============================= 
  ##1\ stacking plot
  
  imm_infli_GSE_sta_choosen_data<-reactive({
    infli_dataset_list[[input$imm_infli_GSE_sta_dataset]][[input$imm_infli_GSE_sta_method]]
  })
  
  imm_infli_GSE_stacking_result_plot<-eventReactive(input$imm_infli_GSE_sta_update,{
    infli_stacking_fun(input$imm_infli_GSE_sta_dataset,input$imm_infli_GSE_sta_method)
  })
  
  imm_infli_GSE_sta_data_show<-eventReactive(input$imm_infli_GSE_sta_update,{
    imminfli_data_show_fun(input$imm_infli_GSE_sta_dataset,input$imm_infli_GSE_sta_method)
  })
  
  shinyjs::hide(id ="imm_infli_GSE_sta_result_sum")
  
  observeEvent(input$imm_infli_GSE_sta_update, {
    shinyjs::show(id ="imm_infli_GSE_sta_result_sum")

    output$imm_infli_GSE_stacking_result_plot_show<-renderPlot({
      imm_infli_GSE_stacking_result_plot()
    })
    
    output$imm_infli_GSE_sta_result_data_panel<-renderUI({
      DT::datatable(imm_infli_GSE_sta_data_show(),rownames=T,
                    extensions=c('Responsive','FixedColumns'),
                    options = list(
                      dom = 'frtip',
                      responsive = TRUE,
                      fixedColumns = list(leftColumns = c(1:5)),
                      pageLength = 10,
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                        "}")
                    ))
    })
    
  })
  
  #observer
  observeEvent(input$imm_infli_GSE_sta_dataset,{
    if (input$imm_infli_GSE_sta_dataset=="") {
      showFeedbackDanger(
        inputId = "imm_infli_GSE_sta_dataset",
        text = "Please select a dataset!",
      )
    } else {
      hideFeedback("imm_infli_GSE_sta_dataset")
    }
    
  })
  
  imm_infli_GSE_sta_update_requirement<-reactive({
    (input$imm_infli_GSE_sta_dataset!="" & input$imm_infli_GSE_sta_method!="")
  })
  
  observe({
    toggleState(id ="imm_infli_GSE_sta_update",
                condition =imm_infli_GSE_sta_update_requirement())
  })
  
  ##download
  output$imm_infli_GSE_stacking_download_svg<- downloadHandler(
    filename = function(){
      paste("ImmInfli_GSE_Stacking_",input$imm_infli_GSE_sta_dataset,".svg",sep="")
    },
    content = function(file){
      svg(file,height=10, width=15)
      print(imm_infli_GSE_stacking_result_plot())
      dev.off()
    }
  )
  
  output$imm_infli_GSE_stacking_download_pdf<- downloadHandler(
    filename = function(){
      paste("ImmInfli_GSE_Stacking_",input$imm_infli_GSE_sta_dataset,".pdf",sep="")
    },
    content = function(file){
      pdf(file,height=10, width=15)
      print(imm_infli_GSE_stacking_result_plot())
      dev.off()
    }
  )
  
  output$imm_infli_GSE_stacking_download_png<- downloadHandler(
    filename = function(){
      paste("ImmInfli_GSE_Stacking_",input$imm_infli_GSE_sta_dataset,".png",sep="")
    },
    content = function(file){
      png(file,width = 800, height = 600, units = "px")
      print(imm_infli_GSE_stacking_result_plot())
      dev.off()
    }
  )
  
  output$imm_infli_GSE_sta_fulldata_download_csv<-downloadHandler(
    filename = function(){
      paste("imm_infli_GSE_",input$imm_infli_GSE_sta_dataset,"_",input$imm_infli_GSE_sta_method,".csv",sep = "")
    },
    content = function(file){
      sep<-","
      write.table(imm_infli_GSE_sta_data_show(),file,sep=sep,row.names =TRUE)
    }
  )
  
  output$imm_infli_GSE_sta_fulldata_download_txt<-downloadHandler(
    filename = function(){
      paste("imm_infli_GSE_",input$imm_infli_GSE_sta_dataset,"_",input$imm_infli_GSE_sta_method,".txt",sep = "")
    },
    content = function(file){
      sep<-" "
      write.table(imm_infli_GSE_sta_data_show(),file,sep=sep,row.names =TRUE)
    }
  )
  
  
  ##2\ heatmap
  observeEvent(input$imm_infli_GSE_heat_dataset, {
    updateSelectInput(session,"imm_infli_GSE_heat_method", 
                      choices =names(infli_dataset_list[[input$imm_infli_GSE_heat_dataset]]),
                      selected = "quanTIseq"
    )
  })
  
  imm_infli_GSE_heat_choosen_data<-reactive({
    infli_dataset_list[[input$imm_infli_GSE_heat_dataset]][[input$imm_infli_GSE_heat_method]]
  })
  
  imm_infli_GSE_heatmap_result_plot<-eventReactive(input$imm_infli_GSE_heat_update,{
    
    infli_heatmap_fun(input$imm_infli_GSE_heat_dataset,input$imm_infli_GSE_heat_method,
                      input$imm_infli_GSE_heatmap_color)
  })
  
  imm_infli_GSE_heat_data_show<-eventReactive(input$imm_infli_GSE_heat_update,{
    imminfli_data_show_fun(input$imm_infli_GSE_heat_dataset,input$imm_infli_GSE_heat_method)
  })
  
  shinyjs::hide(id ="imm_infli_GSE_heat_result_sum")
  
  observeEvent(input$imm_infli_GSE_heat_update, {
    shinyjs::show(id ="imm_infli_GSE_heat_result_sum")

    output$imm_infli_GSE_heatmap_result_plot_show<-renderPlot({
      imm_infli_GSE_heatmap_result_plot()
    },height = 700,width = 900)
    
    output$imm_infli_GSE_heat_result_data_panel<-renderUI({
      DT::datatable(imm_infli_GSE_heat_data_show(),rownames=T,
                    extensions=c('Responsive','FixedColumns'),
                    options = list(
                      dom = 'frtip',
                      responsive = TRUE,
                      fixedColumns = list(leftColumns = c(1:5)),
                      pageLength = 10,
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                        "}")
                    ))
    })
    
  })
  
  #observer
  observeEvent(input$imm_infli_GSE_heat_dataset,{
    if (input$imm_infli_GSE_heat_dataset=="") {
      showFeedbackDanger(
        inputId = "imm_infli_GSE_heat_dataset",
        text = "Please select a dataset!",
      )
    } else {
      hideFeedback("imm_infli_GSE_heat_dataset")
    }
    
  })
  
  imm_infli_GSE_heat_update_requirement<-reactive({
    (input$imm_infli_GSE_heat_dataset!="" & input$imm_infli_GSE_heat_method!="")
  })
  
  observe({
    toggleState(id ="imm_infli_GSE_heat_update",
                condition =imm_infli_GSE_heat_update_requirement())
  })
  
  ##download
  output$imm_infli_GSE_heatmap_download_svg<- downloadHandler(
    filename = function(){
      paste("ImmInfli_GSE_Heatmap_",input$imm_infli_GSE_heat_dataset,".svg",sep="")
    },
    content = function(file){
      svg(file,height=10, width=20)
      print(imm_infli_GSE_heatmap_result_plot())
      dev.off()
    }
  )
  
  output$imm_infli_GSE_heatmap_download_pdf<- downloadHandler(
    filename = function(){
      paste("ImmInfli_GSE_Heatmap_",input$imm_infli_GSE_heat_dataset,".pdf",sep="")
    },
    content = function(file){
      pdf(file,height=10, width=20)
      print(imm_infli_GSE_heatmap_result_plot())
      dev.off()
    }
  )
  
  output$imm_infli_GSE_heatmap_download_png<- downloadHandler(
    filename = function(){
      paste("ImmInfli_GSE_Heatmap_",input$imm_infli_GSE_heat_dataset,".png",sep="")
    },
    content = function(file){
      png(file,width = 800, height = 600, units = "px")
      print(imm_infli_GSE_heatmap_result_plot())
      dev.off()
    }
  )
  
  output$imm_infli_GSE_heat_fulldata_download_csv<-downloadHandler(
    filename = function(){
      paste("imm_infli_GSE_",input$imm_infli_GSE_heat_dataset,"_",input$imm_infli_GSE_heat_method,".csv",sep = "")
    },
    content = function(file){
      sep<-","
      write.table(imm_infli_GSE_heat_data_show(),file,sep=sep,row.names =TRUE)
    }
  )
  
  output$imm_infli_GSE_heat_fulldata_download_txt<-downloadHandler(
    filename = function(){
      paste("imm_infli_GSE_",input$imm_infli_GSE_heat_dataset,"_",input$imm_infli_GSE_heat_method,".txt",sep = "")
    },
    content = function(file){
      sep<-" "
      write.table(imm_infli_GSE_heat_data_show(),file,sep=sep,row.names =TRUE)
    }
  )
  
  
  
  ##3\ boxplot
  observeEvent(input$imm_infli_GSE_box_dataset, {
    updateSelectInput(session,"imm_infli_GSE_box_method", 
                      choices =names(infli_dataset_list[[input$imm_infli_GSE_box_dataset]]),
                      selected = "quanTIseq"
    )
  })
  
  imm_infli_GSE_box_choosen_data<-reactive({
    infli_dataset_list[[input$imm_infli_GSE_box_dataset]][[input$imm_infli_GSE_box_method]]
  })
  
  observeEvent(imm_infli_GSE_box_choosen_data(),{
    updateSelectizeInput(session,inputId = "imm_infli_GSE_boxplot_immunecell",
                         choices =colnames(imm_infli_GSE_box_choosen_data())[-which(colnames(imm_infli_GSE_box_choosen_data())=="ID")],
                         selected = colnames(imm_infli_GSE_box_choosen_data())[2:4],
                         server = T
    )
  })
  
  imm_infli_GSE_boxplot_result_plot<-eventReactive(input$imm_infli_GSE_box_update,{
    
    if(input$imm_infli_GSE_boxplot_type=="All immune cells"){
      infli_all_boxplot_fun(input$imm_infli_GSE_box_dataset,input$imm_infli_GSE_box_method,
                            input$imm_infli_GSE_boxplot_color_p,input$imm_infli_GSE_boxplot_color_n)
      
    }else if(input$imm_infli_GSE_boxplot_type=="Search, select 1~5 immune cell(s):"){
      infli_custom_boxplot_fun(input$imm_infli_GSE_box_dataset,input$imm_infli_GSE_box_method,
                               input$imm_infli_GSE_boxplot_immunecell,
                               input$imm_infli_GSE_boxplot_color_p,input$imm_infli_GSE_boxplot_color_n)
      
    }
    
  })
  
  imm_infli_GSE_box_data_show<-eventReactive(input$imm_infli_GSE_box_update,{
    imminfli_data_show_fun(input$imm_infli_GSE_box_dataset,input$imm_infli_GSE_box_method)
  })
  
  shinyjs::hide(id ="imm_infli_GSE_box_result_sum")
  
  observeEvent(input$imm_infli_GSE_box_update, {
    shinyjs::show(id ="imm_infli_GSE_box_result_sum")

    output$imm_infli_GSE_boxplot_result_plot_show<-renderPlot({
      imm_infli_GSE_boxplot_result_plot()
    },height = 700)
    
    output$imm_infli_GSE_box_result_data_panel<-renderUI({
      DT::datatable(imm_infli_GSE_box_data_show(),rownames=T,
                    extensions=c('Responsive','FixedColumns'),
                    options = list(
                      dom = 'frtip',
                      responsive = TRUE,
                      fixedColumns = list(leftColumns = c(1:5)),
                      pageLength = 10,
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                        "}")
                    ))
    })
    
  })
  
  #observer
  observeEvent(input$imm_infli_GSE_box_dataset,{
    if (input$imm_infli_GSE_box_dataset=="") {
      showFeedbackDanger(
        inputId = "imm_infli_GSE_box_dataset",
        text = "Please select a dataset!",
      )
    } else {
      hideFeedback("imm_infli_GSE_box_dataset")
    }
    
  })
  
  observeEvent(input$imm_infli_GSE_boxplot_immunecell,{
    if(length(input$imm_infli_GSE_boxplot_immunecell)==5){
      showFeedbackSuccess(
        inputId = "imm_infli_GSE_boxplot_immunecell",
        text = "The maximum is 5!",
      )
    }else {
      hideFeedback("imm_infli_GSE_boxplot_immunecell")
    }
    
  })
  
  imm_infli_GSE_box_update_requirement<-reactive({
    if(input$imm_infli_GSE_boxplot_type=="All immune cells"){
      (input$imm_infli_GSE_box_dataset!="" & input$imm_infli_GSE_box_method!="")
      
    }else if(input$imm_infli_GSE_boxplot_type== "Search, select 1~5 immune cell(s):"){
      (input$imm_infli_GSE_box_dataset!="" & input$imm_infli_GSE_box_method!="" &length(input$imm_infli_GSE_boxplot_immunecell)>0)
    }
  })
  
  observe({
    toggleState(id ="imm_infli_GSE_box_update",
                condition =imm_infli_GSE_box_update_requirement())
  })
  
  ##download
  output$imm_infli_GSE_boxplot_download_svg<- downloadHandler(
    filename = function(){
      paste("ImmInfli_GSE_Boxplot_",input$imm_infli_GSE_box_dataset,".svg",sep="")
    },
    content = function(file){
      svg(file,height=10, width=11)
      print(imm_infli_GSE_boxplot_result_plot())
      dev.off()
    }
  )
  
  output$imm_infli_GSE_boxplot_download_pdf<- downloadHandler(
    filename = function(){
      paste("ImmInfli_GSE_Boxplot_",input$imm_infli_GSE_box_dataset,".pdf",sep="")
    },
    content = function(file){
      pdf(file,height=10, width=11)
      print(imm_infli_GSE_boxplot_result_plot())
      dev.off()
    }
  )
  
  output$imm_infli_GSE_boxplot_download_png<- downloadHandler(
    filename = function(){
      paste("ImmInfli_GSE_Boxplot_",input$imm_infli_GSE_box_dataset,".png",sep="")
    },
    content = function(file){
      png(file,width = 800, height = 600, units = "px")
      print(imm_infli_GSE_boxplot_result_plot())
      dev.off()
    }
  )
  
  output$imm_infli_GSE_box_fulldata_download_csv<-downloadHandler(
    filename = function(){
      paste("imm_infli_GSE_",input$imm_infli_GSE_box_dataset,"_",input$imm_infli_GSE_box_method,".csv",sep = "")
    },
    content = function(file){
      sep<-","
      write.table(imm_infli_GSE_box_data_show(),file,sep=sep,row.names =TRUE)
    }
  )
  
  output$imm_infli_GSE_box_fulldata_download_txt<-downloadHandler(
    filename = function(){
      paste("imm_infli_GSE_",input$imm_infli_GSE_box_dataset,"_",input$imm_infli_GSE_box_method,".txt",sep = "")
    },
    content = function(file){
      sep<-" "
      write.table(imm_infli_GSE_box_data_show(),file,sep=sep,row.names =TRUE)
    }
  )
  ##=================================8.imm_infli_immunecell============================= 
  
  imm_infli_immcell_df<-eventReactive(input$imm_infli_immcell_method,{
    immu_infli_dataset_list[[input$imm_infli_immcell_method]][[input$imm_infli_immcell_method]]
  })
  
  imm_infli_immcell_dataset_choices<-eventReactive(imm_infli_immcell_df(),{
    gsechoices<-unique(imm_infli_immcell_df()$GSE)
    imm_infli_immcell_dataset_choices_df<-GSEdatalist[which(GSEdatalist$name%in%gsechoices),c(1,16)]
    imm_infli_immcell_dataset_choices<-setNames(imm_infli_immcell_dataset_choices_df$name,
                                                         imm_infli_immcell_dataset_choices_df$samplenum)
  })
  
  observeEvent(imm_infli_immcell_df(),{
    updateSelectizeInput(session,inputId = "imm_infli_immcell_dataset",
                         choices = imm_infli_immcell_dataset_choices(),
                         selected =c("HNSCC_GSE72536","HNSCC_GSE74927","CC_GSE113942",
                                     "CC_GSE122697") ,
                         server = T)
  })
  
  observeEvent(imm_infli_immcell_df(),{
    updateSelectizeInput(session,inputId = "imm_infli_immcell_boxplot_immunecell",
                         choices =unique(imm_infli_immcell_df()$cell_types),
                         server = T)
  })
  
  
  imm_infli_immcell_boxplot_result_plot<-eventReactive(input$imm_infli_immcell_update,{
    immu_infli_boxplot(input$imm_infli_immcell_method,input$imm_infli_immcell_dataset,
                       input$imm_infli_immcell_boxplot_immunecell,
                       input$imm_infli_immcell_boxplot_color_p,input$imm_infli_immcell_boxplot_color_n)
    
    
  })
  
  shinyjs::hide(id ="imm_infli_immcell_result_smu")
  
  observeEvent(input$imm_infli_immcell_update, {
    shinyjs::show(id ="imm_infli_immcell_result_smu")

    output$imm_infli_immcell_result_plot_panel<-renderPlot({
      imm_infli_immcell_boxplot_result_plot()
    },height = 500,width = 900)
    
  })
  
  ##observe
  observeEvent(input$imm_infli_immcell_method,{
    if (input$imm_infli_immcell_method=="") {
      showFeedbackDanger(
        inputId = "imm_infli_immcell_method",
        text = "Please select an algorithm!",
      )
    } else {
      hideFeedback("imm_infli_immcell_method")
    }
    
  })
  
  observeEvent(input$imm_infli_immcell_dataset,{
    if (length(input$imm_infli_immcell_dataset)==0) {
      showFeedbackDanger(
        inputId = "imm_infli_immcell_dataset",
        text = "Please select at least one dataset!",
      )
    } else if(length(input$imm_infli_immcell_dataset)==5){
      showFeedbackSuccess(
        inputId = "imm_infli_immcell_dataset",
        text = "The maximum is 5!",
      )
    }else {
      hideFeedback("imm_infli_immcell_dataset")
    }
    
  })
  
  observeEvent(input$imm_infli_immcell_boxplot_immunecell,{
    if (input$imm_infli_immcell_boxplot_immunecell=="") {
      showFeedbackDanger(
        inputId = "imm_infli_immcell_boxplot_immunecell",
        text = "Please select one immune cell!",
      )
    } else {
      hideFeedback("imm_infli_immcell_boxplot_immunecell")
    }
    
  })
  
  imm_infli_immcell_update_requirement<-reactive({
    length(input$imm_infli_immcell_dataset)>0 & input$imm_infli_immcell_boxplot_immunecell!=""
  })
  
  observe({
    toggleState(id ="imm_infli_immcell_update",
                condition =imm_infli_immcell_update_requirement())
  })
  
  ##download
  output$imm_infli_immcell_boxplot_download_svg<- downloadHandler(
    filename = function(){
      paste("ImmInfli_InnumeCell_Boxplot_",input$imm_infli_immcell_method,".svg",sep="")
    },
    content = function(file){
      svg(file,height=7, width=12)
      print(imm_infli_immcell_boxplot_result_plot())
      dev.off()
    }
  )
  
  output$imm_infli_immcell_boxplot_download_pdf<- downloadHandler(
    filename = function(){
      paste("ImmInfli_InnumeCell_Boxplot_",input$imm_infli_immcell_method,".pdf",sep="")
    },
    content = function(file){
      pdf(file,height=7, width=12)
      print(imm_infli_immcell_boxplot_result_plot())
      dev.off()
    }
  )
  
  output$imm_infli_immcell_boxplot_download_png<- downloadHandler(
    filename = function(){
      paste("ImmInfli_ImmuneCell_Boxplot_",input$imm_infli_immcell_method,".png",sep="")
    },
    content = function(file){
      png(file,width = 850, height = 500, units = "px")
      print(imm_infli_immcell_boxplot_result_plot())
      dev.off()
    }
  )
  
  
  ###==========imm_infli_meta_forest
  observeEvent(input$imm_infli_forest_method,{
    updateSelectizeInput(session,inputId = "imm_infli_forest_immunecell",
                         choices = unique(unique(immu_infli_dataset_list[[input$imm_infli_forest_method]][[input$imm_infli_forest_method]]$cell_types)))
  })
  
  imm_infli_forest_process<-eventReactive(input$imm_infli_forest_update,{
    
    forest_method_df<-immu_infli_dataset_list[[input$imm_infli_forest_method]][[input$imm_infli_forest_method]]
    
    forest_method_df<-forest_method_df%>%
      dplyr::filter(GSE%in%diff_infli_dataset_name)%>%
      dplyr::filter(cell_types==input$imm_infli_forest_immunecell)%>%
      subset(select=-c(sample))
    
    imm_infli_meta_list= lapply(unique(forest_method_df$GSE),metadf_fun,df=forest_method_df)
    imm_infli_meta_df<- do.call(rbind,imm_infli_meta_list)
    imm_infli_meta_df$Diease<-gsub("_.*", "", imm_infli_meta_df$study)
    
    imm_infli_meta_df
  })
  
  imm_infli_forest_result_plot<-eventReactive(input$imm_infli_forest_update,{
    meta_forest_plot(data=imm_infli_forest_process(),
                     sm_method=input$imm_infli_forest_sm,
                     model_choice=input$imm_infli_forest_model,
                     subgroup_choice=input$imm_infli_forest_subgroup)
    
  })
  
  
  shinyjs::hide(id ="imm_infli_forest_result_smu")
  
  observeEvent(input$imm_infli_forest_update, {
    shinyjs::show(id ="imm_infli_forest_result_smu")
    
    output$imm_infli_forest_result_plot_panel<-renderPlot({
      imm_infli_forest_result_plot()
    },height = 700,width = 950)
    
    output$imm_infli_forest_result_data_panel<-renderUI({
      DT::datatable(imm_infli_forest_process(),rownames=F,
                    caption = paste("Table: Meta-analysis for",input$imm_infli_forest_method,input$imm_infli_forest_immunecell),
                    extensions=c('Responsive'),
                    options = list(
                      dom = 'rtip',
                      responsive = TRUE,
                      pageLength = 10,
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                        "}")
                    ))
    })
    
    
  })
  
  
  ##download
  output$imm_infli_forest_download_svg<- downloadHandler(
    filename = function(){
      paste("ImmInfli_Forest_",input$imm_infli_forest_method,"_",input$imm_infli_forest_immunecell,
            ".svg",sep="")
    },
    content = function(file){
      svg(file,height=10, width=15)
      meta_forest_plot(data=imm_infli_forest_process(),
                       sm_method=input$imm_infli_forest_sm,
                       model_choice=input$imm_infli_forest_model,
                       subgroup_choice=input$imm_infli_forest_subgroup)
      dev.off()
    }
  )
  
  output$imm_infli_forest_download_pdf<- downloadHandler(
    filename = function(){
      paste("ImmInfli_Forest_",input$imm_infli_forest_method,"_",input$imm_infli_forest_immunecell,".pdf",sep="")
    },
    content = function(file){
      pdf(file,height=10, width=15)
      meta_forest_plot(data=imm_infli_forest_process(),
                       sm_method=input$imm_infli_forest_sm,
                       model_choice=input$imm_infli_forest_model,
                       subgroup_choice=input$imm_infli_forest_subgroup)
      dev.off()
    }
  )
  
  output$imm_infli_forest_download_png<- downloadHandler(
    filename = function(){
      paste("ImmInfli_Forest_",input$imm_infli_forest_method,"_",input$imm_infli_forest_immunecell,".png",sep="")
    },
    content = function(file){
      png(file,width = 1000, height = 700, units = "px")
      meta_forest_plot(data=imm_infli_forest_process(),
                       sm_method=input$imm_infli_forest_sm,
                       model_choice=input$imm_infli_forest_model,
                       subgroup_choice=input$imm_infli_forest_subgroup)
      dev.off()
    }
  )
  
  output$imm_infli_forest_fulldata_download_csv<-downloadHandler(
    filename = function(){
      paste("ImmInfli_Forest_",input$imm_infli_forest_method,"_",input$imm_infli_forest_immunecell,".csv",sep = "")
    },
    content = function(file){
      sep<-","
      write.table(imm_infli_forest_process(),file,sep=sep,row.names =FALSE)
    }
  )
  
  output$imm_infli_forest_fulldata_download_txt<-downloadHandler(
    filename = function(){
      paste("ImmInfli_Forest_",input$imm_infli_forest_method,"_",input$imm_infli_forest_immunecell,".txt",sep = "")
    },
    content = function(file){
      sep<-" "
      write.table(imm_infli_forest_process(),file,sep=sep,row.names =FALSE)
    }
  )
  
  ##observer
  
  observeEvent(input$imm_infli_forest_immunecell,{
    if (input$imm_infli_forest_immunecell=="") {
      showFeedbackDanger(
        inputId = "imm_infli_forest_immunecell",
        text = "Please select an immune cell!",
      )
    } else {
      hideFeedback("imm_infli_forest_immunecell")
    }
  })
  
  
  observe({
    toggleState(id ="imm_infli_forest_update",
                condition =input$imm_infli_forest_immunecell!="")
  })
  
  
  ##=================================9.ClusterProfiler=============================
  ##===============ora
  enrich_cluster_ora_data<- reactive({
    if(input$enrich_cluster_ora_dataset!=""){
      load(paste("www/Data/5.Pathway_enrichment/enrichdata/",input$enrich_cluster_ora_dataset,
                 "/ORA.Rdata",sep=""))
      list(GO=get("GO"),KEGG=get("KEGG"),H=get("H"),CP=get("CP"),C5=get("C5"))
    }
  })
  
  enrich_cluster_ora_pathwaydata<-reactive({
    switch (input$enrich_cluster_ora_pathwayset,
            "GO(Gene Ontology gene sets)"=enrich_cluster_ora_data()$GO,
            "KEGG(Kyoto Encyclopedia of Genes and Genomes)"=enrich_cluster_ora_data()$KEGG,
            "H(hallmark gene sets)"=enrich_cluster_ora_data()$H,
            "CP(Canonical pathways)"=enrich_cluster_ora_data()$CP,
            "C5(ontology gene sets)"=enrich_cluster_ora_data()$C5
    )
    
  })
  
  observeEvent(enrich_cluster_ora_pathwaydata(), {
    if(nrow(enrich_cluster_ora_pathwaydata())==0){
      updateSelectizeInput (session,"enrich_cluster_ora_path_category", 
                            options = list(
                              placeholder = "Sorry! Without available pathways!" ,
                              maxItems = 1),
                            server = TRUE)
    }else{
      updateSelectizeInput (session,"enrich_cluster_ora_path_category", 
                            choices =enrich_cluster_ora_pathwaydata()@result$Description[which(enrich_cluster_ora_pathwaydata()@result$p.adjust<0.05)],
                            options = list(placeholder = "Please search and select pathways!" ,
                                           maxItems = 20),
                            server = TRUE)
    }
    
  })
  
  enrich_cluster_ora_path_category_sum<-reactive({
    switch (input$enrich_cluster_ora_path_num,
            "Top20"=20,
            "Customize"=input$enrich_cluster_ora_path_category
    )
  })
  
  
  #plot
  enrich_cluster_ora_result_barplot<-eventReactive(input$enrich_cluster_ora_update,{
    if(nrow(enrich_cluster_ora_pathwaydata())==0){
      cluster_non_pathway_plot("Over Representation Analysis(ORA)")
    }else{
      enrichment_ora_plot("Bar Plot",enrich_cluster_ora_pathwaydata(),
                          enrich_cluster_ora_path_category_sum(),input$enrich_cluster_ora_dataset)
    }
  })
  
  enrich_cluster_ora_result_dotplot<-eventReactive(input$enrich_cluster_ora_update,{
    if(nrow(enrich_cluster_ora_pathwaydata())==0){
      cluster_non_pathway_plot("Over Representation Analysis(ORA)")
    }else{
      enrichment_ora_plot("Dot plot",enrich_cluster_ora_pathwaydata(),
                          enrich_cluster_ora_path_category_sum(),input$enrich_cluster_ora_dataset)
    }
    
  })
  
  
  enrich_cluster_ora_result_enrichmentmap<-eventReactive(input$enrich_cluster_ora_update,{
    if(is.numeric(enrich_cluster_ora_path_category_sum())){ #top20
      if(nrow(enrich_cluster_ora_pathwaydata())==0){
        cluster_non_pathway_plot("Over Representation Analysis(ORA)")
      }else if(nrow(enrich_cluster_ora_pathwaydata())==1){
        cluster_one_pathway_plot("Over Representation Analysis(ORA)")
      }else{
        enrichment_ora_plot("Enrichment Map",enrich_cluster_ora_pathwaydata(),
                            enrich_cluster_ora_path_category_sum(),input$enrich_cluster_ora_dataset)
      }
    }else{ #customize
      if(length(enrich_cluster_ora_path_category_sum())==1){
        cluster_one_pathway_plot("Over Representation Analysis(ORA)")
      }else{
        enrichment_ora_plot("Enrichment Map",enrich_cluster_ora_pathwaydata(),
                            enrich_cluster_ora_path_category_sum(),input$enrich_cluster_ora_dataset)
      }
    }
    
  })
  
  enrich_cluster_ora_result_data<-eventReactive(input$enrich_cluster_ora_update,{
    cluster_show_ora_data(enrich_cluster_ora_pathwaydata(),input$enrich_cluster_ora_pathwayset)
  })
  
  
  shinyjs::hide(id="enrich_cluster_ora_result_smu")
  observeEvent(input$enrich_cluster_ora_update, {
    shinyjs::show(id="enrich_cluster_ora_result_smu")

    output$enrich_cluster_ora_result_barplot_show<-renderPlot({
      enrich_cluster_ora_result_barplot()
    },height = 800,width =700)
    
    output$enrich_cluster_ora_result_dotplot_show<-renderPlot({
      enrich_cluster_ora_result_dotplot()
    },height = 800,width =700)
    
    output$enrich_cluster_ora_result_enrichmentmap_show<-renderPlot({
      enrich_cluster_ora_result_enrichmentmap()
    })
    
    output$enrich_cluster_ora_result_data_panel<-renderUI({
      DT::datatable(enrich_cluster_ora_result_data(),
                    "Table: enrichResult for the selected pathwayset",
                    selection = "single",
                    extensions=c('Responsive'),
                    options = list(
                      dom = 'ftipr',
                      pageLength = 10,
                      responsive = TRUE,
                      columnDefs = 
                        list(list(className = 'none',targets =c(-1))),
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                        "}")
                      
                    ))
      
    })
    
    
  })
  
  #observe
  
  observeEvent(input$enrich_cluster_ora_dataset,{
    if (input$enrich_cluster_ora_dataset=="") {
      showFeedbackDanger(
        inputId = "enrich_cluster_ora_dataset",
        text = "Please select a dataset!",
      )
    } else {
      hideFeedback("enrich_cluster_ora_dataset")
    }
    
  })
  
  observeEvent(input$enrich_cluster_ora_path_category,{
    if (is.null(input$enrich_cluster_ora_path_category)) {
      showFeedbackDanger(
        inputId = "enrich_cluster_ora_path_category",
        text = "Please select one pathway at least!",
      )
    } else if(length(input$enrich_cluster_ora_path_category)==20){
      showFeedbackSuccess(
        inputId = "enrich_cluster_ora_path_category",
        text = "The maximum is 20!",
      )
    }else {
      hideFeedback("enrich_cluster_ora_path_category")
    }
    
  })
  
  enrich_cluster_ora_update_requirement<-reactive({
    if(input$enrich_cluster_ora_path_num=="Top20"){
      input$enrich_cluster_ora_dataset!=""
    }else if(input$enrich_cluster_ora_path_num=="Customize"){
      (input$enrich_cluster_ora_dataset!="" &length(input$enrich_cluster_ora_path_category)>0)
    }
  })
  
  observe({
    toggleState(id ="enrich_cluster_ora_update",
                condition =enrich_cluster_ora_update_requirement())
  })
  
  ##download
  output$enrich_cluster_ora_fulldata_download_csv<-downloadHandler(
    filename = function(){
      paste("ClusterProfiler_ORA_",input$enrich_cluster_ora_dataset,
            "_",input$enrich_cluster_ora_pathwayset,".csv",sep = "")
    },
    content = function(file){
      sep<-","
      write.table(enrich_cluster_ora_result_data(),file,sep=sep,row.names =FALSE)
    }
  )
  
  output$enrich_cluster_ora_fulldata_download_txt<-downloadHandler(
    filename = function(){
      paste("ClusterProfiler_ORA_",input$enrich_cluster_ora_dataset,
            "_",input$enrich_cluster_ora_pathwayset,".txt",sep = "")
    },
    content = function(file){
      sep<-" "
      write.table(enrich_cluster_ora_result_data(),file,sep=sep,row.names = FALSE)
    }
  )
  
  output$enrich_cluster_ora_barplot_download_svg<- downloadHandler(
    filename = function(){
      paste("ORA_Barplot_",input$enrich_cluster_ora_dataset,".svg",sep="")
    },
    content = function(file){
      #pdf(file)
      svg(file,height=10, width=10)
      print(enrich_cluster_ora_result_barplot())
      dev.off()
    }
  )
  
  output$enrich_cluster_ora_barplot_download_pdf<- downloadHandler(
    filename = function(){
      paste("ORA_Barplot_",input$enrich_cluster_ora_dataset,".pdf",sep="")
    },
    content = function(file){
      #pdf(file)
      pdf(file,height=10, width=10)
      print(enrich_cluster_ora_result_barplot())
      dev.off()
    }
  )
  
  output$enrich_cluster_ora_barplot_download_png<- downloadHandler(
    filename = function(){
      paste("ORA_Barplot_",input$enrich_cluster_ora_dataset,".png",sep="")
    },
    content = function(file){
      png(file,width = 600, height = 600, units = "px")
      print(enrich_cluster_ora_result_barplot())
      dev.off()
    }
  )
  
  output$enrich_cluster_ora_dotplot_download_svg<- downloadHandler(
    filename = function(){
      paste("ORA_Dotplot_",input$enrich_cluster_ora_dataset,".svg",sep="")
    },
    content = function(file){
      svg(file,height=10, width=10)
      print(enrich_cluster_ora_result_dotplot())
      dev.off()
    }
  )
  
  output$enrich_cluster_ora_dotplot_download_pdf<- downloadHandler(
    filename = function(){
      paste("ORA_Dotplot_",input$enrich_cluster_ora_dataset,".pdf",sep="")
    },
    content = function(file){
      pdf(file,height=10, width=10)
      print(enrich_cluster_ora_result_dotplot())
      dev.off()
    }
  )
  
  output$enrich_cluster_ora_dotplot_download_png<- downloadHandler(
    filename = function(){
      paste("ORA_Dotplot_",input$enrich_cluster_ora_dataset,".png",sep="")
    },
    content = function(file){
      png(file,width = 600, height = 600, units = "px")
      print(enrich_cluster_ora_result_dotplot())
      dev.off()
    }
  )
  
  output$enrich_cluster_ora_enrichmentmap_download_svg<- downloadHandler(
    filename = function(){
      paste("ORA_Enrichmentmap_",input$enrich_cluster_ora_dataset,".svg",sep="")
    },
    content = function(file){
      svg(file)
      print(enrich_cluster_ora_result_enrichmentmap())
      dev.off()
    }
  )
  
  output$enrich_cluster_ora_enrichmentmap_download_pdf<- downloadHandler(
    filename = function(){
      paste("ORA_Enrichmentmap_",input$enrich_cluster_ora_dataset,".pdf",sep="")
    },
    content = function(file){
      pdf(file)
      print(enrich_cluster_ora_result_enrichmentmap())
      dev.off()
    }
  )
  
  output$enrich_cluster_ora_enrichmentmap_download_png<- downloadHandler(
    filename = function(){
      paste("ORA_Enrichmentmap_",input$enrich_cluster_ora_dataset,".png",sep="")
    },
    content = function(file){
      png(file)
      print(enrich_cluster_ora_result_enrichmentmap())
      dev.off()
    }
  )
  
  ##================gsea
  enrich_cluster_gsea_data<- reactive({
    if(input$enrich_cluster_gsea_dataset!=""){
      load(paste("www/Data/5.Pathway_enrichment/enrichdata/",input$enrich_cluster_gsea_dataset,
                 "/GSEA.Rdata",sep=""))
      list(GO=get("GO"),KEGG=get("KEGG"),H=get("H"),CP=get("CP"),C5=get("C5"))
    }
  })
  
  enrich_cluster_gsea_pathwaydata<-reactive({
    switch (input$enrich_cluster_gsea_pathwayset,
            "GO(Gene Ontology gene sets)"=enrich_cluster_gsea_data()$GO,
            "KEGG(Kyoto Encyclopedia of Genes and Genomes)"=enrich_cluster_gsea_data()$KEGG,
            "H(hallmark gene sets)"=enrich_cluster_gsea_data()$H,
            "CP(Canonical pathways)"=enrich_cluster_gsea_data()$CP,
            "C5(ontology gene sets)"=enrich_cluster_gsea_data()$C5
    )
    
  })
  
  observeEvent(enrich_cluster_gsea_pathwaydata(), {
    if(nrow(enrich_cluster_gsea_pathwaydata())==0){
      updateSelectizeInput (session,"enrich_cluster_gsea_path_category", 
                            options = list(
                              placeholder = "Sorry! Without available pathways!" ,
                              maxItems = 1),
                            server = TRUE)
    }else{
      updateSelectizeInput (session,"enrich_cluster_gsea_path_category", 
                            choices =enrich_cluster_gsea_pathwaydata()@result$Description[which(enrich_cluster_gsea_pathwaydata()@result$p.adjust<0.05)],
                            options = list(placeholder = "Please search and select pathways!" ,
                                           maxItems = 20),
                            server = TRUE)
    }
  })
  
  enrich_cluster_gsea_path_category_sum<-reactive({
    switch (input$enrich_cluster_gsea_path_num,
            "Top20"=20,
            "Customize"=input$enrich_cluster_gsea_path_category
    )
  })
  
  enrich_cluster_gsea_result_barplot<-eventReactive(input$enrich_cluster_gsea_update,{
    if(nrow(enrich_cluster_gsea_pathwaydata())==0){
      cluster_non_pathway_plot("Gene Set Enrichment Analysis(GSEA)")
    }else{
      enrichment_gsea_plot("Bar Plot",enrich_cluster_gsea_pathwaydata(),
                           enrich_cluster_gsea_path_category_sum(),input$enrich_cluster_gsea_dataset)
    }
  })
  
  enrich_cluster_gsea_result_dotplot<-eventReactive(input$enrich_cluster_gsea_update,{
    if(nrow(enrich_cluster_gsea_pathwaydata())==0){
      cluster_non_pathway_plot("Gene Set Enrichment Analysis(GSEA)")
    }else{
      enrichment_gsea_plot("Dot plot",enrich_cluster_gsea_pathwaydata(),
                           enrich_cluster_gsea_path_category_sum(),input$enrich_cluster_gsea_dataset)
    }
  })
  
  enrich_cluster_gsea_result_enrichmentmap<-eventReactive(input$enrich_cluster_gsea_update,{
    if(is.numeric(enrich_cluster_gsea_path_category_sum())){ #top20
      if(nrow(enrich_cluster_gsea_pathwaydata())==0){
        cluster_non_pathway_plot("Gene Set Enrichment Analysis(GSEA)")
      }else if(nrow(enrich_cluster_gsea_pathwaydata())==1){
        cluster_one_pathway_plot("Gene Set Enrichment Analysis(GSEA)")
      }else{
        enrichment_gsea_plot("Enrichment Map",enrich_cluster_gsea_pathwaydata(),
                             enrich_cluster_gsea_path_category_sum(),input$enrich_cluster_gsea_dataset)
      }
    }else{ #customize
      if(length(enrich_cluster_gsea_path_category_sum())==1){
        cluster_one_pathway_plot("Gene Set Enrichment Analysis(GSEA)")
      }else{
        enrichment_gsea_plot("Enrichment Map",enrich_cluster_gsea_pathwaydata(),
                             enrich_cluster_gsea_path_category_sum(),input$enrich_cluster_gsea_dataset)
      }
    }
    
  })
  
  enrich_cluster_gsea_result_ridgeline<-eventReactive(input$enrich_cluster_gsea_update,{
    if(nrow(enrich_cluster_gsea_pathwaydata())==0){
      cluster_non_pathway_plot("Gene Set Enrichment Analysis(GSEA)")
    }else{
      enrichment_gsea_plot("Ridgeline plot",enrich_cluster_gsea_pathwaydata(),
                           enrich_cluster_gsea_path_category_sum(),input$enrich_cluster_gsea_dataset)
    }
  })
  
  enrich_cluster_gsea_result_data<-eventReactive(input$enrich_cluster_gsea_update,{
    cluster_show_gsea_data(enrich_cluster_gsea_pathwaydata(),input$enrich_cluster_gsea_pathwayset)
  })
  
  shinyjs::hide(id="enrich_cluster_gsea_result_smu")
  observeEvent(input$enrich_cluster_gsea_update, {
    shinyjs::show(id="enrich_cluster_gsea_result_smu")
    
    output$enrich_cluster_gsea_result_barplot_show<-renderPlot({
      enrich_cluster_gsea_result_barplot()
    },height = 800,width = 700)
    
    output$enrich_cluster_gsea_result_dotplot_show<-renderPlot({
      enrich_cluster_gsea_result_dotplot()
    },height = 800,width =700)
    
    output$enrich_cluster_gsea_result_enrichmentmap_show<-renderPlot({
      enrich_cluster_gsea_result_enrichmentmap()
    })
    
    output$enrich_cluster_gsea_result_ridgeline_show<-renderPlot({
      enrich_cluster_gsea_result_ridgeline()
    },height = 800,width =700)
    
    output$enrich_cluster_gsea_result_gsea_data_show<-renderDT({
      DT::datatable(enrich_cluster_gsea_result_data(),
                    caption="Table: gseaResult for the selected pathwayset",
                    selection = list(mode = "single", selected = c(1), target = 'row'),
                    extensions=c('Responsive','FixedColumns'),
                    options = list(
                      dom = 'ftipr',
                      pageLength = 3,
                      responsive = TRUE,
                      fixedColumns = list(leftColumns = c(1,2,3)),
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                        "}")
                      
                    ))
      
    })
    
    
  })
  
  enrich_cluster_gsea_result_gseaplot<-eventReactive(input$enrich_cluster_gsea_result_gsea_data_show_rows_selected,{
    s = input$enrich_cluster_gsea_result_gsea_data_show_rows_selected
    sdata<-enrich_cluster_gsea_pathwaydata()
    if (length(s)){
      gseaplot_fun(s,sdata,input$enrich_cluster_gsea_dataset)
    }
  })
  
  output$enrich_cluster_gsea_result_gseaplot_show<-renderPlot({
    enrich_cluster_gsea_result_gseaplot()
  })
  
  #observe
  observeEvent(input$enrich_cluster_gsea_dataset,{
    if (input$enrich_cluster_gsea_dataset=="") {
      showFeedbackDanger(
        inputId = "enrich_cluster_gsea_dataset",
        text = "Please select a dataset!",
      )
    } else {
      hideFeedback("enrich_cluster_gsea_dataset")
    }
    
  })
  
  observeEvent(input$enrich_cluster_gsea_path_category,{
    if (is.null(input$enrich_cluster_gsea_path_category)) {
      showFeedbackDanger(
        inputId = "enrich_cluster_gsea_path_category",
        text = "Please select one pathway at least!",
      )
    } else if(length(input$enrich_cluster_gsea_path_category)==20){
      showFeedbackSuccess(
        inputId = "enrich_cluster_gsea_path_category",
        text = "The maximum is 20!",
      )
    }else {
      hideFeedback("enrich_cluster_gsea_path_category")
    }
    
  })
  
  enrich_cluster_gsea_update_requirement<-reactive({
    if(input$enrich_cluster_gsea_path_num=="Top20"){
      input$enrich_cluster_gsea_dataset!=""
    }else if(input$enrich_cluster_gsea_path_num=="Customize"){
      (input$enrich_cluster_gsea_dataset!="" &length(input$enrich_cluster_gsea_path_category)>0)
    }
  })
  
  observe({
    toggleState(id ="enrich_cluster_gsea_update",
                condition =enrich_cluster_gsea_update_requirement())
  })
  
  ##download

  output$enrich_cluster_gsea_fulldata_download_csv<-downloadHandler(
    filename = function(){
      paste("ClusterProfiler_GSEA_",input$enrich_cluster_gsea_dataset,
            "_",input$enrich_cluster_gsea_pathwayset,".csv",sep = "")
    },
    content = function(file){
      sep<-","
      write.table(enrich_cluster_gsea_result_data(),file,sep=sep,row.names =FALSE)
    }
  )
  
  output$enrich_cluster_gsea_fulldata_download_txt<-downloadHandler(
    filename = function(){
      paste("ClusterProfiler_GSEA_",input$enrich_cluster_gsea_dataset,
            "_",input$enrich_cluster_gsea_pathwayset,".txt",sep = "")
    },
    content = function(file){
      sep<-" "
      write.table(enrich_cluster_gsea_result_data(),file,sep=sep,row.names = FALSE)
    }
  )
  
  output$enrich_cluster_gsea_gseaplot_download_svg<- downloadHandler(
    filename = function(){
      paste("GSEA_gseaplot_",input$enrich_cluster_gsea_dataset,".svg",sep="")
    },
    content = function(file){
      svg(file,height=5, width=5)
      print(enrich_cluster_gsea_result_gseaplot())
      dev.off()
    }
  )
  
  output$enrich_cluster_gsea_gseaplot_download_pdf<- downloadHandler(
    filename = function(){
      paste("GSEA_gseaplot_",input$enrich_cluster_gsea_dataset,".pdf",sep="")
    },
    content = function(file){
      pdf(file,height=5, width=5)
      print(enrich_cluster_gsea_result_gseaplot())
      dev.off()
    }
  )
  
  output$enrich_cluster_gsea_gseaplot_download_png<- downloadHandler(
    filename = function(){
      paste("GSEA_gseaplot_",input$enrich_cluster_gsea_dataset,".png",sep="")
    },
    content = function(file){
      png(file,width = 400, height = 400, units = "px")
      print(enrich_cluster_gsea_result_gseaplot())
      dev.off()
    }
  )
  
  output$enrich_cluster_gsea_barplot_download_svg<- downloadHandler(
    filename = function(){
      paste("GSEA_Barplot_",input$enrich_cluster_gsea_dataset,".svg",sep="")
    },
    content = function(file){
      svg(file,height=10, width=10)
      print(enrich_cluster_gsea_result_barplot())
      dev.off()
    }
  )
  
  output$enrich_cluster_gsea_barplot_download_pdf<- downloadHandler(
    filename = function(){
      paste("GSEA_Barplot_",input$enrich_cluster_gsea_dataset,".pdf",sep="")
    },
    content = function(file){
      pdf(file,height=10, width=10)
      print(enrich_cluster_gsea_result_barplot())
      dev.off()
    }
  )
  
  output$enrich_cluster_gsea_barplot_download_png<- downloadHandler(
    filename = function(){
      paste("GSEA_Barplot_",input$enrich_cluster_gsea_dataset,".png",sep="")
    },
    content = function(file){
      png(file,width = 600, height = 600, units = "px")
      print(enrich_cluster_gsea_result_barplot())
      dev.off()
    }
  )
  
  output$enrich_cluster_gsea_dotplot_download_svg<- downloadHandler(
    filename = function(){
      paste("GSEA_Dotplot_",input$enrich_cluster_gsea_dataset,".svg",sep="")
    },
    content = function(file){
      svg(file,height=10, width=10)
      print(enrich_cluster_gsea_result_dotplot())
      dev.off()
    }
  )
  
  output$enrich_cluster_gsea_dotplot_download_pdf<- downloadHandler(
    filename = function(){
      paste("GSEA_Dotplot_",input$enrich_cluster_gsea_dataset,".pdf",sep="")
    },
    content = function(file){
      pdf(file,height=10, width=10)
      print(enrich_cluster_gsea_result_dotplot())
      dev.off()
    }
  )
  
  output$enrich_cluster_gsea_dotplot_download_png<- downloadHandler(
    filename = function(){
      paste("GSEA_Dotplot_",input$enrich_cluster_gsea_dataset,".png",sep="")
    },
    content = function(file){
      png(file,width = 600, height = 600, units = "px")
      print(enrich_cluster_gsea_result_dotplot())
      dev.off()
    }
  )
  
  output$enrich_cluster_gsea_enrichmentmap_download_svg<- downloadHandler(
    filename = function(){
      paste("GSEA_Enrichmentmap_",input$enrich_cluster_gsea_dataset,".svg",sep="")
    },
    content = function(file){
      svg(file)
      print(enrich_cluster_gsea_result_enrichmentmap())
      dev.off()
    }
  )
  
  output$enrich_cluster_gsea_enrichmentmap_download_pdf<- downloadHandler(
    filename = function(){
      paste("GSEA_Enrichmentmap_",input$enrich_cluster_gsea_dataset,".pdf",sep="")
    },
    content = function(file){
      pdf(file)
      print(enrich_cluster_gsea_result_enrichmentmap())
      dev.off()
    }
  )
  
  output$enrich_cluster_gsea_enrichmentmap_download_png<- downloadHandler(
    filename = function(){
      paste("GSEA_Enrichmentmap_",input$enrich_cluster_gsea_dataset,".png",sep="")
    },
    content = function(file){
      png(file)
      print(enrich_cluster_gsea_result_enrichmentmap())
      dev.off()
    }
  )
  
  output$enrich_cluster_gsea_ridgeline_download_svg<- downloadHandler(
    filename = function(){
      paste("GSEA_Ridgeline_",input$enrich_cluster_gsea_dataset,".svg",sep="")
    },
    content = function(file){
      svg(file,height=10, width=10)
      print(enrich_cluster_gsea_result_ridgeline())
      dev.off()
    }
  )
  
  output$enrich_cluster_gsea_ridgeline_download_pdf<- downloadHandler(
    filename = function(){
      paste("GSEA_Ridgeline_",input$enrich_cluster_gsea_dataset,".pdf",sep="")
    },
    content = function(file){
      pdf(file,height=10, width=10)
      print(enrich_cluster_gsea_result_ridgeline())
      dev.off()
    }
  )
  
  output$enrich_cluster_gsea_ridgeline_download_png<- downloadHandler(
    filename = function(){
      paste("GSEA_Ridgeline_",input$enrich_cluster_gsea_dataset,".png",sep="")
    },
    content = function(file){
      png(file,width = 600, height = 600, units = "px")
      print(enrich_cluster_gsea_result_ridgeline())
      dev.off()
    }
  )
  
  ##=================================10.pathview=============================
  
  enrich_pathview_pathway_data<-reactive({
    if(input$enrich_pathview_dataset!=""){
      load(paste("www/Data/5.Pathway_enrichment/pathviewdata/",input$enrich_pathview_dataset,".Rdata",sep=""))
      get("keggpathway")
    }
  })
  
  enrich_pathview_gene_data<-reactive({
    if(input$enrich_pathview_dataset!=""){
      load(paste("www/Data/5.Pathway_enrichment/pathviewdata/",input$enrich_pathview_dataset,".Rdata",sep=""))
      get("genedata")
    }
  })
  
  observeEvent(enrich_pathview_pathway_data(), {
    updateSelectizeInput(session,"enrich_pathview_pathway_name", 
                         choices =enrich_pathview_pathway_data()$Description,
                         server = TRUE
    )
  })
  
  enrich_pathview_pathway_id<-eventReactive(input$enrich_pathview_update,{
    enrich_pathview_pathway_data()$ID[which(enrich_pathview_pathway_data()$Description==input$enrich_pathview_pathway_name)]
  })
  
  shinyjs::hide(id="enrich_pathview_result_smu")
  observeEvent(input$enrich_pathview_update, {
    shinyjs::show(id="enrich_pathview_result_smu")

    output$enrich_pathview_result_plot_panel<-renderImage({
      list(
        src =paste0("www/Data/5.Pathway_enrichment/pathview_pic/",
                    paste(enrich_pathview_pathway_id(),input$enrich_pathview_dataset,"png",sep = ".")),
        contentType = "image/png",
        width=800
      )
      
    },deleteFile=FALSE)
    
    
  })
  
  #observe
  observeEvent(input$enrich_pathview_dataset,{
    if (input$enrich_pathview_dataset=="") {
      showFeedbackDanger(
        inputId = "enrich_pathview_dataset",
        text = "Please select a dataset!",
      )
    } else {
      hideFeedback("enrich_pathview_dataset")
    }
    
  })
  
  observeEvent(input$enrich_pathview_pathway_name,{
    if (input$enrich_pathview_pathway_name=="") {
      showFeedbackDanger(
        inputId = "enrich_pathview_pathway_name",
        text = "Please select a pathway!",
      )
    } else {
      hideFeedback("enrich_pathview_pathway_name")
    }
    
  })
  
  observe({
    toggleState(id ="enrich_pathview_update",
                condition =(input$enrich_pathview_dataset!=""&input$enrich_pathview_pathway_name!=""))
  })
  
  ##download
  output$enrich_pathview_plot_download_png<-downloadHandler(
    filename <- function() {
      paste(enrich_pathview_pathway_id(),input$enrich_pathview_dataset,"png",sep = ".")
    },
    content <- function(file) {
      file.copy(paste0("www/Data/5.Pathway_enrichment/pathview_pic/",
                       paste(enrich_pathview_pathway_id(),input$enrich_pathview_dataset,"png",sep = ".")), file)
      
    })
  
  ##=================================11.ssgsea=============================
  
  enrich_ssgsea_basic_data<-eventReactive(input$enrich_ssgsea_dataset,{
    if(input$enrich_ssgsea_dataset!=""){
      load(paste("www/Data/0.Basis_data/ssgsea_result/",input$enrich_ssgsea_dataset,".Rdata",sep=""))
      get("ssgsea_matrix")
    }
  })
  
  observeEvent(enrich_ssgsea_basic_data(),{
    updateSelectizeInput(session,"enrich_ssgsea_pathway", 
                         choices =rownames(enrich_ssgsea_basic_data()),
                         selected = rownames(enrich_ssgsea_basic_data())[1:7],
                         server = TRUE
    )
  })
  
  enrich_ssgsea_result_plot<-eventReactive(input$enrich_ssgsea_update,{
    enrich_ssgsea_heatmap(input$enrich_ssgsea_dataset,input$enrich_ssgsea_pathway,
                          input$enrich_ssgsea_heatmap_color)
  })
  
  enrich_ssgsea_result_data<-eventReactive(input$enrich_ssgsea_update,{
    load(paste("www/Data/5.Pathway_enrichment/ssgsea_wilcox/",input$enrich_ssgsea_dataset,".Rdata",sep=""))
    data_b<-get("ssgsea_wilcox")
    data_b<-format(data_b[,c(1,2)], scientific = TRUE,digits = 4)
    load(paste("www/Data/0.Basis_data/ssgsea_result/",input$enrich_ssgsea_dataset,".Rdata",sep=""))
    data_a<-get("ssgsea_matrix")
    data_a<-round(data_a,digits = 3)
    data_merge<-merge(data_a,data_b,by="row.names")
    colnames(data_merge)[1]<-"pathway"
    return(data_merge)
  })
  
  shinyjs::hide(id="enrich_ssgsea_result_smu")
  
  observeEvent(input$enrich_ssgsea_update, {
    shinyjs::show(id="enrich_ssgsea_result_smu")

    output$enrich_ssgsea_result_plot_panel<-renderPlot({
      enrich_ssgsea_result_plot()
    },height = 700,width = 900)
    
    output$enrich_ssgsea_result_data_panel<-renderDT({
      DT::datatable(enrich_ssgsea_result_data(),
                    caption="Table: ssgsea result.",
                    rownames = FALSE,
                    extensions=c('Responsive','FixedColumns'),
                    options = list(
                      dom = 'ftipr',
                      pageLength = 10,
                      responsive = TRUE,
                      fixedColumns = list(leftColumns = c(1:6)),
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                        "}")
                      
                    ))
      
    })
    
  })
  
  #observe
  observeEvent(input$enrich_ssgsea_dataset,{
    if (input$enrich_ssgsea_dataset=="") {
      showFeedbackDanger(
        inputId = "enrich_ssgsea_dataset",
        text = "Please select a dataset!",
      )
    } else {
      hideFeedback("enrich_ssgsea_dataset")
    }
    
  })
  
  observeEvent(input$enrich_ssgsea_pathway,{
    if (is.null(input$enrich_ssgsea_pathway)) {
      showFeedbackDanger(
        inputId = "enrich_ssgsea_pathway",
        text = "Please select one pathway at least!",
      )
    } else if(length(input$enrich_ssgsea_pathway)==20){
      showFeedbackSuccess(
        inputId = "enrich_ssgsea_pathway",
        text = "The maximum is 20!",
      )
    }else {
      hideFeedback("enrich_ssgsea_pathway")
    }
    
  })
  
  observe({
    toggleState(id ="enrich_ssgsea_update",
                condition =(input$enrich_ssgsea_dataset!=""&length(input$enrich_ssgsea_pathway)>0))
  })
  
  ##download
  output$enrich_ssgsea_plot_download_svg<- downloadHandler(
    filename = function(){
      paste("ssGSEA_Heatmap_",input$enrich_ssgsea_dataset,".svg",sep="")
    },
    content = function(file){
      svg(file,height=10, width=20)
      print(enrich_ssgsea_result_plot())
      dev.off()
    }
  )
  
  output$enrich_ssgsea_plot_download_pdf<- downloadHandler(
    filename = function(){
      paste("ssGSEA_Heatmap_",input$enrich_ssgsea_dataset,".pdf",sep="")
    },
    content = function(file){
      pdf(file,height=10, width=20)
      print(enrich_ssgsea_result_plot())
      dev.off()
    }
  )
  
  output$enrich_ssgsea_plot_download_png<- downloadHandler(
    filename = function(){
      paste("ssGSEA_Heatmap_",input$enrich_ssgsea_dataset,".png",sep="")
    },
    content = function(file){
      png(file,width = 800, height = 650, units = "px")
      print(enrich_ssgsea_result_plot())
      dev.off()
    }
  )
  
  output$enrich_ssgsea_fulldata_download_csv<-downloadHandler(
    filename = function(){
      paste("enrich_ssgsea_",input$enrich_ssgsea_dataset,".csv",sep = "")
    },
    content = function(file){
      sep<-","
      write.table(enrich_ssgsea_result_data(),file,sep=sep,row.names =FALSE)
    }
  )
  
  output$enrich_ssgsea_fulldata_download_txt<-downloadHandler(
    filename = function(){
      paste("enrich_ssgsea_",input$enrich_ssgsea_dataset,".txt",sep = "")
    },
    content = function(file){
      sep<-" "
      write.table(enrich_ssgsea_result_data(),file,sep=sep,row.names =FALSE)
    }
  )
  
  ##=================================5.corr Scatter plot=============================
  corrScatter_status_choices<-reactive({
    if(input$corrScatter_dataset%in%GSEdatalist$name[which(GSEdatalist$negativeonly==TRUE)]){
      c("HPV_Negative")
    }else if(input$corrScatter_dataset%in%GSEdatalist$name[which(GSEdatalist$positiveonly==TRUE)]){
      c("HPV_Positive")
    }else if(input$corrScatter_dataset%in%GSEdatalist$name[which(GSEdatalist$DEoverall==TRUE)]){
      c("All","HPV_Positive","HPV_Negative")
    }
  })
  
  observeEvent(input$corrScatter_dataset,{
    updateRadioButtons(session,inputId="corrScatter_status_single",
                       choices =corrScatter_status_choices(),inline = TRUE)
  })
  
  
  corrScatter_xaxis_type_choices<-reactive({
    if(input$corrScatter_dataset%in%GSEdatalist$name[which(GSEdatalist$imm_infli==TRUE)]){
      c("Gene","Pathway","Immune Cell")
    }else{
      c("Gene","Pathway")
    }
  })
  
  observeEvent(input$corrScatter_dataset,{
    updateRadioButtons(session,inputId="corrScatter_xaxis_type",
                       choices =corrScatter_xaxis_type_choices(),inline = TRUE)
  })
  
  corrScatter_yaxis_type_choices<-reactive({
    if(input$corrScatter_dataset%in%GSEdatalist$name[which(GSEdatalist$imm_infli==TRUE)]){
      c("Gene","Pathway","Immune Cell")
    }else{
      c("Gene","Pathway")
    }
  })
  
  observeEvent(input$corrScatter_dataset,{
    updateRadioButtons(session,inputId="corrScatter_yaxis_type",
                       choices =corrScatter_yaxis_type_choices(),inline = TRUE)
  })
  
  ##load data
  corrScatter_gene_choices<-eventReactive(input$corrScatter_dataset,{
    if(input$corrScatter_dataset!=""){
      load(paste("www/Data/0.Basis_data/expression_matrix/",input$corrScatter_dataset,".Rdata",sep=""))
      rownames(get("exprSet"))
    }
  })
  
  
  corrScatter_path_choices<-eventReactive(input$corrScatter_dataset,{
    if(input$corrScatter_dataset!=""){
      load(paste("www/Data/0.Basis_data/ssgsea_result/",input$corrScatter_dataset,".Rdata",sep=""))
      rownames(get("ssgsea_matrix"))
    }
  })
  
  corrScatter_immunecell_choices<-eventReactive(input$corrScatter_dataset,{
    if(input$corrScatter_dataset!="" & input$corrScatter_dataset%in%GSEdatalist$name[which(GSEdatalist$imm_infli==TRUE)]){
      imminfliNameList$immunecells[which(imminfliNameList$gsename==input$corrScatter_dataset)]
    }
  })
  
  
  corrScatter_xaxis_item_option<-reactive({
    corrScatter_xaxis_item_option <- switch (input$corrScatter_xaxis_type,
                                             "Gene" =corrScatter_gene_choices() ,
                                             "Pathway"=corrScatter_path_choices(),
                                             "Immune Cell"=corrScatter_immunecell_choices() )
    
  })
  
  observeEvent(corrScatter_xaxis_item_option(),{ 
    updateSelectizeInput (session,"corrScatter_xaxis_item",
                          choices =corrScatter_xaxis_item_option(),
                          selected = corrScatter_xaxis_item_option()[30],
                          server = TRUE) 
  })
  
  corrScatter_yaxis_item_option<-reactive({
    corrScatter_xaxis_item_option <- switch (input$corrScatter_yaxis_type,
                                             "Gene" =corrScatter_gene_choices() ,
                                             "Pathway"=corrScatter_path_choices(),
                                             "Immune Cell"=corrScatter_immunecell_choices() )
    
  })
  
  observeEvent(corrScatter_yaxis_item_option(),{ 
    updateSelectizeInput (session,"corrScatter_yaxis_item",
                          choices =corrScatter_yaxis_item_option(),
                          selected = corrScatter_yaxis_item_option()[10],
                          server = TRUE) 
  })
  
  ##plot processing
  corrScatter_update_requirement<-reactive({
    (input$corrScatter_xaxis_item!="" & input$corrScatter_yaxis_item!="")
  })
  
  observe({
    toggleState(id ="corrScatter_update",
                condition =corrScatter_update_requirement() )
  })
  
  
  corrScatter_color_option<-reactive({
    switch (input$corrScatter_status_single,
            "All"=c(input$corrScatter_status_color_n,input$corrScatter_status_color_p),
            "HPV_Positive"=c(input$corrScatter_status_color_single),
            "HPV_Negative"=c(input$corrScatter_status_color_single)
    )
  })
  
  
  corrScatter_xyaxis_result<- eventReactive(input$corrScatter_update,{
    expression_correlation_scatter(input$corrScatter_dataset,input$corrScatter_status_single,
                                   input$corrScatter_method_single,corrScatter_color_option(),
                                   input$corrScatter_xaxis_type,input$corrScatter_xaxis_item,
                                   input$corrScatter_yaxis_type,input$corrScatter_yaxis_item)
    
  })
  

  shinyjs::hide(id ="corrScatter_result_smu")
  observeEvent(input$corrScatter_update, {
    shinyjs::show(id ="corrScatter_result_smu")
  })
  
  output$corrScatter_result_plot_panel<-renderPlot({
    corrScatter_xyaxis_result()
  })
  
  ##observer
  observeEvent(input$corrScatter_xaxis_item,{
    if (input$corrScatter_xaxis_item=="") {
      showFeedbackDanger(
        inputId = "corrScatter_xaxis_item",
        text = "Please select an object!",
      )
    } else {
      hideFeedback("corrScatter_xaxis_item")
    }
  })
  
  observeEvent(input$corrScatter_yaxis_item,{
    if (input$corrScatter_yaxis_item=="") {
      showFeedbackDanger(
        inputId = "corrScatter_yaxis_item",
        text = "Please select an object!",
      )
    } else {
      hideFeedback("corrScatter_yaxis_item")
    }
  })
  
  ##download
  output$corrScatter_plot_download_svg<- downloadHandler(
    filename = function(){
      paste("Scatter_",
            paste(input$corrScatter_xaxis_item,input$corrScatter_yaxis_item,sep = "_"),
            ".svg",sep="")
    },
    content = function(file){
      svg(file,height=8, width=8)
      print(corrScatter_xyaxis_result())
      dev.off()
    }
  )
  
  output$corrScatter_plot_download_pdf<- downloadHandler(
    filename = function(){
      paste("Scatter_",
            paste(input$corrScatter_xaxis_item,input$corrScatter_yaxis_item,sep = "_"),
            ".pdf",sep="")
    },
    content = function(file){
      pdf(file,height=8, width=8)
      print(corrScatter_xyaxis_result())
      dev.off()
    }
  )
  
  output$corrScatter_plot_download_png<- downloadHandler(
    filename = function(){
      paste("Scatter_",
            paste(input$corrScatter_xaxis_item,input$corrScatter_yaxis_item,sep = "_"),
            ".png",sep="")
    },
    content = function(file){
      png(file,width = 700, height = 600, units = "px")
      print(corrScatter_xyaxis_result())
      dev.off()
    }
  )
  
  
  
  
  ##=================================6.Multigenes=============================
  ##===heatmap
  output$multi_cor_heat_result_plot_panel<-renderUI({
    div(
      fluidRow(
        column(width = 8,
               dropdownButton(
                 inputId = "multi_cor_heatmap_introduction",
                 label = "Plot Introduction",
                 #icon = icon("question"),
                 status = "primary",
                 circle = FALSE,
                 width="500px",
                 corr_corrHeatmap_text)),
        column(width = 4,
               dropdownButton(
                 inputId = "multi_cor_heatmap_download",
                 label = "Download",
                 status = "primary",
                 icon=icon("download"),
                 circle = FALSE,
                 width="50px",
                 downloadButton(outputId = "multi_cor_heatmap_download_pdf",
                                label = "PDF"),
                 downloadButton(outputId = "multi_cor_heatmap_download_svg",
                                label = "SVG"),
                 downloadButton(outputId = "multi_cor_heatmap_download_png",
                                label = "PNG")))
        
      ),
      plotOutput("multi_cor_heatmap_result_plot_show")%>% withSpinner(color="#6c6689")
    )
    
  })
  
  multi_cor_heat_status_choices<-reactive({
    if(input$multi_cor_heat_dataset%in%GSEdatalist$name[which(GSEdatalist$negativeonly==TRUE)]){
      c("HPV_Negative")
    }else if(input$multi_cor_heat_dataset%in%GSEdatalist$name[which(GSEdatalist$positiveonly==TRUE)]){
      c("HPV_Positive")
    }else if(input$multi_cor_heat_dataset%in%GSEdatalist$name[which(GSEdatalist$DEoverall==TRUE)]){
      c("All","HPV_Positive","HPV_Negative")
    }
  })
  
  observeEvent(input$multi_cor_heat_dataset,{
    updatePrettyRadioButtons(session,inputId="multi_cor_heat_status", 
                             choices =multi_cor_heat_status_choices(),inline = TRUE)
  })
  
  multi_cor_heat_genes_choices<-eventReactive(input$multi_cor_heat_dataset,{
    if(input$multi_cor_heat_dataset!=""){
      load(paste("www/Data/0.Basis_data/expression_matrix/",input$multi_cor_heat_dataset,".Rdata",sep=""))
      rownames(get("exprSet"))
    }
  })
  
  observeEvent(input$multi_cor_heat_dataset, {
    updateSelectizeInput(session,"multi_cor_heat_genes", 
                         choices =multi_cor_heat_genes_choices(),
                         selected = multi_cor_heat_genes_choices()[115:123],
                         server = TRUE
    )
  })
  
  
  multi_cor_heatmap_result_plot<-eventReactive(input$multi_cor_heat_update,{
    multi_cor_plotfun("corrHeatmap",input$multi_cor_heat_dataset,
                      input$multi_cor_heat_status,list(input$multi_cor_heat_genes),
                      input$multi_cor_heat_method,miancolor=input$multi_cor_heatmap_color)
    
  })
  
  shinyjs::hide(id="multi_cor_heat_result_smu")
  observeEvent(input$multi_cor_heat_update, {
    shinyjs::show(id="multi_cor_heat_result_smu")

    output$multi_cor_heatmap_result_plot_show<-renderPlot({
      multi_cor_heatmap_result_plot()
    })
    
  })
  
  #observe
  observeEvent(input$multi_cor_heat_dataset,{
    if (input$multi_cor_heat_dataset=="") {
      showFeedbackDanger(
        inputId = "multi_cor_heat_dataset",
        text = "Please select a dataset!",
      )
    } else {
      hideFeedback("multi_cor_heat_dataset")
    }
    
  })
  
  observeEvent(input$multi_cor_heat_genes,{
    if (length(input$multi_cor_heat_genes)<5) {
      showFeedbackDanger(
        inputId = "multi_cor_heat_genes",
        text = "Please select 5 genes at least!",
      )
    } else if(length(input$multi_cor_heat_genes)==15){
      showFeedbackSuccess(
        inputId = "multi_cor_heat_genes",
        text = "The maximum is 15!",
      )
    }else {
      hideFeedback("multi_cor_heat_genes")
    }
    
  })
  
  
  multi_cor_heat_update_update_requirement<-reactive({
    (input$multi_cor_heat_dataset!="" & length(input$multi_cor_heat_genes)>=5)
  })
  
  observe({
    toggleState(id ="multi_cor_heat_update",
                condition =multi_cor_heat_update_update_requirement())
  })
  
  ##download
  output$multi_cor_heatmap_download_svg<- downloadHandler(
    filename = function(){
      paste("corrHeatmap_",input$multi_cor_heat_dataset,".svg",sep="")
    },
    content = function(file){
      svg(file)
      multi_cor_plotfun("corrHeatmap",input$multi_cor_heat_dataset,
                        input$multi_cor_heat_status,list(input$multi_cor_heat_genes),
                        input$multi_cor_heat_method,miancolor=input$multi_cor_heatmap_color)
      dev.off()
    }
  )
  
  output$multi_cor_heatmap_download_pdf<- downloadHandler(
    filename = function(){
      paste("corrHeatmap_",input$multi_cor_heat_dataset,".pdf",sep="")
    },
    content = function(file){
      pdf(file)
      multi_cor_plotfun("corrHeatmap",input$multi_cor_heat_dataset,
                        input$multi_cor_heat_status,list(input$multi_cor_heat_genes),
                        input$multi_cor_heat_method,miancolor=input$multi_cor_heatmap_color)
      dev.off()
    }
  )
  
  output$multi_cor_heatmap_download_png<- downloadHandler(
    filename = function(){
      paste("corrHeatmap_",input$multi_cor_heat_dataset,".png",sep="")
    },
    content = function(file){
      png(file)
      multi_cor_plotfun("corrHeatmap",input$multi_cor_heat_dataset,
                        input$multi_cor_heat_status,list(input$multi_cor_heat_genes),
                        input$multi_cor_heat_method,miancolor=input$multi_cor_heatmap_color)
      dev.off()
    }
  )
  
  ##===bubble
  output$multi_cor_bub_result_plot_panel<-renderUI({
    div(
      fluidRow(
        column(width = 8,
               dropdownButton(
                 inputId = "multi_cor_bubble_introduction",
                 label = "Plot Introduction",
                 #icon = icon("question"),
                 status = "primary",
                 circle = FALSE,
                 width="500px",
                 corr_corrBubble_text)),
        column(width = 4,
               dropdownButton(
                 inputId = "multi_cor_bubble_download",
                 label = "Download",
                 status = "primary",
                 icon=icon("download"),
                 circle = FALSE,
                 width="50px",
                 downloadButton(outputId = "multi_cor_bubble_download_pdf",
                                label = "PDF"),
                 downloadButton(outputId = "multi_cor_bubble_download_svg",
                                label = "SVG"),
                 downloadButton(outputId = "multi_cor_bubble_download_png",
                                label = "PNG")))
        
      ),
      plotOutput("multi_cor_bubble_result_plot_show")%>% withSpinner(color="#6c6689")
    )
    
  })
  
  
  
  multi_cor_bub_status_choices<-reactive({
    if(input$multi_cor_bub_dataset%in%GSEdatalist$name[which(GSEdatalist$negativeonly==TRUE)]){
      c("HPV_Negative")
    }else if(input$multi_cor_bub_dataset%in%GSEdatalist$name[which(GSEdatalist$positiveonly==TRUE)]){
      c("HPV_Positive")
    }else if(input$multi_cor_bub_dataset%in%GSEdatalist$name[which(GSEdatalist$DEoverall==TRUE)]){
      c("All","HPV_Positive","HPV_Negative")
    }
  })
  
  observeEvent(input$multi_cor_bub_dataset,{
    updatePrettyRadioButtons(session,inputId="multi_cor_bub_status", 
                             choices =multi_cor_bub_status_choices(),inline = TRUE)
  })
  
  multi_cor_bub_genes_choices<-eventReactive(input$multi_cor_bub_dataset,{
    if(input$multi_cor_bub_dataset!=""){
      load(paste("www/Data/0.Basis_data/expression_matrix/",input$multi_cor_bub_dataset,".Rdata",sep=""))
      rownames(get("exprSet"))
    }
  })
  
  observeEvent(input$multi_cor_bub_dataset, {
    updateSelectizeInput(session,"multi_cor_bub_gene1", 
                         choices =multi_cor_bub_genes_choices(),
                         selected = multi_cor_bub_genes_choices()[115:120],
                         server = TRUE
    )
    
  })
  
  
  bub_multi_available_gene<-reactive({
    if(is.null(input$multi_cor_bub_gene1)){
      bub_multi_available_gene<-multi_cor_bub_genes_choices()
      return(bub_multi_available_gene)
    }else{
      bub_multi_available_gene<-multi_cor_bub_genes_choices()
      bub_multi_available_gene<-bub_multi_available_gene[-which(bub_multi_available_gene%in%input$multi_cor_bub_gene1)]
      return(bub_multi_available_gene)
    }
  })
  
  observeEvent(bub_multi_available_gene(),{
    updateSelectizeInput(session,"multi_cor_bub_gene2", 
                         choices =bub_multi_available_gene(),
                         selected = bub_multi_available_gene()[100:105],
                         server = TRUE
    )
  })
  
  
  multi_cor_bubble_result_plot<-eventReactive(input$multi_cor_bub_update,{
    multi_cor_plotfun("corrBubble",input$multi_cor_bub_dataset,
                      input$multi_cor_bub_status,
                      list(input$multi_cor_bub_gene1,input$multi_cor_bub_gene2),
                      input$multi_cor_bub_method)
    
  })
  
  shinyjs::hide(id="multi_cor_bub_result_smu")
  observeEvent(input$multi_cor_bub_update, {
    shinyjs::show(id="multi_cor_bub_result_smu")

    output$multi_cor_bubble_result_plot_show<-renderPlot({
      multi_cor_bubble_result_plot()
    })
    
  })
  
  #observe
  observeEvent(input$multi_cor_bub_dataset,{
    if (input$multi_cor_bub_dataset=="") {
      showFeedbackDanger(
        inputId = "multi_cor_bub_dataset",
        text = "Please select a dataset!",
      )
    } else {
      hideFeedback("multi_cor_bub_dataset")
    }
    
  })
  
  observeEvent(input$multi_cor_bub_gene1,{
    if (length(input$multi_cor_bub_gene1)<5) {
      showFeedbackDanger(
        inputId = "multi_cor_bub_gene1",
        text = "Please select 5 genes at least!",
      )
    } else if(length(input$multi_cor_bub_gene1)==15){
      showFeedbackSuccess(
        inputId = "multi_cor_bub_gene1",
        text = "The maximum is 15!",
      )
    }else {
      hideFeedback("multi_cor_bub_gene1")
    }
    
  })
  
  observeEvent(input$multi_cor_bub_gene2,{
    if (length(input$multi_cor_bub_gene2)<5) {
      showFeedbackDanger(
        inputId = "multi_cor_bub_gene2",
        text = "Please select 5 genes at least!",
      )
    } else if(length(input$multi_cor_bub_gene2)==15){
      showFeedbackSuccess(
        inputId = "multi_cor_bub_gene2",
        text = "The maximum is 15!",
      )
    }else {
      hideFeedback("multi_cor_bub_gene2")
    }
    
  })
  
  
  multi_cor_bub_update_update_requirement<-reactive({
    (input$multi_cor_bub_dataset!="" & length(input$multi_cor_bub_gene1)>=5 & length(input$multi_cor_bub_gene2)>=5)
  })
  
  observe({
    toggleState(id ="multi_cor_bub_update",
                condition =multi_cor_bub_update_update_requirement())
  })
  
  ##download
  output$multi_cor_bubble_download_svg<- downloadHandler(
    filename = function(){
      paste("corrBubble_",input$multi_cor_bub_dataset,".svg",sep="")
    },
    content = function(file){
      svg(file)
      print(multi_cor_bubble_result_plot())
      dev.off()
    }
  )
  
  output$multi_cor_bubble_download_pdf<- downloadHandler(
    filename = function(){
      paste("corrBubble_",input$multi_cor_bub_dataset,".pdf",sep="")
    },
    content = function(file){
      pdf(file)
      print(multi_cor_bubble_result_plot())
      dev.off()
    }
  )
  
  output$multi_cor_bubble_download_png<- downloadHandler(
    filename = function(){
      paste("corrBubble_",input$multi_cor_bub_dataset,".png",sep="")
    },
    content = function(file){
      png(file)
      print(multi_cor_bubble_result_plot())
      dev.off()
    }
  )
  
  
  ##=================================4.Coexpression=============================
  coexp_gene_single_choices<-eventReactive(input$coexp_selective_species,{
    coexp_gene_single_choices<-switch (input$coexp_selective_species,
                                       "Homo sapiens" =c("HPV_Positive","HPV_Negative","All"),
                                       "Mus musculus" =c("HPV_Positive","All"))
  })
  
  observeEvent(input$coexp_selective_species,{ 
    updateRadioButtons (session,"coexp_status_single",
                        choices =coexp_gene_single_choices(),selected ="All",
                        inline = TRUE) 
  })
  
  coexp_genes_sum <- eventReactive(input$coexp_selective_species,{
    coexp_genes_sum <- switch (input$coexp_selective_species,
                               "Homo sapiens" = get("kk_human"),
                               "Mus musculus" = get("kk_mouse"))
  })
  
  observeEvent(input$coexp_selective_species,{ 
    updateSelectizeInput (session,"coexp_gene_single",
                          choices =coexp_genes_sum(),
                          selected = coexp_genes_sum()[20],
                          server = TRUE) 
  })
  
  
  
  coexp_finding_specify_genes <- eventReactive(input$coexp_gene_single,{
    avaiable <- coexp_genes_sum()[-which(coexp_genes_sum() %in% input$coexp_gene_single)] 
  })
  
  
  observeEvent(input$coexp_gene_single,{ 
    updateSelectizeInput (session,"coexp_specific_genes",
                          choices =coexp_finding_specify_genes(),
                          selected = coexp_finding_specify_genes()[c(107:110,990:993,1981:1984,2970:2973)], 
                          server = TRUE) 
  })
  
  
  ####调取基因
  coexp_choosemaingene <- reactive({
    coexp_genes_sum <- coexp_genes_sum()
    coexp_choosefilenumber <- which(coexp_genes_sum==input$coexp_gene_single)
    
    if(input$coexp_selective_species=="Homo sapiens"){
      load(file=paste0("www/Data/3.Correlation/Homo_sapiens/",ceiling(coexp_choosefilenumber/502),".Rdata"))
    }
    else{
      load(file=paste0("www/Data/3.Correlation/Mus_musculus/",ceiling(coexp_choosefilenumber/608),".Rdata"))
    }
    coexp_geneset <- get("geneset")
    coexp_choosemaingene <- coexp_geneset[which(rownames(coexp_geneset)==input$coexp_gene_single),]
  })
  
  coexp_chooseothergene <- reactive({
    coexp_chooseother <- input$coexp_specific_genes
    coexp_genes_sum <- coexp_genes_sum()
    coexp_chooseother_dataframe <- list()
    temporlist <- list()
    if(input$coexp_selective_species=="Homo sapiens"){
      yyy<- unique(sapply(coexp_chooseother,function(xx){
        names <- which(coexp_genes_sum==xx)
        calucu <-ceiling(names/502)}))
      temporlist <- list()
      temporlist <- lapply(yyy,function(xxx){
        load(paste0("www/Data/3.Correlation/Homo_sapiens/",xxx,".Rdata"),temp_env<- new.env())  
        temporlist[[xxx]] <- temp_env
      })
      temporlist_final <- list()
      for(i in 1:length(temporlist)){
        temporlist_final[[i]] <- temporlist[[i]][["geneset"]]
      }
      coexp_chooseother_dataframe <- Reduce(function(a,b){
        rbind(a,b)
      },temporlist_final)
    }else{
      yyy<- unique(sapply(coexp_chooseother,function(xx){
        names <- which(coexp_genes_sum==xx)
        calucu <-ceiling(names/608)}))
      temporlist <- list()
      temporlist <- lapply(yyy,function(xxx){
        load(paste0("www/Data/3.Correlation/Mus_musculus/",xxx,".Rdata"),temp_env<- new.env())  
        temporlist[[xxx]] <- temp_env
      })
      temporlist_final <- list()
      for(i in 1:length(temporlist)){
        temporlist_final[[i]] <- temporlist[[i]][["geneset"]]
      }
      coexp_chooseother_dataframe <- Reduce(function(a,b){
        rbind(a,b)
      },temporlist_final)
    }
    
    coexp_chooseothergene <- coexp_chooseother_dataframe[coexp_chooseother,]  
  })
  
  
  ##合并所有基因将其变成List
  coexp_mergegenecoex_final <- reactive({
    coexp_choosemaingene <- coexp_choosemaingene()
    coexp_chooseothergene <- coexp_chooseothergene()
    coexp_mergegenecoex <- merge(data.frame(t(coexp_choosemaingene)),data.frame(t(coexp_chooseothergene)),by="row.names")
    rownames(coexp_mergegenecoex) <- coexp_mergegenecoex$'Row.names'
    coexp_mergegenecoex <- subset(coexp_mergegenecoex,select=-c(Row.names))
    
    coexp_metadata <- switch (input$coexp_selective_species,
                              "Homo sapiens" = get(load("www/Data/3.Correlation/metadata_human.Rdata")),
                              "Mus musculus"=get(load("www/Data/3.Correlation/metadata_mouse.Rdata"))
    )
    coexp_mergelist <- merge(coexp_mergegenecoex,coexp_metadata,by="row.names")
    
    if(input$coexp_status_single=="HPV_Positive"){
      coexp_mergelist <- subset(coexp_mergelist,hpvstatus== "HPV_Positive")
      coexp_mergelist$GSE <- paste0(coexp_mergelist$GSE,"_","HPVPositive")
    }else if (input$coexp_status_single=="HPV_Negative"){
      coexp_mergelist <- subset(coexp_mergelist,hpvstatus== "HPV_Negative")
      coexp_mergelist$GSE <- paste0(coexp_mergelist$GSE,"_","HPVNegative")
    }else{
      coexp_mergelist_positive <- subset(coexp_mergelist,hpvstatus== "HPV_Positive")
      coexp_mergelist_positive$GSE <- paste0(coexp_mergelist_positive$GSE,"_","HPV_Positive")
      coexp_mergelist_negative <- subset(coexp_mergelist,hpvstatus== "HPV_Negative")
      coexp_mergelist_negative$GSE <- paste0(coexp_mergelist_negative$GSE,"_","HPV_Negative")
      coexp_mergelist <- rbind(coexp_mergelist_positive,coexp_mergelist_negative)
    }
    
    rownames(coexp_mergelist) <- coexp_mergelist$GSM
    coexp_mergelist <- subset(coexp_mergelist,select=-c(Row.names,hpvstatus,GSM))
    coexp_mergegenecoex_final <- 
      rlist::list.update(split(coexp_mergelist,coexp_mergelist$GSE),GSE=NULL)
    
    coexp_mergegenecoex_final<-
      coexp_mergegenecoex_final[!sapply(coexp_mergegenecoex_final, function(x) all(is.na(x)))]
    
    coexp_mergegenecoex_final<-
      coexp_mergegenecoex_final[!sapply(coexp_mergegenecoex_final, function(x) all(is.na(x[,1])))]
    
  })
  
  ##将dataframe变成list并且做相关性分析
  coexp_splitlist_funcion <- reactive({
    coexp_mergegenecoex_final <- coexp_mergegenecoex_final()
    coexp_splitlist_funcion <- switch (input$coexp_method_single,
                                       "Pearson" = lapply(coexp_mergegenecoex_final,
                                                          function(x){
                                                            psych::corr.test(x,method = "pearson",adjust="none")}),
                                       "Spearman"=lapply(coexp_mergegenecoex_final,
                                                         function(x){
                                                           psych::corr.test(x,method = "spearman",adjust="none")})
                                       
    )
    
    return(coexp_splitlist_funcion)
    
  })
  
  
  coexp_main_rdf_final <- reactive({
    coexp_splitlist_funcion<-coexp_splitlist_funcion()
    
    coexp_rlist <-lapply(sapply(coexp_splitlist_funcion,"[", "r"), data.frame)
    coexp_rdf_final <- data.frame(sapply(coexp_rlist,"[[", input$coexp_gene_single),
                                  row.names = names(coexp_rlist[[1]]))
    colnames(coexp_rdf_final) <- gsub('.r', '', 
                                      colnames(coexp_rdf_final))
    colnames(coexp_rdf_final) <- gsub('HPV_Positive', 'HPVPositive', 
                                      colnames(coexp_rdf_final))
    colnames(coexp_rdf_final) <- gsub('HPV_Negative', 'HPVNegative',
                                      colnames(coexp_rdf_final))
    #coexp_rdf_final<-coexp_rdf_final[,-which(apply(coexp_rdf_final,2,function(x) all(is.na(x))))]
    
    return(coexp_rdf_final)
    
  })
  
  coexp_main_pdf_final <-reactive({
    coexp_splitlist_funcion<-coexp_splitlist_funcion()
    
    coexp_plist <-lapply(sapply(coexp_splitlist_funcion,"[", "p"), data.frame)
    coexp_pdf_final <- data.frame(sapply(coexp_plist,"[[", input$coexp_gene_single),
                                  row.names = names(coexp_plist[[1]]))
    colnames(coexp_pdf_final) <- gsub('.p', '', 
                                      colnames(coexp_pdf_final))
    colnames(coexp_pdf_final) <- gsub('HPV_Positive', 'HPVPositive', 
                                      colnames(coexp_pdf_final))
    colnames(coexp_pdf_final) <- gsub('HPV_Negative', 'HPVNegative',
                                      colnames(coexp_pdf_final))
    #coexp_pdf_final<-coexp_pdf_final[,-which(apply(coexp_pdf_final,2,function(x) all(is.na(x))))]
    
    return(coexp_pdf_final)
    
  })
  
  
  coexp_HeatMap<- eventReactive(input$coexp_update,{
    coexp_heatmap_fun(main_rdata=coexp_main_rdf_final(),main_pdate=coexp_main_pdf_final(),
                      cor_method=input$coexp_method_single,
                      status_single=input$coexp_status_single,
                      miancolor=input$coexp_heatmap_color)
  })
  
  
  shinyjs::hide(id="coexp_result_smu")
  
  observeEvent(input$coexp_update, {
    shinyjs::show(id="coexp_result_smu")
    
    output$coexp_result_plot_panel <- renderPlot({
      coexp_HeatMap()
    },height = 600,width = 1000)
    
  })
  
  coexp_data_show<- eventReactive(input$coexp_update,{
    coexp_data_show_fun(coexp_main_rdf_final(),coexp_main_pdf_final(),
                        input$coexp_method_single)
  })
  
  output$coexpression_datatable <- renderUI({
    
    DT::datatable(coexp_data_show(),rownames=F,
                  caption =htmltools::tags$caption(
                    style = 'font-size:14px',
                    htmltools::em(paste("Table: Coexpression matrix for",input$coexp_gene_single,sep = " "))),
                  options = list(
                    dom = 'frtip',
                    columnDefs = 
                      list(list(className = 'dt-center', 
                                targets = "_all")),
                    pageLength = 10,
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                      "}")
                  ))
    
  })
  
  ##observer
  observe({
    toggleState(id ="coexp_update",
                condition = length(input$coexp_specific_genes)>0)
  })
  
  observeEvent(input$coexp_gene_single,{
    if (input$coexp_gene_single=="") {
      showFeedbackDanger(
        inputId = "coexp_gene_single",
        text = "Please select one gene!",
      )
    } else {
      hideFeedback("coexp_gene_single")
    }
    
  })
  
  observeEvent(input$coexp_specific_genes,{
    if (length(input$coexp_specific_genes)==0) { #fail
      showFeedbackDanger(
        inputId = "coexp_specific_genes",
        text = "Please select one gene at least!"
      )
    } else if(length(input$coexp_specific_genes)==20){
      showFeedbackSuccess(
        inputId = "coexp_specific_genes",
        text = "The maximum is 20!"
      )
    }else {
      hideFeedback("coexp_specific_genes")
    }
    
  })
  
  output$coexp_plot_download_svg<- downloadHandler(
    filename = function(){
      paste("CoExpression_Heatmap",input$coexp_gene_single,".svg",sep="")
    },
    content = function(file){
      svg(file,height=10, width=20)
      print(coexp_HeatMap())
      dev.off()
    }
  )
  
  output$coexp_plot_download_pdf<- downloadHandler(
    filename = function(){
      paste("CoExpression_Heatmap",input$coexp_gene_single,".pdf",sep="")
    },
    content = function(file){
      pdf(file,height=10, width=20)
      print(coexp_HeatMap())
      dev.off()
    }
  )
  
  output$coexp_plot_download_png<- downloadHandler(
    filename = function(){
      paste("CoExpression_Heatmap",input$coexp_gene_single,".png",sep="")
    },
    content = function(file){
      png(file,width = 1000, height = 600, units = "px")
      print(coexp_HeatMap())
      dev.off()
    }
  )
  
  output$coexpression_datatable_download_csv<- downloadHandler(
    filename = function(){
      paste("Coexpressionheatmap_data",Sys.Date(),".csv",sep="")
    },
    content = function(file){
      coexp_main_data_final <- coexp_main_data_final()
      write.csv(coexp_main_data_final, file)
    }
  )
  output$coexpression_datatable_download_txt<- downloadHandler(
    filename = function(){
      paste("Coexpressionheatmap_data",Sys.Date(),".txt",sep="")
    },
    content = function(file){
      coexp_main_data_final <- coexp_main_data_final()
      write.table(coexp_main_data_final, file)
    }
  )
  
  ##=============================corMetaAnalysis
  corrMeta_xaxis_item_option<-reactive({
    corrMeta_xaxis_item_option <- switch (input$corrMeta_xaxis_type,
                                          "Gene" = switch (input$corrMeta_selective_species,
                                                           "Homo sapiens" = get("kk_human"),
                                                           "Mus musculus" = get("kk_mouse")),
                                          "Pathway"= get("metaanalysis_pathwayname"),
                                          "Immune Cell"= switch (input$corrMeta_selective_species,
                                                                 "Homo sapiens" = unique(imminfliNameList$immunecells[which(imminfliNameList$species=="Homo sapiens")]),
                                                                 "Mus musculus" = unique(imminfliNameList$immunecells[which(imminfliNameList$species=="Mus musculus")])))
    
  })
  
  observeEvent(corrMeta_xaxis_item_option(),{ 
    updateSelectizeInput (session,"corrMeta_xaxis_item",
                          choices =corrMeta_xaxis_item_option(),
                          selected = corrMeta_xaxis_item_option()[10],
                          server = TRUE) 
  })
  
  corrMeta_yaxis_item_option<-reactive({
    corrMeta_yaxis_item_option <- switch (input$corrMeta_yaxis_type,
                                          "Gene" = switch (input$corrMeta_selective_species,
                                                           "Homo sapiens" = get("kk_human"),
                                                           "Mus musculus" = get("kk_mouse")),
                                          "Pathway"= get("metaanalysis_pathwayname"),
                                          "Immune Cell"= switch (input$corrMeta_selective_species,
                                                                 "Homo sapiens" = unique(imminfliNameList$immunecells[which(imminfliNameList$species=="Homo sapiens")]),
                                                                 "Mus musculus" = unique(imminfliNameList$immunecells[which(imminfliNameList$species=="Mus musculus")])))
    
  })
  
  observeEvent(corrMeta_yaxis_item_option(),{ 
    updateSelectizeInput (session,"corrMeta_yaxis_item",
                          choices =corrMeta_yaxis_item_option(),
                          selected = corrMeta_yaxis_item_option()[20],
                          server = TRUE) 
  })
  
  corrMeta_update_requirement<-reactive({
    (input$corrMeta_xaxis_item!="" & input$corrMeta_yaxis_item!="")
  })
  
  observe({
    toggleState(id ="corrMeta_update",
                condition =corrMeta_update_requirement() )
  })
  
  ##result
  ##pos
  corrMeta_xaxis_pos<- eventReactive(input$corrMeta_update,{
    if(input$corrMeta_status_single=="HPV_Negative"){
      NULL
    }else{
      corrMeta_preproce_fun(input$corrMeta_xaxis_type,input$corrMeta_xaxis_item,
                            input$corrMeta_selective_species,"HPV_Positive")
    }
    
  })
  
  corrMeta_yaxis_pos<- eventReactive(input$corrMeta_update,{
    if(input$corrMeta_status_single=="HPV_Negative"){
      NULL
    }else{
      corrMeta_preproce_fun(input$corrMeta_yaxis_type,input$corrMeta_yaxis_item,
                            input$corrMeta_selective_species,"HPV_Positive")
    }
    
  })
  
  
  corrMeta_xyaxis_result_pos<- eventReactive(input$corrMeta_update,{
    if(input$corrMeta_status_single=="HPV_Negative"){
      NULL
    }else{
      corrMeta_correlation_fun(corrMeta_xaxis_pos(),
                               corrMeta_yaxis_pos(),input$corrMeta_method_single)
    }
    
  })
  
  corrMeta_xyaxis_metaresult_pos<-eventReactive(input$corrMeta_update,{
    if(input$corrMeta_status_single=="HPV_Negative"){
      NULL
    }else{
      corrMeta_metaanalysis_fun(corrMeta_xyaxis_result_pos(),input$corrMeta_forest_sm,
                          input$corrMeta_forest_model,input$corrMeta_forest_subgroup)
    }
  })
  
  corrMeta_xyaxis_forest_pos<-eventReactive(input$corrMeta_update,{
    if(input$corrMeta_status_single=="HPV_Negative"){
      NULL
    }else{
      if(class(corrMeta_xyaxis_metaresult_pos())[1]=="metacor"){
        corrMeta_forest_plot_fun(corrMeta_xyaxis_metaresult_pos())
      }else{
        NULL
      }
      
    }
  })
  
  output$corrMeta_result_posui<-renderUI({
    if(class(corrMeta_xyaxis_metaresult_pos())[1]=="metacor"){
      plotOutput("corrMeta_result_plot_pos_panel")%>% withSpinner(color="#6c6689")
    }else{
      div(
        h5("Note: In the metacor() (from the 'meta' package), the Fisher scoring algorithm does not converge. 
        Therefore, we use the rma() (from the 'metafor' package) to provide you with the results of the meta-analysis, which is for reference only. 
           You can also download relevant data from the 'Data' tab and conduct your own meta-analysis."),
        verbatimTextOutput("corrMeta_result_pos_text")
      )
      
    }
  })
  
  ##neg
  corrMeta_xaxis_neg<- eventReactive(input$corrMeta_update,{
    if(input$corrMeta_status_single=="HPV_Positive"){
      NULL
    }else{
      corrMeta_preproce_fun(input$corrMeta_xaxis_type,input$corrMeta_xaxis_item,
                            input$corrMeta_selective_species,"HPV_Negative")
    }
    
  })
  
  corrMeta_yaxis_neg<- eventReactive(input$corrMeta_update,{
    if(input$corrMeta_status_single=="HPV_Positive"){
      NULL
    }else{
      corrMeta_preproce_fun(input$corrMeta_yaxis_type,input$corrMeta_yaxis_item,
                            input$corrMeta_selective_species,"HPV_Negative")
    }
    
  })
  
  
  corrMeta_xyaxis_result_neg<- eventReactive(input$corrMeta_update,{
    if(input$corrMeta_status_single=="HPV_Positive"){
      NULL
    }else{
      corrMeta_correlation_fun(corrMeta_xaxis_neg(),
                               corrMeta_yaxis_neg(),input$corrMeta_method_single)
    }
    
  })
  
  
  corrMeta_xyaxis_metaresult_neg<-eventReactive(input$corrMeta_update,{
    if(input$corrMeta_status_single=="HPV_Positive"){
      NULL
    }else{
      corrMeta_metaanalysis_fun(corrMeta_xyaxis_result_neg(),input$corrMeta_forest_sm,
                                input$corrMeta_forest_model,input$corrMeta_forest_subgroup)
    }
  })
  
  corrMeta_xyaxis_forest_neg<-eventReactive(input$corrMeta_update,{
    if(input$corrMeta_status_single=="HPV_Positive"){
      NULL
    }else{
      if(class(corrMeta_xyaxis_metaresult_neg())[1]=="metacor"){
        corrMeta_forest_plot_fun(corrMeta_xyaxis_metaresult_neg())
      }else{
        NULL
      }
      
    }
  })
  
  output$corrMeta_result_negui<-renderUI({
    if(class(corrMeta_xyaxis_metaresult_neg())[1]=="metacor"){
      plotOutput("corrMeta_result_plot_neg_panel")%>% withSpinner(color="#6c6689")
    }else{
      div(
        h5("Note: In the metacor() (from the 'meta' package), the Fisher scoring algorithm does not converge. 
        Therefore, we use the rma() (from the 'metafor' package) to provide you with the results of the meta-analysis, which is for reference only. 
           You can also download relevant data from the 'Data' tab and conduct your own meta-analysis."),
        verbatimTextOutput("corrMeta_result_neg_text")
      )
    }
  })
 
  
  
  
  ##
  output$corrMeta_datatable <- renderUI({
    div(
      div(id="corrMeta_result_data_pos",
          dropdownButton(
            inputId = "corrMeta_datatable_download_pos",
            label = "Download Data",
            status = "primary",
            icon=icon("download"),
            circle = FALSE,
            width="50px",
            downloadButton(outputId = "corrMeta_datatable_download_csv_pos",
                           label = "CSV"),
            downloadButton(outputId = "corrMeta_datatable_download_txt_pos",
                           label = "TXT")),
          dataTableOutput("corrMeta_datatable_pos")
      ),
      div(id="corrMeta_result_data_neg",
          dropdownButton(
            inputId = "corrMeta_datatable_download_neg",
            label = "Download Data",
            status = "primary",
            icon=icon("download"),
            circle = FALSE,
            width="50px",
            downloadButton(outputId = "corrMeta_datatable_download_csv_neg",
                           label = "CSV"),
            downloadButton(outputId = "corrMeta_datatable_download_txt_neg",
                           label = "TXT")),
          dataTableOutput("corrMeta_datatable_neg")
      )
    )
  })
  
  
  
  ##show
  shinyjs::hide(id ="corrMeta_result_smu")
  observeEvent(input$corrMeta_update, {
    shinyjs::show(id ="corrMeta_result_smu")
    
    if(input$corrMeta_status_single=="All"){
      shinyjs::show(id ="corrMeta_result_plot_pos")
      shinyjs::show(id ="corrMeta_result_plot_neg")
      shinyjs::show(id ="corrMeta_result_data_pos")
      shinyjs::show(id ="corrMeta_result_data_neg")
    }else if(input$corrMeta_status_single=="HPV_Positive"){
      shinyjs::show(id ="corrMeta_result_plot_pos")
      shinyjs::hide(id ="corrMeta_result_plot_neg")
      shinyjs::show(id ="corrMeta_result_data_pos")
      shinyjs::hide(id ="corrMeta_result_data_neg")
    }else if(input$corrMeta_status_single=="HPV_Negative"){
      shinyjs::hide(id ="corrMeta_result_plot_pos")
      shinyjs::show(id ="corrMeta_result_plot_neg")
      shinyjs::hide(id ="corrMeta_result_data_pos")
      shinyjs::show(id ="corrMeta_result_data_neg")
    }
    
  })
  
  output$corrMeta_result_plot_pos_panel<-renderPlot({
    corrMeta_xyaxis_forest_pos()
  },width=800,height = 1200)
  
  output$corrMeta_result_pos_text<-renderPrint({
    print(corrMeta_xyaxis_metaresult_pos())
  })
  
  output$corrMeta_result_plot_neg_panel<-renderPlot({
    corrMeta_xyaxis_forest_neg()
  },width=800,height = 1000)
  
  output$corrMeta_datatable_pos <- renderDataTable({
    DT::datatable(corrMeta_xyaxis_result_pos(),rownames=T,
                  caption =htmltools::tags$caption(
                    style = 'font-size:14px',
                    htmltools::em("Table: Input data (HPV Postive samples) for meta-analysis.")),
                  options = list(
                    dom = 'rtip',
                    columnDefs = 
                      list(list(className = 'dt-center', 
                                targets = "_all")),
                    pageLength = 10,
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                      "}")
                  ))
  })
  
  
  output$corrMeta_datatable_neg <- renderDataTable({
    DT::datatable(corrMeta_xyaxis_result_neg(),rownames=T,
                  caption =htmltools::tags$caption(
                    style = 'font-size:14px',
                    htmltools::em("Table: Input data (HPV Negative samples) for meta-analysis.")),
                  options = list(
                    dom = 'rtip',
                    columnDefs = 
                      list(list(className = 'dt-center', 
                                targets = "_all")),
                    pageLength = 10,
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                      "}")
                  ))
  })
  
  ##observer
  observeEvent(input$corrMeta_xaxis_item,{
    if (input$corrMeta_xaxis_item=="") {
      showFeedbackDanger(
        inputId = "corrMeta_xaxis_item",
        text = "Please select an object!",
      )
    } else {
      hideFeedback("corrMeta_xaxis_item")
    }
  })
  
  observeEvent(input$corrMeta_yaxis_item,{
    if (input$corrMeta_yaxis_item=="") {
      showFeedbackDanger(
        inputId = "corrMeta_yaxis_item",
        text = "Please select an object!",
      )
    } else {
      hideFeedback("corrMeta_yaxis_item")
    }
  })
  
  ##download
  output$corrMeta_plot_download_svg_pos<- downloadHandler(
    filename = function(){
      paste("CorrMeta_",
            paste("HPVpositive",input$corrMeta_xaxis_item,input$corrMeta_yaxis_item,sep = "_"),
            ".svg",sep="")
    },
    content = function(file){
      svg(file,height=20, width=10)
      corrMeta_forest_plot_fun(corrMeta_xyaxis_metaresult_pos())
      dev.off()
    }
  )
  
  output$corrMeta_plot_download_pdf_pos<- downloadHandler(
    filename = function(){
      paste("CorrMeta_",
            paste("HPVpositive",input$corrMeta_xaxis_item,input$corrMeta_yaxis_item,sep = "_"),
            ".pdf",sep="")
    },
    content = function(file){
      pdf(file,height=20, width=10)
      corrMeta_forest_plot_fun(corrMeta_xyaxis_metaresult_pos())
      dev.off()
    }
  )
  
  output$corrMeta_plot_download_png_pos<- downloadHandler(
    filename = function(){
      paste("CorrMeta_",
            paste("HPVpositive",input$corrMeta_xaxis_item,input$corrMeta_yaxis_item,sep = "_"),
            ".png",sep="")
    },
    content = function(file){
      png(file,width = 700, height = 1400, units = "px")
      corrMeta_forest_plot_fun(corrMeta_xyaxis_metaresult_pos())
      dev.off()
    }
  )
  
  output$corrMeta_plot_download_svg_neg<- downloadHandler(
    filename = function(){
      paste("CorrMeta_",
            paste("HPVnegative",input$corrMeta_xaxis_item,input$corrMeta_yaxis_item,sep = "_"),
            ".svg",sep="")
    },
    content = function(file){
      svg(file,height=20, width=10)
      corrMeta_forest_plot_fun(corrMeta_xyaxis_metaresult_neg())
      dev.off()
    }
  )
  
  output$corrMeta_plot_download_pdf_neg<- downloadHandler(
    filename = function(){
      paste("CorrMeta_",
            paste("HPVnegative",input$corrMeta_xaxis_item,input$corrMeta_yaxis_item,sep = "_"),
            ".pdf",sep="")
    },
    content = function(file){
      pdf(file,height=20, width=10)
      corrMeta_forest_plot_fun(corrMeta_xyaxis_metaresult_neg())
      dev.off()
    }
  )
  
  output$corrMeta_plot_download_png_neg<- downloadHandler(
    filename = function(){
      paste("CorrMeta_",
            paste("HPVnegative",input$corrMeta_xaxis_item,input$corrMeta_yaxis_item,sep = "_"),
            ".png",sep="")
    },
    content = function(file){
      png(file,width = 700, height = 1400, units = "px")
      corrMeta_forest_plot_fun(corrMeta_xyaxis_metaresult_neg())
      dev.off()
    }
  )
  
  output$corrMeta_datatable_download_csv_pos<- downloadHandler(
    filename = function(){
      paste("corrMeta_pathway_HPVpositived",Sys.Date(),".csv",sep="")
    },
    content = function(file){
      write.csv(corrMeta_xyaxis_result_pos(), file)
    }
  )
  output$corrMeta_datatable_download_txt_pos<- downloadHandler(
    filename = function(){
      paste("corrMeta_pathway_HPVpositive",Sys.Date(),".txt",sep="")
    },
    content = function(file){
      write.table(corrMeta_xyaxis_result_pos(), file)
    }
  )
  
  output$corrMeta_datatable_download_csv_neg<- downloadHandler(
    filename = function(){
      paste("corrMeta_pathway_HPVnegative",Sys.Date(),".csv",sep="")
    },
    content = function(file){
      write.csv(corrMeta_xyaxis_result_neg(), file)
    }
  )
  output$corrMeta_datatable_download_txt_neg<- downloadHandler(
    filename = function(){
      paste("corrMeta_pathway_HPVnegative",Sys.Date(),".txt",sep="")
    },
    content = function(file){
      write.table(corrMeta_xyaxis_result_neg(), file)
    }
  )

  
  ##=============================Data1
  
  output$data_immunecell_genes_df<-renderDataTable({
    DT::datatable(data_immunecell_genes,
                  caption="Table2: Immune cell related genes.",
                  rownames = FALSE,
                  escape=FALSE,
                  extensions=c('Buttons'),
                  filter = list(position = 'top', clear = FALSE),
                  options = list(
                    dom = 'rtip',
                    buttons = c('csv'),
                    pageLength = 5,
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                      "}")))
  })
  
  output$data_immunecell_genes_download_csv<-downloadHandler(
    filename = function(){
      paste("Immunecells_related_genes",".csv",sep = "")
    },
    content = function(file){
      sep<-","
      write.table(data_immunecell_genes,file,sep=sep,row.names =FALSE)
    }
  )
  
  ##=============================Data2
  data_datasets_whole_show<-reactive({
    data_datasets_species <- input$data_species
    data_datasets_classification <- input$data_classification
    data_datasets_cancer_type <- input$data_cance_type
    
    if (is.null(data_datasets_species)&is.null(data_datasets_classification)&is.null(data_datasets_cancer_type)){
      data_datasets_whole_show<-datasetbrowser
      return(data_datasets_whole_show)
    }else{
      data_datasets_whole_show<-datasetbrowser
      if(!is.null(data_datasets_species)){
        data_datasets_whole_show<-data_datasets_whole_show%>%
          filter(Species%in% data_datasets_species)
      }
      
      if(!is.null(data_datasets_classification)){
        data_datasets_whole_show<-data_datasets_whole_show%>%
          filter(Classification%in% data_datasets_classification)
      }
      
      if(!is.null(data_datasets_cancer_type)){
        data_datasets_whole_show<-data_datasets_whole_show%>%
          filter(`Cancer Types`%in%data_datasets_cancer_type)
      }
      
      return(data_datasets_whole_show)
    }
  })
  
  output$data_datasets_wholedataframe<-renderDataTable({
    
    datatable(data_datasets_whole_show()[,c(1:9)],
              style = 'bootstrap', class = 'table-bordered',
              rownames = TRUE,
              escape = F,
              extensions = c('Responsive','Scroller'),
              options = list(dom='itrp',
                             responsive=TRUE,
                             pageLength = 8,
                             scrollY = "300px",
                             ordering=F,
                             columnDefs = 
                               list(list(className = 'dt-left',targets =c(1:6)),
                                    list(className = 'none',targets =c(-1,-2))),
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}")
              ),
              
              caption = htmltools::tags$caption(
                style = 'caption-side: bottom; text-align: center;',
                'Table 1: ', htmltools::em('Overview Table')))
    
    
  })
  
  
  data_datasets_detail_show <- reactive({
    data_datasets_whole_show()[input$data_datasets_wholedataframe_rows_selected,c(1,10:19)]
  })
  
  output$data_datasets_detailsdataframe<-renderDataTable({
    
    datatable(data_datasets_detail_show(),
              style = 'bootstrap', class = 'table-bordered',
              rownames = FALSE,
              escape = F,
              extensions = c('Responsive'),
              options = list(dom='itrp',
                             responsive=TRUE,
                             pageLength = 10,
                             ordering=F,
                             columnDefs = 
                               list(list(className = 'dt-left',targets ="_all")),
                             #language.emptyTable="hahahaha",
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}")
              ),
              
              caption = htmltools::tags$caption(
                style = 'caption-side: bottom; text-align: center;',
                'Table 2: ', htmltools::em('Details Table (Please select one dataset in Table1 first.)')))
    
    
  })
  
  output$data_datasets_browser_csv<-downloadHandler(
    filename = function(){
      paste("DatasetsBrowser.csv")
    },
    content = function(file){
      sep<-","
      write.table(datasetbrowser,file,sep=sep,row.names = FALSE)
    }
  )
  
  ##============clinical data
  clinical_data_show<-eventReactive(input$data_clinical_dataset,{
    
    if(input$data_clinical_dataset!=""){
      clinical_data_fun(input$data_clinical_dataset)
    }
    
  })
  
  
  output$data_clinical_show_panel<-renderDataTable({
    
    datatable(clinical_data_show(),
              style = 'bootstrap', class = 'table-bordered',
              rownames = TRUE,
              escape = F,
              extensions = c('Responsive','Scroller'),
              options = list(dom='itrp',
                             responsive=TRUE,
                             pageLength = 8,
                             scrollY = "300px",
                             ordering=F,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}")
              ),
              
              caption = htmltools::tags$caption(
                style = 'caption-side: bottom; text-align: center;',
                htmltools::em('Baseline Characteristics Table')))
    
    
  })
  
  output$data_clinical_csv<-downloadHandler(
    filename = function(){
      paste(input$data_clinical_dataset,"_clinicalData.csv")
    },
    content = function(file){
      sep<-","
      write.table(clinical_data_show(),file,sep=sep,row.names = FALSE)
    }
  )
  
  ##======comment box
  observe({
    shinyjs::toggleState(id ="comment_submit",
                         condition =(input$comment_text!=""))
  })
  
  output$comment_file_input<-renderUI({
    fileInput("comment_pic", "Choose a PNG/JPGE File (optional)",
              multiple = FALSE,width = "100%",
              accept = c("image/png","image/jpeg"))
  })
  
  observeEvent(input$comment_file_clear, {
    output$comment_file_input<-renderUI({
      fileInput("comment_pic", "Choose a PNG/JPGE File (optional)",
                multiple = FALSE,width = "100%",
                accept = c("image/png","image/jpeg"))
    })
  })
  
  
  observeEvent(input$comment_submit, {
    
    req(input$comment_text)
    
    smtp <- emayili::server(
      host = "smtp.163.com",
      port = 25,
      username = "shiny_luopeng@163.com",
      password = "LFZKTEZQQLXDTOGK"
    )
    
    email <- envelope() %>%
      from("shiny_luopeng@163.com") %>%
      to("shiny_luopeng@163.com")%>%
      subject(paste("HPVTIMER-SHINY FEEDBACK: ", input$comment_contact))%>%
      emayili::html(
        tagList(
          h2("Comments for HPVTIMER"),
          p(input$comment_text),
          hr(),
          p(paste("from:",input$comment_contact))
        )
      )
    
    if(is.null(input$comment_pic)){
      email <- email
    }else{
      email <- email %>% attachment(input$comment_pic$datapath,
                                    name=input$comment_pic$name)
    }
    
    smtp(email)
    
    show_alert(
      title = "Success",
      text = "Thanks, your response was submitted successfully! We will get back to you as soon as possible.",
      type = "success"
    )
  })
  
}

