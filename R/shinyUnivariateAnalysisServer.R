#' shinyUnivariateAnalysisServer
#'
#' Create a shiny server for the univariate analysis shiny app
#'
#' @param input shiny input
#' @param output shiny output
#' @param con SQLite connection object
#' @param drug_df drug data frame
#' @param gsc piano gsc object
#'
#' @return a shiny server
#' @export
shinyUnivariateAnalysisServer <- function(input, output, con, drug_df=NULL, gsc=NULL) {

  #get the cell lines for the given tissues
  proc_cls <- reactive({

    if (is.null(drug_df)) {
      intersect(getTissueCellLines(con, input$tissue), proc_resp_data()$unified_id)
    } else {
      intersect(getTissueCellLines(con, input$tissue), proc_resp_data()$unified_id)
    }

  })

  #get custom and database response data into a common format for reference of what cell lines/drugs are present
  proc_resp_data <- reactive({

    if (is.null(drug_df)) {
      #if no drug data frame provided just use CCLE
      ccle_drugs.df <- src_sqlite(con@dbname) %>% tbl("ccle_drug_data") %>% select(Compound, CCLE_name) %>% distinct %>% collect
      resp_data <- getDrugData_CCLE(con, unique(ccle_drugs.df$Compound), unique(ccle_drugs.df$CCLE_name)) %>%
        transmute(unified_id, resp_id=assayed_id, resp_value=value)
      return(resp_data)
    } else {
      #if drug data provided then use that
      resp_data <- getDrugData_custom(drug_df, unique(drug_df$compound_id), unique(drug_df$unified_id) ) %>%
        transmute(unified_id, resp_id=assayed_id, resp_value=value)
      return(resp_data)
    }
  })

  #convert the selected geneset into gene ids
  proc_gsc <- reactive({

      if(is.null(gsc) | is.null(input$geneset)) {
          return(NULL)
      }

      unique(unlist(unname(gsc$gsc[input$geneset])))

  })

  #make the gene set collection UI
  output$genesetUI <- renderUI({

      if(is.null(gsc)) {
          return(NULL)
      }

      selectInput("geneset", label = h5("Select a gene set"),
                  choices = names(gsc$gsc),
                  multiple = TRUE,
                  selectize = TRUE)

  })

  #combine gene list and manually entered gene ids
  proc_gene_ids <- reactive({

      unique(c(unlist(strsplit(input$geneids,',')), proc_gsc()))

  })


  proc_data <- reactive({
    makeRespVsGeneticDataFrame(con, gene=proc_gene_ids(),
                               cell_lines=proc_cls(),
                               drug=input$respid,
                               data_types = input$data_type,
                               drug_df = drug_df,
                               tissue_info = input$tissue_option)
  })

  proc_results <- reactive({

      res_df <- univariateAnalysis(proc_data())
      res_df <- univariateAnalysisFDR(res_df, mut_count_th = input$mut_count)
      return(res_df)

  })

  #make a reactive tissue selection UI
  output$tissuesUI <- renderUI({

    tissues.df <- getTissueInfo(con, input$tissue_option)
    resp_cls <- unique(proc_resp_data()$unified_id)

    tissues.df <- tissues.df %>%
      dplyr::filter(unified_id %in% resp_cls) %>%
      dplyr::select(tissue) %>%
      distinct %>% arrange(tissue)

    tissues <- tissues.df$tissue

    selectInput("tissue", label = h5("Select a tissue type"),
                choices = tissues,
                selected = tissues,
                multiple = TRUE,
                selectize = FALSE,
                size=10)

  })

  #make a reactive response variable selection UI
  output$respUI <- renderUI({

    resp.df <- proc_resp_data() %>% select(resp_id) %>%
      distinct() %>% arrange(resp_id)

    resps <- resp.df$resp_id

    selectInput("respid", label = h5("Select a response variable"),
                choices = resps,
                selected = resps[1])

  })

  output$resultsUI <- renderUI({

    if (input$output_option == 1) {
      mainPanel(plotOutput("plot1", width=input$plot_width, height=input$plot_height),
                downloadButton('downloadData', 'Download Data'),
                downloadButton('downloadMutCounts', 'Download Mutation Counts'),
                downloadButton('downloadResults', 'Download Results'))
    } else if (input$output_option == 2) {
        mainPanel(DT::dataTableOutput('df'),
                  downloadButton('downloadData', 'Download Data'),
                  downloadButton('downloadMutCounts', 'Download Mutation Counts'),
                  downloadButton('downloadResults', 'Download Results'))
    } else if (input$output_option == 3) {
      mainPanel(DT::dataTableOutput('res_df'),
                downloadButton('downloadData', 'Download Data'),
                downloadButton('downloadMutCounts', 'Download Mutation Counts'),
                downloadButton('downloadResults', 'Download Results'))
    } else {
        mainPanel(DT::dataTableOutput('mut_df'),
                  downloadButton('downloadData', 'Download Data'),
                  downloadButton('downloadMutCounts', 'Download Mutation Counts'),
                  downloadButton('downloadResults', 'Download Results'))
    }


  })

  output$text1 <- renderText({
    sprintf("Tissue is length: %s", length(input$tissue))
  })

  output$df <- DT::renderDataTable({
    proc_data()
    #get_shiny_cell_lines(con, input$tissue) %>% as.data.frame
  }, filter='top')

  output$res_df <- DT::renderDataTable({
      proc_results()
  }, filter='top')

  output$mut_df <- DT::renderDataTable({
      univariateAnalysisMutCounts(proc_data())
  }, filter='top')

  output$plot1 <- renderPlot({
      univariateVolcanoPlot(proc_results(), pval_th = 10^(0-input$pval_th), effect_th = input$effect_th, use_fdr = input$fdr_option, repel_option=input$repel_option)
  })


  output$downloadData <- downloadHandler(
    filename = "univariateAnalaysis_inputdata.txt",
    content = function(file) {
      write.table (proc_data(), file = file, sep = '\t', row.names = FALSE, col.names=TRUE, na='')
    }
  )

  output$downloadResults <- downloadHandler(
      filename = "univariateAnalysis_results.txt",
      content = function(file) {
          write.table (proc_results(), file = file, sep = '\t', row.names = FALSE, col.names=TRUE, na='')
      }
  )

  output$downloadMutCounts <- downloadHandler(
      filename = "univariateAnalysis_mut_counts.txt",
      content = function(file) {
          write.table (univariateAnalysisMutCounts(proc_data()), file = file, sep = '\t', row.names = FALSE, col.names=TRUE, na='')
      }
  )

}
