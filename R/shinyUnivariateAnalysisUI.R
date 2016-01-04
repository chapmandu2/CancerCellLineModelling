shinyUnivariateAnalysisUI <- function () {

  fluidPage(
    sidebarLayout(
      sidebarPanel(
        helpText("Correlate genomic and response data"),
        uiOutput('genesetUI'),
        textInput("geneids", label = h3("Manually Enter Gene Symbols"),
                  value = "BRAF,TP53,PTEN,NRAS,DGAT1"),
        uiOutput('respUI'),
        selectInput("data_type", label = h3("Select mutation data source"),
                    multiple = TRUE,
                    choices = list("CCLE hybcap" = 'hybcap', "Cosmic CLP" = 'cosmicclp'),
                    selected = c('hybcap', 'cosmicclp'),
                    selectize = FALSE,
                    size=2),
        selectInput("output_option", label = h3("Generate"),
                    choices = list('Volcano Plot' = 1, 'Input Data'=2, 'Results'=3),
                    selected = 1),
        h3("Plot Options:"),
        selectInput("tissue_option", label = "Select tissue categorisation",
                    choices = list('Crude' = 'crude', 'CCLE' = 'ccle', 'Eurofins'='eurofins', 'Cosmic CLP' = 'cosmic_clp'),
                    selected = 'crude'),
        sliderInput('plot_width', label = 'Control plot width', min=200, max=2000, value=600),
        sliderInput('plot_height', label = 'Control plot height', min=200, max=2000, value=400),
        uiOutput('tissuesUI')
        # submitButton('Submit')
      ),
      uiOutput('resultsUI')


    )
  )
}
