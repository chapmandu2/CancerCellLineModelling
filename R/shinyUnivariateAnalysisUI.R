shinyUnivariateAnalysisUI <- function () {

  fluidPage(
    sidebarLayout(
      sidebarPanel(
        helpText("Correlate genomic and response data"),
        uiOutput('genesetUI'),
        textInput("geneids", label = h5("Manually Enter Gene Symbols"),
                  value = "BRAF,TP53,PTEN,NRAS,DGAT1"),
        uiOutput('respUI'),
        selectInput("data_type", label = h5("Select mutation data source"),
                    multiple = TRUE,
                    choices = list("CCLE hybcap" = 'hybcap', "Cosmic CLP" = 'cosmicclp'),
                    selected = c('hybcap', 'cosmicclp'),
                    selectize = FALSE,
                    size=2),
        h5("Analysis Options"),
        checkboxInput("fdr_option", label = "Correct for multiple testing"),
        sliderInput('mut_count', label = 'Minimum number of mutations to include', min=1, max=20, value=2),
        sliderInput('effect_th', label = 'Effect size threshold (fold change)', min=1, max=20, value=2, step=0.1),
        sliderInput('pval_th', label = 'P-value threshold (negative log10 scale)', min=0, max=5, value=1, step=0.1),
        selectInput("output_option", label = h5("Generate"),
                    choices = list('Volcano Plot' = 1, 'Input Data'=2, 'Results'=3),
                    selected = 1),
        h5("Plot Options:"),
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
