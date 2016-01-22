shinyUnivariateAnalysisGridUI <- function () {

  fluidPage(
      h3('Univariate Analysis Shiny App'),
    fluidRow(
        column(3,
               wellPanel(
               uiOutput('genesetUI'),
               textInput("geneids", label = p("Manually Enter Gene Symbols"),
                         value = "BRAF,TP53,PTEN,NRAS,DGAT1"),
               uiOutput('respUI'),
               selectInput("data_type", label = p("Select mutation data source"),
                           multiple = TRUE,
                           choices = list("CCLE hybcap" = 'hybcap', "Cosmic CLP" = 'cosmicclp'),
                           selected = c('hybcap', 'cosmicclp'),
                           selectize = FALSE,
                           size=2)
               )
               #####
               ),
        column(3,
               wellPanel(
               sliderInput('mut_count', label = 'Minimum number of mutations to include', min=1, max=20, value=2),
               sliderInput('effect_th', label = 'Effect size threshold (fold change)', min=1, max=20, value=2, step=0.1),
               sliderInput('pval_th', label = 'P-value threshold (negative log10 scale)', min=0, max=5, value=2, step=0.1),
               checkboxInput("fdr_option", label = "Correct for multiple testing")
               )

               ######
               ),
        column(3,
               wellPanel(
               selectInput("tissue_option", label = "Select tissue categorisation",
                           choices = list('Crude' = 'crude', 'CCLE' = 'ccle', 'Eurofins'='eurofins', 'Cosmic CLP' = 'cosmic_clp'),
                           selected = 'crude'),
               sliderInput('plot_width', label = 'Control plot width', min=200, max=2000, value=600),
               sliderInput('plot_height', label = 'Control plot height', min=200, max=2000, value=400),
               checkboxInput("repel_option", label = "Avoid label overlap (slow)")
               )

               #####
               ),
        column(3,
               wellPanel(
               uiOutput('tissuesUI'),
               selectInput("output_option", label = p("Generate"),
                           choices = list('Volcano Plot' = 1, 'Input Data'=2, 'Results'=3, 'Mutation Counts'=4),
                           selected = 1)
               )
        )
    ),

    fluidRow(
        uiOutput('resultsUI')
    ),

    hr(),

    helpText('Developed by Phil.Chapman@cruk.manchester.ac.uk')

  )
}
