
#' Univariate Shiny App
#'
#' Takes a user supplied data frame of drug response data and does univariate analysis vs mutation status of genes
#'
#' @param con A \code{SQLiteConnection} object to the database
#' @param drug_df A data frame containing the drug data
#' @param gsc Should either be NULL or a GSC object as created by piano::loadGSC.  A common use would be gsc=loadGSC('MSigDB.gmt')
#' @return Launches an interactive Shiny application
#' @export
#' @import shiny
shinyUnivariateAnalysisApp <- function(con, drug_df=NULL, gsc=NULL) {

  shiny::shinyApp(
    ui = shinyUnivariateAnalysisGridUI(),
    server = function(input, output) {
        shinyUnivariateAnalysisServer(input, output, con, drug_df, gsc)
    }
  )
}
