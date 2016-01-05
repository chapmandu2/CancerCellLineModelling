#' multivariateAnalysisGBM
#' Do a multivariate boosted tree analysis
#' @param df Output data from from CancerCellLines::makeRespVsGeneticDataFrame
#'
#' @return data frame
#' @export
#' @import dplyr caret foreach parallel doParallel

multivariateAnalysisGBM <- function (tall_df, resp_var=NULL, n.trees=NULL, interaction.depth=NULL, shrinkage=NULL) {

    caret_data <- multivariateAnalysisPrep(tall_df, resp_var)

    #allow for custom values of n.trees, interaction.depth and shrinkage
    if(is.null(n.trees)) { n.trees <- (1:30)*15 }
    if(is.null(interaction.depth)) { interaction.depth <- c(1,2,3) }
    if(is.null(shrinkage)) { shrinkage <- c(0.003, 0.01) }

    #do the gbm fit
    ctrl <- trainControl(method = "repeatedcv",number=5, repeats = 5 )  #5 fold x validation
    gbmFit_grid <- expand.grid(n.trees=n.trees, interaction.depth=interaction.depth, shrinkage=shrinkage, n.minobsinnode=5) #grid of parameters to test
    cl <- makeCluster(detectCores())
    registerDoParallel(cl)
    gbmFit <- train(formula(sprintf("`%s_resp` ~ .", resp_var)),
                    data = caret_data,
                    method='gbm',
                    tuneGrid=gbmFit_grid,
                    trControl = ctrl

    )
    stopCluster(cl)

    #store the compound name for future reference
    attr(gbmFit, 'resp_var') <- resp_var

    return(gbmFit)

}
