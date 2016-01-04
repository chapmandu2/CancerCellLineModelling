#' multivariateAnalysisElasticNet
#' Do a univariate analysis
#' @param df Output data from from CancerCellLines::makeRespVsGeneticDataFrame
#'
#' @return data frame
#' @export
#' @import dplyr caret foreach parallel doParallel

multivariateAnalysisElasticNet <- function (tall_df, resp_var=NULL, lambda=NULL, alpha=NULL) {

    #require(caret);require(foreach);require(parallel);require(doParallel)

    caret_data <- multivariateAnalysisPrep(tall_df, resp_var)

    #allow for custom values of lambda and alpha
    if(is.null(lambda)) {lambda <- 10^seq(8,-5, length=100)}
    if(is.null(alpha)) {alpha <- seq(0,1,0.05)}

    #do the glmnet fit
    ctrl <- trainControl(method = "repeatedcv",number=5, repeats = 5 )  #5 fold x validation
    glmnet_grid <- expand.grid(lambda=lambda, alpha = alpha) #grid of parameters to test
    cl <- makeCluster(detectCores())
    registerDoParallel(cl)
    glmnetFit <- train(formula(sprintf("`%s_resp` ~ .", resp_var)),
                       data = caret_data,
                       method='glmnet',
                       tuneGrid=glmnet_grid,
                       preProcess = c("center", "scale"),
                       trControl = ctrl

    )
    stopCluster(cl)

    #store the compound name for future reference
    attr(glmnetFit, 'resp_var') <- resp_var

    return(glmnetFit)

}
