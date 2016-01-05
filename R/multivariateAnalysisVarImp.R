#' multivariateAnalysisVarImp
#' Extract variable importance parameters from a caret::train object
#' @param caret_res Train object as produced by multivariateAnalysisElasticNet for example
#'
#' @return data frame
#' @export
#' @import dplyr caret

multivariateAnalysisVarImp <- function (caret_res) {

    if (!('train' %in% class(caret_res))) {
        stop('Class type of supplied result is not "train"')
    }

    if (caret_res$method == 'glmnet') {
        #varImp doesn't work very well for glmnet objects so extract the betas ourselves
        bestModel <- caret_res$finalModel
        bestLambda_idx <- which.min(abs(bestModel$lambda - bestModel$lambdaOpt))
        output <- as.data.frame(bestModel$beta [ , bestLambda_idx ])
    } else {
        #most of the time just use the varImp function
        output <- varImp(caret_res, scale = FALSE)$importance
    }

    colnames(output) <- 'Imp'

    #order the features by importance and add convenience variables
    output <- output %>%
        dplyr::add_rownames('feature_name') %>%
        dplyr::arrange(desc(abs(Imp))) %>%
        dplyr::mutate(idx=row_number())

    return(output)

}
