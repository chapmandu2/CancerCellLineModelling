#' multivariateAnalysisVarImp
#' Extract variable importance parameters from a caret::train object
#' @param caret_res Train object as produced by multivariateAnalysisElasticNet for example
#' @param glmnet_caret Boolean to determine whether to use the caret varImp function for glmnet objects (default FALSE)
#' @param varImp_scale Boolean to determine whether caret::varImp scale parameter is set to FALSE (default) or TRUE
#'
#' @return data frame
#' @export
#' @import dplyr caret

multivariateAnalysisVarImp <- function (caret_res, glmnet_caret=FALSE, varImp_scale=FALSE) {

    if (!('train' %in% class(caret_res))) {
        stop('Class type of supplied result is not "train"')
    }

    if (caret_res$method == 'glmnet' & !glmnet_caret) {
        #varImp doesn't work very well for glmnet objects so extract the betas ourselves if glmnet_caret set to FALSE (Default)
        bestModel <- caret_res$finalModel
        bestLambda_idx <- which.min(abs(bestModel$lambda - bestModel$lambdaOpt))
        output <- as.data.frame(bestModel$beta [ , bestLambda_idx ])
    } else {
        #most of the time just use the varImp function
        output <- varImp(caret_res, scale = varImp_scale)$importance
    }

    colnames(output) <- 'Imp'

    #order the features by importance and add convenience variables
    output <- output %>%
        dplyr::add_rownames('feature_name') %>%
        dplyr::arrange(desc(abs(Imp))) %>%
        dplyr::mutate(idx=row_number(),
                      model_type=caret_res$modelInfo$label,
                      resp_var=attr(caret_res, 'resp_var'))


    return(output)

}
