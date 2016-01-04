#' multivariateAnalysisPrep
#' Converts a tall data frame from makeTallDataFrame into a wide data frame with a single response variable ready for multivariate analysis
#' @param df Output data from from CancerCellLines::makeTallDataFrame
#' @param resp_var Response variable present in the data.frame - ie value of ID field when Type field is resp
#'
#' @return data frame
#' @export
#' @import dplyr caret

multivariateAnalysisPrep <- function (tall_df, resp_var=NULL) {

    #which response variable are we using?
    resp_df <- tall_df %>% dplyr::filter(Type=='resp') %>% dplyr::select(ID) %>% distinct() %>% arrange(ID)
    if(is.null(resp_var)) {
        resp_var <- resp_df$ID[1]
        warning(sprintf("No response variable defined so first one used: %s", resp_var))
    } else if (!(resp_var %in% resp_df$ID)) {
        stop(sprintf("Defined response variable %s not found in supplied data frame", resp_var))
    }

    #filter the rvg_df so we only have a single response variable
    outdata <- tall_df %>%
        dplyr::filter(!(Type == 'resp' & ID != resp_var)) %>%
        makeWideFromTallDataFrame() %>%
        dplyr::select(-CCLE_name)

    #get rid of near zero variance variables
    nzv <- nearZeroVar(outdata)
    outdata <- outdata[, -nzv]

    return(outdata)

}
