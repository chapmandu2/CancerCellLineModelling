#' univariateAnalysis
#' Do a univariate analysis
#' @param df Output data from from CancerCellLines::makeRespVsGeneticDataFrame
#'
#' @return data frame
#' @export
#' @import dplyr broom

univariateAnalysis <- function (rvg_df) {

    mut_counts <- univariateAnalysisMutCounts(rvg_df)

    uni_res <- rvg_df %>%
        dplyr::group_by(assayed_id, feature_type, resp_id) %>%
        dplyr::do(broom::tidy(lm(resp_value ~ feature_value, data=.))) %>%
        dplyr::filter(grepl('feature_value', term)) %>%
        dplyr::ungroup() %>%
        dplyr::inner_join(mut_counts, by=c('assayed_id', 'feature_type', 'resp_id'))

    volcano_data <- uni_res %>%
        dplyr::mutate(effect_size_unlog = 10^estimate,
                      FDR = as.numeric(NA),
                      log10FDR = as.numeric(NA))


    return(volcano_data)


}
