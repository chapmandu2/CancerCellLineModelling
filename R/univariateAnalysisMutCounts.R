#' univariateAnalysisMutCounts
#' Do mutation counting step for univariate analysis
#' @param df Output data from from CancerCellLines::makeRespVsGeneticDataFrame
#'
#' @return data frame of mutation counts
#' @export
#' @import dplyr

univariateAnalysisMutCounts <- function (rvg_df) {

    mut_counts <- rvg_df %>%
        dplyr::filter(!is.na(resp_value) ) %>%
        dplyr::group_by(assayed_id, feature_type, resp_id, feature_value) %>%
        dplyr::summarise(mut_count=n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(feature_value = paste0('gt', as.character(feature_value))) %>%
        tidyr::spread(feature_value,mut_count) %>%
        dplyr::mutate_each(funs(ifelse(is.na(.), 0, .)), starts_with('gt'))

    return(mut_counts)

}
