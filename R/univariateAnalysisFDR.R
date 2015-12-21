#' univariateAnalysisFDR
#' Do a univariate analysis
#' @param df Output data from from CancerCellLineModelling::univariateAnalysis
#' @param mut_count_th Minimum number of mutated samples
#'
#' @return data frame
#' @export
#' @import dplyr

univariateAnalysisFDR <- function (df, mut_count_th=1) {

    df_corrected <- df %>%
        dplyr::filter(gt1 >= mut_count_th) %>%
        dplyr::group_by(feature_type, resp_id) %>%
        dplyr::mutate(FDR=p.adjust(p.value, 'fdr')) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(log10FDR = log10(FDR))

    df_uncorrected <- df %>%
        dplyr::filter(gt1 < mut_count_th)

    return(bind_rows(df_corrected, df_uncorrected))

}
