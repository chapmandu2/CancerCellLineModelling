#' multivariateAnalysisVarImpPlot
#' Plot variable importance parameters from a caret::train object
#' @param caret_res Train object as produced by multivariateAnalysisElasticNet for example
#' @param n Number of features to plot
#' @return ggplot object
#' @export
#' @import dplyr caret ggplot2

multivariateAnalysisVarImpPlot <- function (caret_res, n=20) {

    if (!('train' %in% class(caret_res))) {
        stop('Class type of supplied result is not "train"')
    }

    #generte plot_data dataframe which is the top n features
    plot_data <- multivariateAnalysisVarImp(caret_res) %>%
        filter(idx <= n) %>%
        arrange(Imp)

    #do the plot
    p <- ggplot ( plot_data , aes(y=Imp, x=feature_name)) +
       scale_x_discrete(limits= plot_data$feature_name) +
        coord_flip() +
        theme_bw() +
        geom_point(colour="black", size = 6) +
        geom_point(size=5, aes(colour=Imp)) +
        scale_color_gradient2(low='blue', mid='white', high='red') +
        theme(axis.text.x = element_text(size=rel(1), angle=330, hjust=0, vjust=1),
                   axis.text.y = element_text(size=rel(1))) +
        ggtitle(sprintf('Top %s Features for %s', nrow(plot_data), attr(caret_res, 'resp_var')))
    return(p)

}
