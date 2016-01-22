#' univariateVolcanoPlot
#' Makes a volcano plot from the output of the univariateAnalysis function
#' @param df Output data from from CancerCellLinesModelling::univariateAnalysis
#' @param pval_th p-value threshold (unlogged)
#' @param effect_th effect threshold (unlogged - ie 2 = 2 fold up or down)
#' @return plot
#' @export
#' @import dplyr ggplot2 scales ggrepel

univariateVolcanoPlot <- function (ua_df, pval_th=NULL, effect_th=2, use_fdr=FALSE, repel_option=FALSE) {

    #put effect_th on log scale
    effect_th <- log10(effect_th)

    #suitable pval_th
    if(!is.numeric(pval_th)) {
        pval_th <- ifelse(use_fdr, 0.2, 0.05)
    }

    #define required pvalue
    if(use_fdr) {
        volcano_data <- ua_df %>% dplyr::mutate(pval_selected = FDR)
    } else {
        volcano_data <- ua_df %>% dplyr::mutate(pval_selected = p.value)
    }

    #define which genetic features exceed the thresholds set
    volcano_data <- volcano_data %>%
        dplyr::filter(!is.na(FDR)) %>%
        dplyr::mutate(effect_class = ifelse(estimate < -effect_th & pval_selected < pval_th, 'neg',
                              ifelse(estimate > effect_th & pval_selected < pval_th, 'pos', 'same')))

    #make a seperate data frame for the labels
    volcano_data_geomtext <- volcano_data %>% filter(effect_class != 'same')

    #define a local function to plot the y axis as we want it
    reverselog_trans <- function(base = exp(1)) {
        trans <- function(x) -log(x, base)
        inv <- function(x) base^(-x)
        trans_new(paste0("reverselog-", format(base)), trans, inv,
                  log_breaks(base = base),
                  domain = c(1e-100, Inf))
    }

    #do the volcano plot
    volcano_plot <- ggplot ( volcano_data , aes (x=effect_size_unlog, y=pval_selected, fill=effect_class, size=log10(gt1))) + geom_point(aes(shape=feature_type))

    #make sure we don't try to do repelling labels with too many points...
    if (nrow(volcano_data_geomtext) < 100 & repel_option) {
        volcano_plot <- volcano_plot +
            geom_text_repel(data = volcano_data_geomtext, aes(label=assayed_id, colour=effect_class),
                            size=5, force = 5, max.iter = 1e4,
                            box.padding = unit(0.35, "lines"),
                            point.padding = unit(0.3, "lines"))
    } else {
        volcano_plot <- volcano_plot +
            geom_text(data = volcano_data_geomtext, aes(label=assayed_id, colour=effect_class), size=4, hjust=-0.3, vjust=-0.4, angle=0)
    }

    #finish off the plot
    volcano_plot <- volcano_plot +
        geom_vline(xintercept = c(10^(-effect_th),10^effect_th) , color='blue', alpha=0.5, linetype='dotted') +
        geom_hline(yintercept = pval_th, color='blue', alpha=0.5, linetype='dotted') +
        scale_y_continuous(trans=reverselog_trans(10)) +
        scale_fill_manual (values=c('lightgreen', 'orange', 'lightgray'), limits=c('neg', 'pos', 'same')) +
        scale_colour_manual (values = c('darkgreen', 'red', 'blue'), limits=c('neg', 'pos', 'same')) +
        scale_shape_manual(values = c(21,22,23), limits = c("hybcap", "cosmicclp", "custom")) +
        scale_size_continuous(range=c(3,9)) +
        scale_x_log10(limits=c(min(volcano_data$effect_size_unlog,0.1),max(10,volcano_data$effect_size_unlog))) +
        xlab("IC50 Ratio") + ylab(ifelse(use_fdr, 'FDR', 'P value')) +
        facet_wrap( ~ resp_id) +
        theme_bw()  + theme(legend.position="none",
                            axis.text = element_text(size=rel(1.5)),
                            axis.title = element_text(size=rel(2)),
                            strip.text = element_text(size=rel(1.7)))

    return(volcano_plot)



}
