# #'@importFrom ggplot2 qplot
#setGeneric("qplot")
#setGeneric("plot")

# #' @param varLabels A vector of character string labels for the variables used in
# #' \code{pcadscObj}. If non-null, these labels appear in the plot instead of the
# #' variable names. Note that they must be listed in the same order as the variables
# #' in \code{pcadscObj} and this order can be expected by calling
# #' \code{pcadscObj$varNames}. If \code{NULL} (the default), these variable
# #' names are used.
# #'
# #' @param covCO Not implemented, something like a variance contribution cut-off, maybe?
# #'
# #' @param splitLabels Labels for the two categories of the splitting variable used
# #' to create the PCADSC object, \code{pcadscObj}, given as a named list (see examples).
# #' These labels will appear in the headers of the two PCADSC plots.
# #' If \code{NULL} (the default), the original levels of the splitting variable
# #' are used.
# #'
# #' @param hideCumVar If \code{TRUE} (the default), cummulated explained variance
# #' percentages are not shown for each component.
# #'
# #' @importFrom methods setMethod
# #' @importFrom ggplot2 qplot ggplot aes_string geom_bar coord_flip scale_x_reverse
# #'             scale_y_continuous geom_text xlab ylab theme facet_wrap
# #'             as_labeller scale_fill_discrete theme_bw
# #'
# #'@describeIn PCADSC Plot a PCADSC object
# #'@include PCADSC.R
# #'@include pancakePlot.R
# #'@export
# setMethod("qplot","PCADSC", pancakePlot)


# #'@rdname qplot,PCADSC-method
# setMethod("plot", "PCADSC", function(x, y = NULL, ...) qplot(x, ...))
