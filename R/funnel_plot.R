#' Display a linear layout plot for correlations between a designated response and the independent variables
#' grouped by a grouping variable
#'
#' creates a funnel-like structure for correlations with a designated response
#'
#' @param measure_df dataframe
#' @param response character vector as response
#'
#' @return plot
#' @export
#'
#' @examples
#' funnel_plot(measure_df,response)

funnel_plot <- function(measure_df,response=NULL){

  if(is.null(response)){
    names(measure_df)[3] <- "measure"
    measure_df$var3 <- paste0(measure_df$var1, sep=" : ", measure_df$var2)
    measure_df <- dplyr::arrange(measure_df,desc(abs(measure)))
    measure_df$var3 <- forcats::fct_inorder(measure_df$var3)

    ggplot2::ggplot(data=measure_df) +
      ggplot2::geom_point(ggplot2::aes(x=var3,y=measure,colour=group_by)) +
      ggplot2::ylim(-1,1) +
      ggplot2::coord_flip() +
      ggplot2::scale_x_discrete(limits=rev) +
      ggplot2::theme(legend.position = "bottom",
                     axis.title.y  = ggplot2::element_blank())
  } else{

    m <- measure_df
    names(m)[3] <- "measure"
    n <- m
    names(n)[1:2] <- c("var2","var1")
    measure_df_mod <- rbind(m,n)

    measure_w_res <- dplyr::filter(measure_df_mod, var1==response)
    measure_w_res <- dplyr::arrange(measure_w_res,desc(abs(measure)))
    measure_w_res$var2 <- forcats::fct_inorder(measure_w_res$var2)

    ggplot2::ggplot(data=measure_w_res) +
      ggplot2::geom_point(ggplot2::aes(x=var2,y=measure,colour=group_by)) +
      ggplot2::ylim(-1,1) +
      ggplot2::coord_flip() +
      ggplot2::scale_x_discrete(limits=rev)+
      ggplot2::ggtitle(paste0("plot with response = ", response)) +
      ggplot2::theme(legend.position = "bottom")

  }

}
