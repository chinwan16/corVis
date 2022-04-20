#' Display a matrix layout plot for correlations between pairs of variables grouped by a grouping variable
#'
#' creates a pairwise plot of correlations for all the numeric variables grouped by a grouping variable
#'
#' @param data dataframe
#'
#' @return plot
#' @export
#'
#' @examples
#' bar_display(lower_tri)


bar_display <- function(lower_tri.measure, upper_tri.measure=NULL,order.by="overall") {

  if(order.by=="overall"){
    names(lower_tri.measure)[3] <- "measure"
    m <- lower_tri.measure
    cor_df_overall <- dplyr::filter(m,group_by == "overall")

    vars_df <- unique(unlist(cor_df_overall[,1:2]))
    m1 <- matrix(0, nrow=length(vars_df), ncol=length(vars_df),dimnames=list(vars_df, vars_df))
    m1[upper.tri(m1)] <- cor_df_overall$measure
    cor_matrix <- m1 + t(m1)
    diag(cor_matrix) <- 1

    cor_matrix_n <- 1-cor_matrix
    d <- stats::dist(cor_matrix_n)
    clust <- stats::hclust(d, method = "average")
    ordering <- rownames(cor_matrix)[clust$order]

  }else if(order.by=="max_diff"){

    names(lower_tri.measure)[3] <- "measure"
    m <- lower_tri.measure

    df_max_diff <- dplyr::group_by(m,var1,var2)
    df_max_diff <- dplyr::mutate(df_max_diff,max_diff=diff(range(measure)))
    df_max_diff_uni <- unique.data.frame(df_max_diff[,c("var1","var2","max_diff")])

    vars_df <- unique(unlist(df_max_diff_uni[,1:2]))
    m1 <- matrix(0, nrow=length(vars_df), ncol=length(vars_df),dimnames=list(vars_df, vars_df))
    m1[upper.tri(m1)] <- df_max_diff_uni$max_diff
    max_diff_matrix <- m1 + t(m1)

    d <- stats::dist(max_diff_matrix)
    clust <- stats::hclust(d, method = "average")
    ordering <- rownames(max_diff_matrix)[clust$order]

  }


  if(is.null(upper_tri.measure)){
    names(lower_tri.measure)[3] <- "measure"
    m <- lower_tri.measure
    lower_tri <- dplyr::filter(m,group_by != "overall")
    upper_tri <- lower_tri
    names(upper_tri)[1:2] <- c("var2","var1")

    cor_df <- rbind(upper_tri,lower_tri)
    cor_df$var1 <- factor(cor_df$var1,levels=ordering)
    cor_df$var2 <- factor(cor_df$var2,levels=ordering)

    lower_tri.overall_cor <- dplyr::filter(m,group_by == "overall")
    upper_tri.overall_cor <- lower_tri.overall_cor
    names(upper_tri.overall_cor)[1:2] <- c("var2","var1")


    overall_cor <- rbind(upper_tri.overall_cor,lower_tri.overall_cor)
    overall_cor$var1 <- factor(overall_cor$var1,levels=ordering)
    overall_cor$var2 <- factor(overall_cor$var2,levels=ordering)

    diag_label_df <- data.frame(var1=overall_cor$var1, var2=overall_cor$var1, text= overall_cor$var1)

    ggplot2::ggplot(cor_df) +
      ggplot2::facet_grid(ggplot2::vars(var1), ggplot2::vars(var2)) +
      ggplot2::geom_bar(ggplot2::aes(x=1,y=measure,fill=group_by),stat = "identity",position = "dodge") +
      ggplot2::geom_hline(data=overall_cor,ggplot2::aes(yintercept=measure),linetype="dashed")  +
      ggplot2::geom_hline(ggplot2::aes(yintercept=0), size=0.5) +
      #ggplot2::scale_y_continuous(limits=c(-1,1)) +
      ggplot2::geom_text(data=diag_label_df,ggplot2::aes(x=1,y=0,label=text),size=2.8) +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.text = ggplot2::element_text(size = 7),
                     panel.background = ggplot2::element_rect(),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "bottom",
                     axis.title.x = ggplot2::element_blank(),
                     strip.background = ggplot2::element_blank(),
                     strip.text.y = ggplot2::element_blank(),
                     strip.text.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank())
    }else{

      names(lower_tri.measure)[3] <- "measure"
      names(upper_tri.measure)[3] <- "measure"
      m1 <- dplyr::filter(lower_tri.measure,group_by != "overall")
      m2 <- dplyr::filter(upper_tri.measure,group_by != "overall")
      lower_tri <- m1
      upper_tri <- lower_tri
      names(upper_tri)[1:2] <- c("var2","var1")
      upper_tri$measure <- m2$measure

      cor_df <- rbind(upper_tri,lower_tri)

      m1_overall <- dplyr::filter(lower_tri.measure,group_by == "overall")
      m2_overall <- dplyr::filter(upper_tri.measure,group_by == "overall")
      lower_tri.overall_cor <- m1_overall
      upper_tri.overall_cor <- lower_tri.overall_cor
      names(upper_tri.overall_cor)[1:2] <- c("var2","var1")
      upper_tri.overall_cor$measure <- m2_overall$measure

      overall_cor <- rbind(upper_tri.overall_cor,lower_tri.overall_cor)

      ggplot2::ggplot(cor_df) +
        ggplot2::facet_grid(ggplot2::vars(var1), ggplot2::vars(var2)) +
        ggplot2::geom_bar(ggplot2::aes(x=1,y=measure,fill=group_by),stat = "identity",position = "dodge") +
        ggplot2::geom_hline(data=overall_cor,ggplot2::aes(yintercept=measure),linetype="dashed")  +
        ggplot2::geom_hline(ggplot2::aes(yintercept=0), size=0.5) +
        ggplot2::scale_y_continuous(limits=c(-1,1)) +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                       panel.background = ggplot2::element_rect(),
                       panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(),
                       legend.position = "bottom")
      }
  }
