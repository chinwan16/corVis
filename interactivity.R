### interactivity

library(crosstalk)
library(plotly)

# generally speaking, use a "unique" key for filter,
# especially when you have multiple filters!
tx <- highlight_key(txhousing)
gg <- ggplot(tx) + geom_line(aes(date, median, group = city))
filter <- bscols(
  filter_select("id", "Select a city", tx, ~city),
  ggplotly(gg, dynamicTicks = TRUE),
  widths = c(12, 12)
)

tx2 <- highlight_key(txhousing, ~city, "Select a city")
gg <- ggplot(tx2) + geom_line(aes(date, median, group = city))
select <- highlight(
  ggplotly(gg, tooltip = "city"),
  selectize = TRUE, persistent = TRUE
)

bscols(filter, select)

# interactive correlation heatmap

library(plotly)
library(htmltools)
library(htmlwidgets)

# compute correlation matrix
correlation <- round(cor(mtcars), 3)

customdata <- setNames(lapply(mtcars, function(var) {
  setNames(lapply(mtcars, function(var2) {
    list(x = var, y = var2)
  }), NULL)
}), NULL)

p <- plot_ly() %>%
  add_heatmap(
    x = names(mtcars),
    y = names(mtcars),
    z = correlation,
    customdata = customdata
  ) %>%
  onRender(
    "function(el, x) {
       el.on('plotly_click', function(d) {
          var cdata = d.points[0].customdata;
          cdata.mode = 'markers';
          cdata.type = 'scattergl';
          Plotly.newPlot('filtered-plot', [cdata]);
       })
    }"
  )

browsable(tagList(p,
                  tags$div(id = 'filtered-plot')
))


d <- iris
assoc_d <- calc_assoc(d)

customdata <- setNames(lapply(d, function(var) {
  setNames(lapply(d, function(var2) {
    list(x = var, y = var2)
  }), NULL)
}), NULL)

p <- association_heatmap(assoc_d)

p_int <- p %>%
  ggplotly(customdata=customdata)  %>%
  onRender(
    "function(el, x) {
       el.on('plotly_click', function(d) {
          var cdata = d.points[0].customdata;
          cdata.mode = 'markers';
          cdata.type = 'scattergl';
          Plotly.newPlot('filtered-plot', [cdata]);
       })
    }"
  )

browsable(tagList(p_int,
                  tags$div(id = 'filtered-plot')
))


#### interactive association heatmap with variable names when hover over tiles

association_heatmap_int <- function(lassoc, uassoc=NULL, glyph = c("square","circle"), var_order = "default", limits=c(-1,1)){

  glyph = match.arg(glyph)
  vartypes <- attr(lassoc,"vartypes")

  if (isTRUE(var_order == "default")){
    var_order <- order_assoc(lassoc, method=var_order)
  } else var_order <- unique(c(lassoc$y, lassoc$x))
  if (is.null(uassoc))
    assoc <- sym_assoc(lassoc) else {
      names(uassoc)[1:2] <- names(uassoc)[2:1]
      assoc <-rbind(lassoc, uassoc)
    }


  if (is.null(limits)) {
    limits <- range(lassoc$measure, na.rm=TRUE)
    limits <- range(labeling::rpretty(limits[1], limits[2]))
  }

  assoc$x <- factor(assoc$x, levels=var_order)
  assoc$y <- factor(assoc$y, levels=var_order)

  diag_df <- assoc[1:length(var_order),]
  diag_df$x <- diag_df$y <- var_order
  diag_df$measure <- NA
  diag_df$intercept <- NA
  diag_df$text <- diag_df$x
  diag_df$var_type <- vartypes[var_order]
  assoc$text <- NA
  assoc$intercept <- 0
  assoc$var_type <- NA
  assoc <- rbind(assoc, diag_df)
  assoc$text_int <- paste0("x: ", assoc$y,
           "\n", "y: ", assoc$x,
           "\n", "measure_type: ",assoc$measure_type,
           "\n", "measure: ",round(assoc$measure,2))

  p <- ggplot2::ggplot(assoc) +

    ggplot2::facet_grid(ggplot2::vars(.data$x), ggplot2::vars(.data$y)) +
    ggplot2::geom_text(ggplot2::aes(x=0,y=0,label=.data$text,color=.data$var_type),size=3)+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
                       color="grey",alpha=0) +
    #ggplot2::geom_hline(ggplot2::aes(yintercept=intercept), size=0.5) +
    #viridis::scale_fill_viridis(option="inferno",direction = -1,na.value=NA,limits=limits) +
    ggplot2::scale_fill_gradient2(low="blue", mid="white", high="brown",na.value=NA,limits=limits) +
    ggplot2::scale_color_hue(guide = "none") +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 5),
                   panel.background = ggplot2::element_rect(fill="white"),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.spacing = ggplot2::unit(0,'lines'),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   legend.position = "bottom",
                   axis.title.x = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   strip.text.y = ggplot2::element_blank(),
                   strip.text.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   aspect.ratio = 1)

  if (glyph=="square"){
    p <- p+
      ggplot2::coord_cartesian(xlim = c(-0.5,0.5), ylim = c(-0.5,0.5)) +
      ggplot2::geom_rect(ggplot2::aes(xmin = -sqrt(abs(.data[["measure"]]))/2,
                                      xmax = sqrt(abs(.data[["measure"]]))/2,
                                      ymin = -sqrt(abs(.data[["measure"]]))/2,
                                      ymax = sqrt(abs(.data[["measure"]]))/2,
                                      fill = .data[["measure"]],
                                      text = text_int),na.rm = TRUE)
  } else {
    p <- p +
      ggforce::geom_circle(ggplot2::aes(x0 = 0, y0 = 0, r = sqrt(abs(.data[["measure"]])/pi),
                                        fill = .data[["measure"]],
                                        text = text_int))
  }

  p <- plotly::ggplotly(p, tooltip = "text")

  suppressWarnings(print(p))
}

d <- dat
types <- update_assoc(ordered_pair = "tbl_gkGamma")
assoc_d <- calc_assoc(d,types=types)
association_heatmap_int(assoc.obesity)




##### interactivity using shiny

library(ggplot2)


ui <- fluidPage(
  # Some custom CSS for a smaller font for preformatted text
  tags$head(
    tags$style(HTML("
      pre, table.table {
        font-size: smaller;
      }
    "))
  ),

  fluidRow(
    column(width = 4, wellPanel(
      radioButtons("plot_type", "Plot type",
                   c("base", "ggplot2")
      )
    )),
    column(width = 4,
           # In a plotOutput, passing values for click, dblclick, hover, or brush
           # will enable those interactions.
           plotOutput("plot1", height = 350,
                      # Equivalent to: click = clickOpts(id = "plot_click")
                      click = "plot_click",
                      dblclick = dblclickOpts(
                        id = "plot_dblclick"
                      ),
                      hover = hoverOpts(
                        id = "plot_hover"
                      ),
                      brush = brushOpts(
                        id = "plot_brush"
                      )
           )
    )
  ),
  fluidRow(
    column(width = 3,
           verbatimTextOutput("click_info")
    ),
    column(width = 3,
           verbatimTextOutput("dblclick_info")
    ),
    column(width = 3,
           verbatimTextOutput("hover_info")
    ),
    column(width = 3,
           verbatimTextOutput("brush_info")
    )
  )
)


server <- function(input, output) {
  output$plot1 <- renderPlot({
    if (input$plot_type == "base") {
      plot(mtcars$wt, mtcars$mpg)
    } else if (input$plot_type == "ggplot2") {
      ggplot(mtcars, aes(wt, mpg)) + geom_point()
    }
  })

  output$click_info <- renderPrint({
    cat("input$plot_click:\n")
    str(input$plot_click)
  })
  output$hover_info <- renderPrint({
    cat("input$plot_hover:\n")
    str(input$plot_hover)
  })
  output$dblclick_info <- renderPrint({
    cat("input$plot_dblclick:\n")
    str(input$plot_dblclick)
  })
  output$brush_info <- renderPrint({
    cat("input$plot_brush:\n")
    str(input$plot_brush)
  })

}


shinyApp(ui, server)



assoc_spellman
assoc_iris <- calc_assoc(iris)
ggplot(assoc_iris) + geom_tile(aes(x=x,y=y,fill=measure))

ui <- fluidPage(
  fluidRow(plotOutput("plot", click = "plot_click")),
  fluidRow(
    verbatimTextOutput("panel1"),
    verbatimTextOutput("panel2")
  ),
  fluidRow(plotOutput("scatter"))
  )

server <- function(input, output,session) {

  p <- association_heatmap(assoc_iris)
  output$plot <- renderPlot({

    p

      })

  p1 <- eventReactive(input$plot_click,{
    x <- input$plot_click$panelvar1
    y <- input$plot_click$panelvar2
    list(x=x,y=y)
  })

  output$panel1 = renderText(input$plot_click$panelvar1)
  output$panel2 = renderText(input$plot_click$panelvar2)
  output$scatter <- renderPlot({
    ggplot(data=iris) + geom_point(aes(x=iris[,p1()$x],y=iris[,p1()$y])) + xlab(p1()$x) +
      ylab(p1()$y)
  })


  }

shinyApp(ui, server)


p <- eventReactive(input$plot_click,{
  levs_x <- levels(factor(assoc_iris$x))
  x <- levs_x[round(input$plot_click$x)]
  levs_y <- levels(factor(assoc_iris$y))
  y <- levs_y[round(input$plot_click$y)]
  list(x=x,y=y)
})





############## creating a function for plotting the association

show_assoc <- function(d, x, y, by = NULL){



  if ( is.numeric(d[[x]]) & is.numeric(d[[y]]) ) {
    p <- ggplot2::ggplot(data=d) +
      ggplot2::geom_point(ggplot2::aes(x = .data[[x]], y = .data[[y]])) + xlab(x) + ylab(y) +
      {if(!is.null(by)) ggplot2::facet_wrap(~.data[[by]])}

  } else if ( is.factor(d[[x]]) & is.factor(d[[y]]) ) {

    p <- ggplot2::ggplot(data=d) +
      ggmosaic::geom_mosaic(ggplot2::aes(x = ggmosaic::product(.data[[x]], .data[[y]]), fill= .data[[x]] )) +
      xlab(x) + ylab(y) + {if(!is.null(by)) ggplot2::facet_wrap(~.data[[by]])}

  } else {

    p <- ggplot2::ggplot(data=d) +
      ggplot2::geom_boxplot(ggplot2::aes(x =.data[[x]], y =.data[[y]]) ) + xlab(x) +
      ylab(y) + {if(!is.null(by)) ggplot2::facet_wrap(~.data[[by]])}
  }

  print(p)

}









