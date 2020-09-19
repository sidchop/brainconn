#' workhorse function for \code{brainconn()}
#'
#' returns a ggraph object of plotted brain connectivity matrix
#' @author Sidhant Chopra
#' @import ggraph
#' @importFrom ggplot2 aes annotation_custom




build_plot <- function(conmat, data, data.row=NULL, data.col=NULL, background, node.size, node.color="network",
                       thr=NULL, uthr=NULL, view, edge.color, edge.alpha, edge.width, show.legend, label.size,
                       labels, include.vec=NULL, scale.edge.width, edge.color.weighted, ...) {


  if (view =="top"){
    x.mni<-data$x.mni
    y.mni<-data$y.mni
    depth <- data$z.mni
    xmax = 70  #70
    xmin = -75 #-70
    ymax = 73 #73
    ymin = -107 #-107
  }

  if (view =="bottom"){
    x.mni<-data$x.mni*-1
    y.mni<-data$y.mni
    depth <- data$z.mni*-1
    xmax = 70
    xmin = -70
    ymax = 73
    ymin = -107
  }

  if (view =="front"){
    x.mni<-data$x.mni
    y.mni<-data$z.mni
    depth <- data$y.mni
    xmax = 70
    xmin = -70
    ymax = 80
    ymin = -48
  }


  if (view =="back"){
    x.mni<-data$x.mni*-1
    y.mni<-data$z.mni
    depth <- data$y.mni*-1
    xmax = 70
    xmin = -70
    ymax = 80
    ymin = -48
  }


  if (view =="left"){
    x.mni<-data$y.mni*-1
    y.mni<-data$z.mni
    depth <- data$x.mni
    xmax = 103
    xmin = -72
    ymax = 77
    ymin = -50
  }

  ##fix below
  if (view =="right"){
    x.mni<-data$y.mni
    y.mni<-data$z.mni
    depth <- data$x.mni*-1
    xmax = 103
    xmin = -140
    ymax = 77
    ymin = -50
  }


  #is matrix directed (i.e. symetric)
  ifelse(isSymmetric.matrix(conmat)==TRUE,
         directed <- FALSE,
         directed <- TRUE)

  #is matrix weighed
  ifelse(all(conmat %in% c(0,1))==TRUE,
         weighted <- FALSE,
         weighted <- TRUE)

  #should edges be colored bby weight

#  ifelse(edge.color=="weight", edge.color.weighted <- T, edge.color.weighted <- F)

  if (!exists("conmat")) stop(print("Please enter a valid connectivity matrix"))
  if (!is.null(thr)) {conmat[conmat < thr] <- 0} #lower threshold graph
  if (!is.null(uthr)) {conmat[conmat > thr] <- 0} #upper threshold graph


  if(directed == F) {
    layout <- ggraph::create_layout(graph = conmat, layout ="stress", circular=TRUE)
    layout$x <- x.mni
    layout$y <- y.mni
    layout
  }



  if(directed == T) {
    layout <- ggraph::create_layout(graph = conmat, layout ="stress", circular=TRUE)
    layout$x <- x.mni
    layout$y <- y.mni
    layout$facet <- include.vec
  }


  #make graph

  if(directed == T && weighted==F){p <- ggraph::ggraph(layout) +
    ggplot2::annotation_custom(background, xmax = xmax ,xmin = xmin , ymax = ymax , ymin = ymin ) +
    ggraph::geom_edge_parallel(color=edge.color,
                       edge_width = edge.width,
                       edge_alpha = edge.alpha,
                       arrow = grid::arrow(length = grid::unit(3, 'mm')),
                       end_cap = ggraph::circle((node.size/2)+0.6, 'mm'))  +
    #  ggraph::geom_edge_loop0(aes(strength=node.size*3), color=edge.color, edge_width = edge.width, arrow = arrow(length = unit(1, 'mm'))) +
    ggplot2::coord_fixed(xlim = c(-70,70), ylim = c(-107,73))
  }

  if(directed == T && weighted==T && edge.color.weighted==F){p <- ggraph::ggraph(layout) +
    ggplot2::annotation_custom(background, xmax = xmax ,xmin = xmin , ymax = ymax , ymin = ymin ) +
    ggraph::geom_edge_parallel(aes(width=weight),
                        color=edge.color,
                       edge_alpha = edge.alpha,
                       arrow = grid::arrow(length = grid::unit(3, 'mm')),
                       end_cap = ggraph::circle(node.size/2, 'mm'))  +
    ggraph::geom_edge_loop0(aes(strength=node.size*3, width=weight),
                            color=edge.color,
                            edge_alpha = edge.alpha,
                            arrow = grid::arrow(length = grid::unit(3, 'mm'))) +
    ggplot2::coord_fixed(xlim = c(-70,70), ylim = c(-107,73))

  }

  if(directed == T && weighted==T && edge.color.weighted==T){p <- ggraph::ggraph(layout) +
    ggplot2::annotation_custom(background, xmax = xmax ,xmin = xmin , ymax = ymax , ymin = ymin ) +
    ggraph::geom_edge_parallel(aes(color=weight),
  #                            color=edge.color,
                               edge_alpha = edge.alpha,
                                edge_width = edge.width,
                               arrow = grid::arrow(length = grid::unit(3, 'mm')),
                               end_cap = ggraph::circle(node.size/2, 'mm'))  +
    ggraph::geom_edge_loop0(aes(strength=node.size*3, color=weight),
                            edge_width = edge.width,
                            edge_alpha = edge.alpha,
                            arrow = grid::arrow(length = grid::unit(3, 'mm'))) +
    ggplot2::coord_fixed(xlim = c(-70,70), ylim = c(-107,73))

  }



  if(directed == F && weighted==F){p <- ggraph::ggraph(layout, circular = FALSE) +
    ggplot2::annotation_custom(background, xmax = xmax ,xmin = xmin , ymax = ymax , ymin = ymin ) +
    ggraph::geom_edge_link(color=edge.color,
                   edge_width = edge.width,
                   edge_alpha = edge.alpha) +
    ggplot2::coord_fixed(xlim = c(-70,70), ylim = c(-107,73))
  }

  if(directed == F && weighted==T && edge.color.weighted==F){p <- ggraph::ggraph(layout, circular = FALSE) +
    ggplot2::annotation_custom(background, xmax = xmax ,xmin = xmin , ymax = ymax , ymin = ymin ) +
    ggraph::geom_edge_link(aes(width=weight),
                           color=edge.color,
                   edge_alpha = edge.alpha) +
    ggplot2::coord_fixed(xlim = c(-70,70), ylim = c(-107,73))
  }

  if(directed == F && weighted==T && edge.color.weighted==T){p <- ggraph::ggraph(layout, circular = FALSE) +
    ggplot2::annotation_custom(background, xmax = xmax ,xmin = xmin , ymax = ymax , ymin = ymin ) +
    ggraph::geom_edge_link(aes(colour=weight),
                           edge_width = edge.width,
                           edge_alpha = edge.alpha) +
    #  geom_edge_loop0(aes(strength=node.size*2), color=edge.color, edge_width = edge.width) +
    ggplot2::coord_fixed(xlim = c(-70,70), ylim = c(-107,73))
  }



  ##scale edge weight for weighted networks
  if (weighted==T && !is.null(scale.edge.width)){
    p <- p + ggraph::scale_edge_width(range = scale.edge.width)
  }




  #adjust xylim for left and right views --- probably can get rid of this with correct inital placement ratios
  if(view=="left") {
    p <- p + ggplot2::coord_fixed(xlim = c(-64,98), ylim = c(-44,76)) }
  if(view=="right") {
    p <- p + ggplot2::coord_fixed(xlim = c(-98,64), ylim = c(-44,76)) }

#set node size with degree option  #### NOT WORKING ###
  #ifelse(node.size=="degree", node.size <- as.vector((degree(graph_from_adjacency_matrix(conmat)))*0.2), node.size <- node.size)

#add nodes
  if(directed == T){
    ifelse(node.color=="network",
           p <- p + ggraph::geom_node_point(size=node.size, aes(colour=as.factor(data$network), filter = as.logical(facet))),
           p <- p + ggraph::geom_node_point(size=node.size, colour=node.color))
  }
  if(directed == F){
    ifelse(node.color=="network",
           p <- p + ggraph::geom_node_point(size=node.size, aes(colour=as.factor(data$network))),
           p <- p + ggraph::geom_node_point(size=node.size, colour=node.color))
  }


  ## add labs
  if(directed == T && labels==T){
    p <- p + ggraph::geom_node_text(aes(label = data$ROI.Name, filter = as.logical(facet)),
                            size=label.size, repel=TRUE,
                            nudge_x = node.size+2, nudge_y = node.size)
  }


  if (directed == F && labels==T){
    p <- p + ggraph::geom_node_text(aes(label = data$ROI.Name),
                            size=label.size, repel=TRUE,
                            nudge_x = node.size+2, nudge_y = node.size)
  }

#remove gridlines
  p <- p + ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          axis.title.x=ggplot2::element_blank(),
          axis.text.x=ggplot2::element_blank(),
          axis.ticks.x=ggplot2::element_blank(),
          axis.title.y =ggplot2::element_blank(),
          axis.text.y=ggplot2::element_blank(),
          axis.ticks.y=ggplot2::element_blank(),

    )
  #legend
  if (show.legend==F){p <- p + ggplot2::theme(legend.position="none")}
  if (show.legend==T){p <- p + ggplot2::scale_color_discrete(name="Network")}


p

}
