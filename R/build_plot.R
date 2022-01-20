#' workhorse function for \code{brainconn()}
#'
#' returns a ggraph object of plotted brain connectivity matrix
#' @author Sidhant Chopra
#' @import ggraph
#' @import ggplot2
#' @import grid




build_plot <- function(conmat, data, data.row=NULL, data.col=NULL, background, node.size, node.color="network",
                       thr=NULL, uthr=NULL, view, edge.color, edge.alpha, edge.width, show.legend, label.size,
                       labels, include.vec=NULL, scale.edge.width, edge.color.weighted, label.edge.weight, ...) {


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

  #should edges be colored by weight
#  ifelse(edge.color=="weight", edge.color.weighted <- T, edge.color.weighted <- F)

  if (!exists("conmat")) stop(print("Please enter a valid connectivity matrix"))
  if (!is.null(thr)) {conmat[conmat < thr] <- 0} #lower threshold graph
  if (!is.null(uthr)) {conmat[conmat > thr] <- 0} #upper threshold graph


  if(directed == F) {
    conmat[upper.tri(conmat)] <- 0 #only take bottom tri of matrix to stop the edge labels being plotted twice
    layout <- create_layout(graph = conmat, layout ="stress", circular=TRUE)
    layout$x <- x.mni
    layout$y <- y.mni
    layout
  }



  if(directed == T) {
    layout <- create_layout(graph = conmat, layout ="stress", circular=TRUE)
    layout$x <- x.mni
    layout$y <- y.mni
    layout$facet <- include.vec
  }


  #make graph

  if(directed == T && weighted==F){p <- ggraph(layout) +
    annotation_custom(background, xmax = xmax ,xmin = xmin , ymax = ymax , ymin = ymin ) +
    geom_edge_parallel(color=edge.color,
                       edge_width = edge.width,
                       edge_alpha = edge.alpha,
                       arrow = arrow(length = unit(3, 'mm')),
                       end_cap = circle((node.size/2)+0.6, 'mm'))  +
    #  ggraph::geom_edge_loop0(aes(strength=node.size*3), color=edge.color, edge_width = edge.width, arrow = arrow(length = unit(1, 'mm'))) +
    coord_fixed(xlim = c(-70,70), ylim = c(-107,73))
  }

  if(directed == T && weighted==T && edge.color.weighted==F && label.edge.weight==F){p <- ggraph(layout) +
    annotation_custom(background, xmax = xmax ,xmin = xmin , ymax = ymax , ymin = ymin ) +
    geom_edge_parallel(aes(width=weight),
                        color=edge.color,
                       edge_alpha = edge.alpha,
                       arrow = arrow(length = unit(3, 'mm')),
                       end_cap = circle(node.size/2, 'mm'))  +
    geom_edge_loop0(aes(strength=node.size*3, width=weight),
                            color=edge.color,
                            edge_alpha = edge.alpha,
                            arrow = arrow(length = unit(3, 'mm'))) +
    coord_fixed(xlim = c(-70,70), ylim = c(-107,73))

  }

 if(directed == T && weighted==T && edge.color.weighted==F && label.edge.weight==T){p <- ggraph(layout) +
    annotation_custom(background, xmax = xmax ,xmin = xmin , ymax = ymax , ymin = ymin ) +
    geom_edge_parallel(aes(width=weight, label=round(weight,3)),
                       color=edge.color,
                       edge_alpha = edge.alpha,
                       arrow = arrow(length = unit(3, 'mm')),
                       end_cap = circle(node.size/2, 'mm'),
                       angle_calc = 'along',
                       alpha = 0,
                       label_dodge = unit(2.5, 'mm'),
                       label_size = 2,
                       fontface = "bold")  +
    geom_edge_loop0(aes(strength=node.size*3, width=weight, label=round(weight,3)),
                    color=edge.color,
                    edge_alpha = edge.alpha,
                    arrow = arrow(length = unit(3, 'mm')),
                    angle_calc = 'none',
                    alpha = 0,
                    label_dodge = unit(6, 'mm'),
                    label_size = 2,
                    vjust = -1,
                    fontface = "bold") +
    coord_fixed(xlim = c(-70,70), ylim = c(-107,73))

  }

  if(directed == T && weighted==T && edge.color.weighted==T && label.edge.weight==F){p <- ggraph(layout) +
    annotation_custom(background, xmax = xmax ,xmin = xmin , ymax = ymax , ymin = ymin ) +
    geom_edge_parallel(aes(color=weight),
                       edge_alpha = edge.alpha,
                       edge_width = edge.width,
                       arrow = arrow(length = unit(3, 'mm')),
                       end_cap = circle(node.size/2, 'mm')) +
    geom_edge_loop(aes(strength=node.size*3, color=weight),
                   edge_width = edge.width,
                   edge_alpha = edge.alpha,
                   arrow = arrow(length = unit(3, 'mm'))) +
    coord_fixed(xlim = c(-70,70), ylim = c(-107,73))

  }



  if(directed == T && weighted==T && edge.color.weighted==T && label.edge.weight==T){p <- ggraph(layout) +
    annotation_custom(background, xmax = xmax ,xmin = xmin , ymax = ymax , ymin = ymin ) +
    geom_edge_parallel(aes(color=weight, label=round(weight,3)),
                       #color=edge.color,
                       edge_alpha = edge.alpha,
                       edge_width = edge.width,
                       arrow = arrow(length = unit(3, 'mm')),
                       end_cap = circle(node.size/2, 'mm'),
                       angle_calc = 'along',
                       alpha = 0,
                       label_dodge = unit(2.5, 'mm'),
                       label_size = 2,
                       fontface = "bold") +
    geom_edge_loop(aes(strength=node.size*3, color=weight, label=round(weight,3)),
                    edge_width = edge.width,
                    edge_alpha = edge.alpha,
                    arrow = arrow(length = unit(3, 'mm')),
                   angle_calc = 'none',
                   alpha = 0,
                   label_dodge = unit(6, 'mm'),
                   label_size = 2,
                   vjust = -1,
                   fontface = "bold") +
    coord_fixed(xlim = c(-70,70), ylim = c(-107,73))

  }



  if(directed == F && weighted==F){p <- ggraph(layout, circular = FALSE) +
    annotation_custom(background, xmax = xmax ,xmin = xmin , ymax = ymax , ymin = ymin ) +
    geom_edge_link(color=edge.color,
                   edge_width = edge.width,
                   edge_alpha = edge.alpha) +
    coord_fixed(xlim = c(-70,70), ylim = c(-107,73))
  }




  if(directed == F && weighted==T && edge.color.weighted==F && label.edge.weight==F){p <- ggraph(layout, circular = FALSE) +
    annotation_custom(background, xmax = xmax ,xmin = xmin , ymax = ymax , ymin = ymin ) +
    geom_edge_link(aes(width=weight),
                           color=edge.color,
                   edge_alpha = edge.alpha) +
    coord_fixed(xlim = c(-70,70), ylim = c(-107,73))
  }

  if(directed == F && weighted==T && edge.color.weighted==F && label.edge.weight==T){
    p <- ggraph(layout, circular = FALSE) +
    annotation_custom(background, xmax = xmax ,xmin = xmin , ymax = ymax , ymin = ymin ) +
    geom_edge_link(aes(width=weight, label=round(weight,3)),
                   color=edge.color,
                   edge_alpha = edge.alpha,
                   angle_calc = 'along',
                   alpha = 0,
                   label_dodge = unit(2.5, 'mm'),
                   label_size = 2,
                   fontface = "bold") +
    coord_fixed(xlim = c(-70,70), ylim = c(-107,73))
  }


  if(directed == F && weighted==T && edge.color.weighted==T && label.edge.weight==F){p <- ggraph(layout, circular = FALSE) +
    annotation_custom(background, xmax = xmax ,xmin = xmin , ymax = ymax , ymin = ymin ) +
    geom_edge_link(aes(colour=weight),
                           edge_width = edge.width,
                           edge_alpha = edge.alpha) +
    coord_fixed(xlim = c(-70,70), ylim = c(-107,73))
  }

  if(directed == F && weighted==T && edge.color.weighted==T && label.edge.weight==T){p <- ggraph(layout, circular = FALSE) +
    annotation_custom(background, xmax = xmax ,xmin = xmin , ymax = ymax , ymin = ymin ) +
    geom_edge_link(aes(colour=weight, label=round(weight,3)),
                   edge_width = edge.width,
                   edge_alpha = edge.alpha,
                   angle_calc = 'along',
                   alpha = 0,
                   label_dodge = unit(2.5, 'mm'),
                   label_size = 2,
                   fontface = "bold") +
    coord_fixed(xlim = c(-70,70), ylim = c(-107,73))
  }





  ##scale edge weight for weighted networks
  if (weighted==T && !is.null(scale.edge.width)){
    p <- p + scale_edge_width(range = scale.edge.width)
  }




  #adjust xylim for left and right views --- probably can get rid of this with correct inital placement ratios
  if(view=="left") {
    p <- p + coord_fixed(xlim = c(-64,98), ylim = c(-44,76)) }
  if(view=="right") {
    p <- p + coord_fixed(xlim = c(-98,64), ylim = c(-44,76)) }

#set node size with degree option  #### NOT WORKING ###
  #ifelse(node.size=="degree", node.size <- as.vector((degree(graph_from_adjacency_matrix(conmat)))*0.2), node.size <- node.size)

#add nodes
  if(directed == T){
    ifelse(node.color=="network",
           p <- p + geom_node_point(size=node.size, aes(colour=as.factor(data$network), filter = as.logical(facet))),
           p <- p + geom_node_point(size=node.size, colour=node.color))
  }
  if(directed == F){
    ifelse(node.color=="network",
           p <- p + geom_node_point(size=node.size, aes(colour=as.factor(data$network))),
           p <- p + geom_node_point(size=node.size, colour=node.color))
  }


  ## add labs
  if(directed == T && labels==T){
    p <- p + geom_node_text(aes(label = data$ROI.Name, filter = as.logical(facet)),
                            size=label.size, repel=TRUE,
                            nudge_x = node.size+2, nudge_y = node.size)
  }


  if (directed == F && labels==T){
    p <- p + geom_node_text(aes(label = data$ROI.Name),
                            size=label.size, repel=TRUE,
                            nudge_x = node.size+2, nudge_y = node.size)
  }

#remove gridlines
  p <- p + theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y =element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),

    )
  #legend
  if (show.legend==F){p <- p + theme(legend.position="none")}
  if (show.legend==T){p <- p + scale_color_discrete(name="Network")}


p

}
