#function to build static plot
#featuers to add, optional ledgend for network, optional label
build_plot <- function(conmat, data, data.row=NULL, data.col=NULL, background, node.size, node.color="network",
                       thr=NULL, uthr=NULL, view, edge.color, edge.alpha, edge.width, show.legend, label.size,
                       labels, include.vec=NULL, scale.edge.width, ...) {
  require(ggraph)
  require(ggplot2)
  require(igraph)

  if (view =="top"){
    x.mni<-data$x.mni
    y.mni<-data$y.mni
    depth <- data$z.mni
    xmax = 70
    xmin = -70
    ymax = 73
    ymin = -107
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

  if (!exists("conmat")) stop(print("Please enter a valid connectivity matrix"))
  if (!is.null(thr)) {conmat[conmat < thr] <- 0} #lower threshold graph
  if (!is.null(uthr)) {conmat[conmat > thr] <- 0} #upper threshold graph


  if(directed == F) {
    layout <- create_layout(graph = conmat, layout ="stress", circular=TRUE)
    layout$x <- x.mni
    layout$y <- y.mni
    layout
    attributes(layout)
  }



  if(directed == T) {
    layout <- create_layout(graph = conmat, layout ="stress", circular=TRUE)
    layout$x <- x.mni
    layout$y <- y.mni
    layout$facet <- include.vec
    attributes(layout)
  }


  #make graph

  if(directed == T && weighted==F){p <- ggraph(layout) +
    annotation_custom(background, xmax = xmax ,xmin = xmin , ymax = ymax , ymin = ymin ) +
    geom_edge_parallel(color=edge.color,
                       edge_width = edge.width,
                       edge_alpha = edge.alpha,
                       arrow = arrow(length = unit(3, 'mm')),
                       end_cap = circle((node.size/2)+0.6, 'mm'))  +
    #  geom_edge_loop0(aes(strength=node.size*3), color=edge.color, edge_width = edge.width, arrow = arrow(length = unit(1, 'mm'))) +
    coord_fixed(xlim = c(-70,70), ylim = c(-107,73))
  }

  if(directed == T && weighted==T){p <- ggraph(layout) +
    annotation_custom(background, xmax = xmax ,xmin = xmin , ymax = ymax , ymin = ymin ) +
    geom_edge_parallel(color=edge.color,
                       aes(width = weight),
                       edge_alpha = edge.alpha,
                       arrow = arrow(length = unit(3, 'mm')),
                       end_cap = circle(node.size/2, 'mm'))  +
    geom_edge_loop0(aes(strength=node.size*3), color=edge.color, edge_width = edge.width, arrow = arrow(length = unit(3, 'mm'))) +
    coord_fixed(xlim = c(-70,70), ylim = c(-107,73))

  }


  if(directed == F && weighted==F){p <- ggraph(layout, circular = FALSE) +
    annotation_custom(background, xmax = xmax ,xmin = xmin , ymax = ymax , ymin = ymin ) +
    geom_edge_link(color=edge.color,
                   edge_width = edge.width,
                   edge_alpha = edge.alpha) +
    geom_edge_loop0(aes(strength=node.size*2), color=edge.color, edge_width = edge.width) +
    coord_fixed(xlim = c(-70,70), ylim = c(-107,73))
  }

  if(directed == F && weighted==T){p <- ggraph(layout, circular = FALSE) +
    annotation_custom(background, xmax = xmax ,xmin = xmin , ymax = ymax , ymin = ymin ) +
    geom_edge_link(color=edge.color,
                   aes(width = weight),
                   edge_alpha = edge.alpha) +
    geom_edge_loop0(aes(strength=node.size*2), color=edge.color, edge_width = edge.width) +
    coord_fixed(xlim = c(-70,70), ylim = c(-107,73))
  }


  ##scale edge weight for weighted networks
  if (weighted==T && !is.null(scale.edge.width)){
    p <- p + scale_edge_width(range = scale.edge.width)
  }




  #adjust xylim for left and right views
  if(view=="left") {
    p <- p + coord_fixed(xlim = c(-64,98), ylim = c(-44,76)) }
  if(view=="right") {
    p <- p + coord_fixed(xlim = c(-98,64), ylim = c(-44,76)) }

  #set node size with degree option  #### NOT WORKING ###
  #ifelse(node.size=="degree", node.size <- as.vector((degree(graph_from_adjacency_matrix(conmat)))*0.2), node.size <- node.size)

  #add nodes
  if(directed == T){
    ifelse(node.color=="network",
           p <- p + geom_node_point(size=node.size, aes(colour=data$network, filter = as.logical(facet))),
           p <- p + geom_node_point(size=node.size, colour=node.color))
  }
  if(directed == F){
    ifelse(node.color=="network",
           p <- p + geom_node_point(size=node.size, aes(colour=data$network)),
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





  #Add degree sizing
  #if(node.size=="degree"){ p <- p + geom_node_point(aes(size=as.vector((degree(graph_from_adjacency_matrix(x))))+1))}

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


  print(p)

}
