#
#
# This is the function countKeywords that counts
# the frequency of terms from a dictionary 
# in a corpus. 
#
#' @import ggplot2
#' @import igraph
#' @import stringi
#' @importFrom reshape2 melt
#' @importFrom ggtext element_markdown
#' @importFrom scales rescale
#' @export
plotNetCentrality <- function(g,
                              title="Network Centrality Measures", 
                              subtitle="Ranking of nodes according to centrality.",
                              caption="Source: Own elaboration.",
                              palette=c("#1B9E77", 
                                        "#D95F02", 
                                        "#7570B3", 
                                        "#E7298A", 
                                        "#66A61E", 
                                        "#E6AB02", 
                                        "#A6761D", 
                                        "#666666"), 
                              methods=c("degree",
                                        "authority",
                                        "page_rank",
                                        "eigenvector",
                                        "betweenness",
                                        "closeness",
                                        "hub"),
                              topn=20,
                              ties.method="min"){
  
  
  ck <- methods%in%c("degree",
                     "authority",
                     "page_rank",
                     "eigenvector",
                     "betweenness",
                     "closeness",
                     "hub")
  
  
  if(FALSE%in%ck){
    stop(simpleError("methods must be one of degree, authority, page_rank, eigenvector, betweenness, closeness or hub."))
  } 
  
  # Calcula las medidas de centralidaad
  nm<- names(igraph::V(g))
  
  if (is.null(nm)){
    
    nm <- 1:length(g)
  }
  
  lis <- methods
  
  directed <- igraph::is_directed(g)
  
  if(directed==TRUE){
    mode <- "in"
  }else{
    mode <- "all"
  }
  
  clo <- vector()
  clo <- igraph::closeness(g, mode = mode, normalized = T)
  clo[is.nan(clo)] <- 0
  
  # Medidas de centralidad
  ce <- data.frame(
    Name=nm,
    degree_conectividad=round(igraph::degree(g, mode = mode)),
    betweenness_moderacion=round(igraph::betweenness(g, directed=directed),1),
    closeness=round(clo,2),
    eigenvector_prestigio=round(igraph::eigen_centrality(g, directed = directed)$vector,2),
    page_rank_norm_prestigio=round(igraph::page_rank(g, directed = directed)$vector,2),
    hub=round(igraph::hub_score(g)$vector,2),
    authority=round(igraph::authority_score(g)$vector,2)
  )
  
  
  # Medidas normalizadas de centralidad
  dx <- data.frame(Name = ce$Name,
                   degree = scales::rescale(ce$degree_conectividad, to = c(0,1)),
                   betweenness = scales::rescale(ce$betweenness_moderacion, to = c(0,1)),
                   closeness=scales::rescale(ce$closeness, to = c(0,1)),
                   eigenvector = scales::rescale(ce$eigenvector_prestigio, to = c(0,1)),
                   page_rank = scales::rescale(ce$page_rank_norm_prestigio, to = c(0,1)),
                   hub=scales::rescale(ce$hub, to = c(0,1)),
                   authority=scales::rescale(ce$authority, to = c(0,1)))
  
  
  
  dx$overall <- rowMeans(dx[,2:ncol(dx)])
  
  
  # Rankings de cada centralidad
  Df <- data.frame(Name = dx$Name,
                   overall=rank(-dx$overall, ties.method = ties.method),
                   degree = rank(-dx$degree, ties.method = ties.method),
                   betweenness = rank(-dx$betweenness, ties.method = ties.method),
                   closeness = rank(-dx$closeness, ties.method = ties.method),
                   eigenvector = rank(-dx$eigenvector, ties.method = ties.method),
                   page_rank = rank(-dx$page_rank, ties.method = ties.method),
                   hub = rank(-dx$hub, ties.method = ties.method),
                   authority = rank(-dx$authority, ties.method = ties.method)
  )
  
  dx <- dx[, c("Name", "overall", lis)]
  Df <- Df[, c("Name", "overall", lis)]
  
  
  if(ncol(dx)>3){
    dx$overall <- rowMeans(dx[,3:ncol(dx)])
  }else{
    dx$overall <- dx[,3]
  }
  
  Df$overall <-  rank(-dx$overall, ties.method = ties.method)
  
  #Ordena segun la centralidad de grado 
  dx <- dx[order(dx$overall, decreasing = T),]
  lev <- dx$Name
  
  # Ordena segun el orden de personajes de dx
  Df <- Df[match(lev, Df$Name),]
  
  if(topn > nrow(dx)) topn <- nrow(dx)
  
  dx <- dx[1:topn,]
  Df <- Df[1:topn,]
  
  # Reestructura los datos para ggplot2
  Df <- reshape2::melt(Df, id.vars = "Name", measure.vars = 2:ncol(Df))
  names(Df) <- c("Name", "keys", "values")
  Df$keys <- factor(Df$keys)
  Df <- Df[order(Df$keys), ]
  
  # Reestructura los datos para ggplot2
  dx <- reshape2::melt(dx, id.vars = "Name", measure.vars = 2:ncol(dx))
  names(dx) <- c("Name", "keys", "values")
  dx$keys <- factor(dx$keys)
  dx <- dx[order(dx$keys), ]
  
    
  dx$rank <- Df$values
  
  # Convierte los nombres de los personajes en un factor
  dx$keys <- gsub("_", " ", dx$keys)
  dx$keys <- stringi::stri_trans_totitle(dx$keys)
  
  nma <- unique(dx$keys)
  
  dx$keys <- factor(dx$keys, levels = nma, labels = nma, ordered = F)
  
  dx$Name <- factor(dx$Name, levels = lev, labels = lev, ordered = T)
  
  co <- length(unique(dx$keys))  
  
  ggplot2::ggplot(dx, ggplot2::aes_string("Name", "values")) +
    ggplot2::geom_col(ggplot2::aes_string(fill = "keys")) +
    ggplot2::facet_wrap(~ keys, nrow = 1, ncol = co) +
    ggplot2::coord_flip(clip = "off")+
    ggplot2::theme_minimal()+
    ggplot2::geom_hline(yintercept = c(0.25,0.5,0.75,1.1), color="white")+
    ggplot2::theme(legend.position = "none",
                   plot.title=ggtext::element_markdown(size=18, 
                                                       face="bold"),
                   plot.subtitle=ggtext::element_markdown(),
                   plot.caption=ggtext::element_markdown(),
                   panel.grid = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size=12),
                   strip.text.x = ggplot2::element_text(size = 12)
    )+
    ggplot2::scale_fill_manual(values=palette)+
    ggplot2::ylab("")+
    ggplot2::xlab("")+
    ggplot2::labs(title = title, 
                  subtitle = subtitle,
                  caption= caption)+
    ggplot2::geom_label(ggplot2::aes(label=rank),
                        hjust=0.5, 
                        size=5,
                        label.r=ggplot2::unit(8,"pt"))+
    ggplot2::scale_x_discrete(limits=rev)
  
  
}
