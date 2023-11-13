#SYSTEMIC TRADE INTERCONNECTEDNESS
#Cunto, G. & Santana, D. (202X)

#SCRIPT 3: VISUALIZATIONS-----

##Load packages--------
library(tidyverse)
library(imfr)
library(rdbnomics)
library(countrycode)
library(igraph)
library(BBmisc)
library(ggraph)
library(graphlayouts)
library(corrplot)
library(circlize)
library(ComplexHeatmap)
library(grid)
library(gridBase)
library(rticles)

#NETWORK GRAPH-----

#Graficador 1######

year <- 2020
filtro <- 0.9
choose_layout <- "drl"
label_filter <- 80

graficador <- function(year, filtro = 0.9, choose_layout = "drl", label_filter = 80) {
  imf_graph <- graph_list[[as.character(year)]]
  #Filtrar conexiones
  imf_graph_2 <- delete_edges(imf_graph, which(percent_rank(E(imf_graph)$weight) < filtro))
  #Añadir atributos adicionales
  V(imf_graph_2)$rank <- imf_jurisdiction_scores_percentil$index_rank_per[imf_jurisdiction_scores_percentil$year == year]
  V(imf_graph_2)$index <- imf_jurisdiction_scores_percentil$index_final_per[imf_jurisdiction_scores_percentil$year == year]
  #Colores
  imf_nodes <- country_set %>%
    rename(vertices = iso3c) %>%
    select(vertices, country.name.en , region, weo_group_region)
  V(imf_graph_2)$group <- as.character(imf_nodes$region[match(V(imf_graph_2)$name,imf_nodes$vertices)])
  V(imf_graph_2)$color <- V(imf_graph_2)$group
  V(imf_graph_2)$color <- gsub("East Asia & Pacific","#D70036",V(imf_graph_2)$color)
  V(imf_graph_2)$color <- gsub("Europe & Central Asia","#003A5D",V(imf_graph_2)$color)
  V(imf_graph_2)$color <- gsub("Latin America & Caribbean" ,"#157549",V(imf_graph_2)$color)
  V(imf_graph_2)$color <- gsub("Middle East & North Africa","#F1BA0D",V(imf_graph_2)$color)
  V(imf_graph_2)$color <- gsub("North America","#00ACC8",V(imf_graph_2)$color)
  V(imf_graph_2)$color <- gsub("South Asia","#291E58",V(imf_graph_2)$color)
  V(imf_graph_2)$color <- gsub("Sub-Saharan Africa","#F26122",V(imf_graph_2)$color)
  #Eliminar Vertices
  isolated <- which(igraph::degree(imf_graph_2) == 0)
  imf_graph_3 <- delete.vertices(imf_graph_2, isolated)
  #Tamano nodos
  node_size_1 <- BBmisc::normalize(V(imf_graph_3)$index, method = "range",  range = c(0 , 1))
  node_size_2 <- (node_size_1)^2
  
  if(choose_layout != "centrality") {  #Graficar
    ggraph(imf_graph_3, layout = choose_layout) +
      geom_edge_link0(aes(edge_width = weight), edge_colour = "grey") +
      geom_node_point(aes(fill = region, size = node_size_2), shape = 21) +
      geom_node_text(aes(filter= index > label_filter, label = name, size = 0.01), family = "serif", color = "white") + 
      scale_edge_width_continuous(range = c(0.05, 0.9)) + 
      scale_size_continuous(range = c(2,15)) +
      scale_fill_manual(values = region_color) +
      theme_graph() +
      theme(legend.position = "none") +
      ggtitle("Interconectividad Comercial Sistémica",subtitle = year) +
      labs(caption = "Fuente: Cunto, G. & Santana, D. (2021). IMF DOTS (2021), IMF WEO (2021)")}  
  else {
    ggraph(imf_graph_3, layout = choose_layout, cent = eigen_vector_w) +
      geom_edge_link0(aes(edge_width = weight), edge_colour = "grey") +
      geom_node_point(aes(fill = region, size = node_size_2), shape = 21) +
      geom_node_text(aes(filter= index > label_filter, label = name, size = 0.01), family = "serif", color = "white") + 
      scale_edge_width_continuous(range = c(0.05, 0.9)) + 
      scale_size_continuous(range = c(2,15)) +
      scale_fill_manual(values = region_color) +
      theme_graph() +
      theme(legend.position = "none") +
      ggtitle("Interconectividad Comercial Sistémica",subtitle = year) +
      labs(caption = "Fuente: Cunto, G. & Santana, D. (2021). IMF DOTS (2021), IMF WEO (2021)")
  }
}

#Me gustan drl, dh, mds, centrality


graficador(2020, 0.9, "centrality", 90)

graficador(2020, 0.9, "dh", 90)

#Graficador 2###########


graficador_2 <- function(year, filtro = 0.9, choose_layout = "lgl", label_filter = 80, seed.i =1) {
  imf_graph <- graph_list[[as.character(year)]]
  #Añadir atributos adicionales
  V(imf_graph)$rank <- imf_jurisdiction_scores_percentil$index_rank_per[imf_jurisdiction_scores_percentil$year == year]
  V(imf_graph)$index <- imf_jurisdiction_scores_percentil$index_final_per[imf_jurisdiction_scores_percentil$year == year]
  #Filtrar conexiones
  graph_data_frame <- as_long_data_frame(imf_graph)
  
  graph_data_frame <- graph_data_frame %>% group_by(from_name) %>% filter(percent_rank(weight) >= filtro) 
  
  vertex_dataframe <- graph_data_frame %>% select(from_name, from_region, from_weo_group_region, from_n_region,
                                                  from_strength_out, from_strength_in, from_strength_total, from_degree_out,
                                                  from_degree_in, from_closeness_uw, from_closeness_w, from_betweenness_uw,
                                                  from_betweenness_w, from_eigen_vector_uw, from_eigen_vector_w,
                                                  from_alpha_uw, from_alpha_w, from_page_rank, from_rank,
                                                  from_index) %>% unique()
  
  colnames(vertex_dataframe) <-  c("name", "region", "weo_group_region", "n_region", "strength_out", "strength_in",
                                   "strength_total", "degree_out", "degree_in", "closeness_uw", "closeness_w", 
                                   "betweenness_uw", "betweenness_w", "eigen_vector_uw", "eigen_vector_w",
                                   "alpha_uw", "alpha_w", "page_rank", "rank", "index")
  
  edge_dataframe <- graph_data_frame %>% select(from_name, to_name, weight, n_weight,  dist_weight)
  
  imf_graph_2 <- graph_from_data_frame(d = edge_dataframe, vertices = vertex_dataframe , directed = TRUE)
  #Colores
  imf_nodes <- country_set %>%
    rename(vertices = iso3c) %>%
    select(vertices, country.name.en , region, weo_group_region)
  V(imf_graph_2)$group <- as.character(imf_nodes$region[match(V(imf_graph_2)$name,imf_nodes$vertices)])
  V(imf_graph_2)$color <- V(imf_graph_2)$group
  V(imf_graph_2)$color <- gsub("East Asia & Pacific","#D70036",V(imf_graph_2)$color)
  V(imf_graph_2)$color <- gsub("Europe & Central Asia","#003A5D",V(imf_graph_2)$color)
  V(imf_graph_2)$color <- gsub("Latin America & Caribbean" ,"#157549",V(imf_graph_2)$color)
  V(imf_graph_2)$color <- gsub("Middle East & North Africa","#F1BA0D",V(imf_graph_2)$color)
  V(imf_graph_2)$color <- gsub("North America","#00ACC8",V(imf_graph_2)$color)
  V(imf_graph_2)$color <- gsub("South Asia","#291E58",V(imf_graph_2)$color)
  V(imf_graph_2)$color <- gsub("Sub-Saharan Africa","#F26122",V(imf_graph_2)$color)
  
  region_color_v <- c("South Asia" = "#291E58",
                      "Europe & Central Asia" = "#003A5D",
                      "Middle East & North Africa" = "#F1BA0D",
                      "East Asia & Pacific" = "#D70036",
                      "Sub-Saharan Africa" = "#F26122",
                      "Latin America & Caribbean" = "#157549",
                      "North America" = "#00ACC8")
  
  V(imf_graph_2)$region_esp = V(imf_graph_2)$group
  V(imf_graph_2)$region_esp = gsub("East Asia & Pacific","Asia del Este y Pacífico" ,V(imf_graph_2)$region_esp)
  V(imf_graph_2)$region_esp = gsub("Europe & Central Asia", "Europa y Asia Central",V(imf_graph_2)$region_esp)
  V(imf_graph_2)$region_esp = gsub("Latin America & Caribbean" ,"América Latina y el Caribe",V(imf_graph_2)$region_esp)
  V(imf_graph_2)$region_esp = gsub("Middle East & North Africa","Medio Oriente y África del Norte",V(imf_graph_2)$region_esp)
  V(imf_graph_2)$region_esp = gsub("North America","América del Norte",V(imf_graph_2)$region_esp)
  V(imf_graph_2)$region_esp = gsub("South Asia","Asia del Sur",V(imf_graph_2)$region_esp)
  V(imf_graph_2)$region_esp = gsub("Sub-Saharan Africa","África Subsahariana",V(imf_graph_2)$region_esp)
  
  region_color <- c("Asia del Sur" = "#291E58",
                    "Europa y Asia Central" = "#003A5D",
                    "Medio Oriente y África del Norte" = "#F1BA0D",
                    "Asia del Este y Pacífico" = "#D70036",
                    "África Subsahariana" = "#F26122",
                    "América Latina y el Caribe" = "#157549",
                    "América del Norte" = "#00ACC8")
  
  #Eliminar Vertices
  isolated <- which(igraph::degree(imf_graph_2) == 0)
  imf_graph_3 <- delete.vertices(imf_graph_2, isolated)
  #Tamano nodos
  node_size_1 <- BBmisc::normalize(V(imf_graph_3)$index, method = "range",  range = c(0 , 0.5))
  node_size_2 <- (node_size_1)^2
  
  if(choose_layout != "centrality") {  #Graficar
    set.seed(seed.i)
    ggraph(imf_graph_3, layout = choose_layout) +
      geom_edge_link0(aes(edge_width = weight), edge_colour = "grey") +
      geom_node_point(aes(fill = region_esp, size = node_size_2), shape = 21) +
      geom_node_text(aes(filter= index > label_filter, label = name, size = node_size_2/25), family = "serif", 
                     color = "white") + 
      scale_edge_width_continuous(range = c(0.15, 1), guide = "none") + 
      scale_size_continuous(range = c(0.01,11), guide = "none") +
      scale_fill_manual(values = region_color, name = NULL) +
      theme_graph() +
      theme(legend.position = "bottom") 
  }  
  else {
    ggraph(imf_graph_3, layout = choose_layout, cent = eigen_vector_w) +
      geom_edge_link0(aes(edge_width = weight), edge_colour = "grey") +
      geom_node_point(aes(fill = region, size = node_size_2), shape = 21) +
      geom_node_text(aes(filter= index > label_filter, label = name, size = 0.01), family = "serif", color = "white") + 
      scale_edge_width_continuous(range = c(0.05, 0.9), guide = "none") + 
      scale_size_continuous(range = c(2,15), guide = "none") +
      scale_fill_manual(values = region_color, name = NULL) +
      theme_graph() +
      theme(legend.position = "bottom") +
      ggtitle("Interconectividad Comercial Sistémica",subtitle = year) +
      labs(caption = "Fuente: Cunto, G. & Santana, D. (2021). IMF DOTS (2021), IMF WEO (2021)")
  }
}

?scale_color_discrete

?scale_fill_manual

graficador_2(2000, 0.9, "lgl", 0, 42)

#Chord###########

#Region group legend
lgd_region <- Legend(
  labels =  c(sort(unique(country_set$region))),
  legend_gp =  gpar(fill = c("#003A5D", "#D70036", "#F26122", "#F1BA0D", "#157549", "#00ACC8", "grey")),
  nrow = 2
)

lgd_list_horizontal <- packLegend(lgd_region, direction = "horizontal")


imf_chord <- function(x = imf_graph, ISO = NULL){
  
  #Set seed
  set.seed(101)
  
  #Anadir ranking al grafo
  V(x)$rank <- imf_jurisdiction_scores_percentil$index_rank_per[imf_jurisdiction_scores_percentil$year == (x)$year]
  
  #Creaci?n de data.frames
  imf_top_50_nodes <- as_data_frame(x, what = "vertices") %>% filter(rank <= 50)
  
  imf_top_50_edges <- as_data_frame(x, what = "edges") %>% 
    filter(from %in% imf_top_50_nodes$name, to %in% imf_top_50_nodes$name) %>% 
    select(from, to, weight) %>%
    mutate(weight = weight/1000000000)
  
  #Groups
  imf_group <- structure(imf_top_50_nodes$region, names = imf_top_50_nodes$name)
  
  #Group color
  group_color <- structure(c("#D70036", "#003A5D", "#157549", "#F1BA0D","#00ACC8", "#291E58", "#F26122"),
                           names = sort(unique(imf_group)))
  #Colores para todos
  grid_color_original <- imf_group %>%
    replace(imf_group == sort(unique(imf_group))[1], "#D70036") %>%
    replace(imf_group == sort(unique(imf_group))[2], "#003A5D") %>%
    replace(imf_group == sort(unique(imf_group))[3], "#157549") %>%
    replace(imf_group == sort(unique(imf_group))[4], "#F1BA0D") %>%
    replace(imf_group == sort(unique(imf_group))[5], "#00ACC8") %>%
    replace(imf_group == sort(unique(imf_group))[6], "#291E58") %>%
    replace(imf_group == sort(unique(imf_group))[7], "#F26122")
  
  if(is.null(ISO) == TRUE) {
    grid_color <- grid_color_original  
  } else if (ISO == "no_color") {
    #Colores para ninguno
    grid_color[] <- "#00000000"  
  } else {
    #Colores para highlight
    grid_color_original[imf_top_50_nodes$name != ISO] <- "#00000000"
    grid_color <- grid_color_original 
  }
  
  
  
  #Chord Diagram Parameters
  circos.par(start.degree = 90)
  
  #Diagrama
  chordDiagram(imf_top_50_edges,
               preAllocateTracks = list(track.height = 0.025, track.margin = c(0.05,0)),
               group = imf_group,
               order = names(imf_group),
               grid.col = grid_color,
               link.sort = TRUE,
               link.zindex = rank(imf_top_50_edges$weight),
               directional = 1,
               direction.type = c("diffHeight","arrows"),
               link.arr.type = "big.arrow",
               diffHeight = -uh(0.5,"mm"),
               annotationTrack = "grid")
  
  #Nombres en sectores
  circos.trackPlotRegion(track.index = 2, panel.fun = function(x,y){
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.index = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), 
                mean(ylim), 
                sector.index, 
                col = "white", 
                cex = 0.4, 
                facing = "clockwise", 
                niceFacing = TRUE)
  }, bg.border = NA)
  
  #Axes
  for(i in get.all.sector.index()){
    breaks <- seq(0, 5000, by = 500)
    circos.axis(major.at = breaks,
                labels.cex = 0.4, 
                sector.index = i,
                track.index = 2)
  }
  
  #Track de grupos
  for(i in unique(imf_group)) {
    jurisdiction = names(imf_group[imf_group == i])
    highlight.sector(sector.index = jurisdiction, 
                     track.index = 1, 
                     col = group_color[i])
  }
  
  
  circos.clear()
  
}

###Gráfico de Chord con legenda-----
plot.new()
circle_size = unit(1, "snpc")
pushViewport(viewport(x = 0.5, y = 1, width = circle_size, height = circle_size, just = c("center", "top")))
par(omi = gridOMI(), new = TRUE)
imf_chord(imf_graph)
upViewport()
draw(lgd_region , y = unit(1, "npc") - circle_size, just = "top")

imf_graph <- graph_list[[as.character(2000)]]

imf_chord(imf_graph)
imf_chord(imf_graph, "no_color")
vector_countries <- c("USA", "CHN")
imf_chord(imf_graph, vector_countries)


vertex_attr(imf_graph)

#CHORD DIAGRAM TOP 50-----
imf_chord <- function(x = imf_graph, ISO = NULL){
  
  #Set seed
  set.seed(101)
  
  #Anadir ranking al grafo
  V(x)$rank <- imf_jurisdiction_scores_percentil$index_rank_per[imf_jurisdiction_scores_percentil$year == (x)$year]
  
  #Creaci?n de data.frames
  imf_top_50_nodes <- as_data_frame(x, what = "vertices") %>% filter(rank <= 50)
  
  imf_top_50_edges <- as_data_frame(x, what = "edges") %>% 
    filter(from %in% imf_top_50_nodes$name, to %in% imf_top_50_nodes$name) %>% 
    select(from, to, weight) %>%
    mutate(weight = weight/1000000000)
  
  #Groups
  imf_group <- structure(imf_top_50_nodes$region, names = imf_top_50_nodes$name)
  
  #Group color
  group_color <- structure(c("#003A5D", "#D70036", "#F26122", "#F1BA0D", "#157549", "#00ACC8", "grey"),
                           names = sort(unique(imf_group)))
  #Colores para todos
  grid_color_original <- imf_group %>%
    replace(imf_group == sort(unique(imf_group))[1], "#003A5D") %>%
    replace(imf_group == sort(unique(imf_group))[2], "#D70036") %>%
    replace(imf_group == sort(unique(imf_group))[3], "#F26122") %>%
    replace(imf_group == sort(unique(imf_group))[4], "#F1BA0D") %>%
    replace(imf_group == sort(unique(imf_group))[5], "#157549") %>%
    replace(imf_group == sort(unique(imf_group))[6], "#00ACC8") %>%
    replace(imf_group == sort(unique(imf_group))[7], "grey")
  
  if(is.null(ISO) == TRUE) {
    grid_color <- grid_color_original  
  } else if (ISO == "no_color") {
    #Colores para ninguno
    grid_color[] <- "#00000000"  
  } else {
    #Colores para highlight
    grid_color[imf_top_50_nodes$name != ISO] <- "#00000000"
  }
  
  
  
  #Chord Diagram Parameters
  circos.par(start.degree = 90)
  
  #Diagrama
  chordDiagram(imf_top_50_edges,
               preAllocateTracks = list(track.height = 0.025, track.margin = c(0.05,0)),
               group = imf_group,
               order = names(imf_group),
               grid.col = grid_color,
               link.sort = TRUE,
               link.zindex = rank(imf_top_50_edges$weight),
               directional = 1,
               direction.type = c("diffHeight","arrows"),
               link.arr.type = "big.arrow",
               diffHeight = -uh(0.5,"mm"),
               annotationTrack = "grid")
  
  #Nombres en sectores
  circos.trackPlotRegion(track.index = 2, panel.fun = function(x,y){
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.index = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), 
                mean(ylim), 
                sector.index, 
                col = "white", 
                cex = 0.4, 
                facing = "clockwise", 
                niceFacing = TRUE)
  }, bg.border = NA)
  
  #Axes
  for(i in get.all.sector.index()){
    breaks <- seq(0, 5000, by = 500)
    circos.axis(major.at = breaks,
                labels.cex = 0.4, 
                sector.index = i,
                track.index = 2)
  }
  
  #Track de grupos
  for(i in unique(imf_group)) {
    jurisdiction = names(imf_group[imf_group == i])
    highlight.sector(sector.index = jurisdiction, 
                     track.index = 1, 
                     col = group_color[i])
  }
  
  
  circos.clear()
  
  
}

imf_chord(imf_graph)

##Chord Diagram Top 50 (2020)-----

###Legend-----
lgd_region <- Legend(
  labels =  c(sort(unique(country_set$region))),
  legend_gp =  gpar(fill = c("#003A5D", "#D70036", "#F26122", "#F1BA0D", "#157549", "#00ACC8", "grey")),
  nrow = 2
)

lgd_list_horizontal <- packLegend(lgd_region, direction = "horizontal")

###Chord with legend-----
plot.new()
circle_size = unit(1, "snpc")
pushViewport(viewport(x = 0.5, y = 1, width = circle_size, height = circle_size, just = c("center", "top")))
par(omi = gridOMI(), new = TRUE)
imf_chord(imf_graph)
upViewport()
draw(lgd_region , y = unit(1, "npc") - circle_size, just = "top")

#INDEX----------

#RANKING--------