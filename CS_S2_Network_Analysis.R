#SYSTEMIC TRADE INTERCONNECTEDNESS
  #Cunto, G. & Santana, D. (202X)

#SCRIPT 2: NETWORK ANALYSIS-----

install.packages("writexl")

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
library(writexl)

#NOMINAL NETWORK ANALYSIS----

##Tibbles to save results-----

#Netowrk atributes (Network as a whole)
imf_network_scores <- tibble()

#Jusrisdiction attributes (Measures for individual countries)
imf_jurisdiction_scores <- tibble()

#Graph list
graph_list <- list()

#Graph year identifier
graph_year <- 0

##Loop to analyse graphs----
for(i in unique(imf_dots$year)){
  
  #Graph construction
  
  #Edges
  imf_edges <- imf_dots %>%
    filter(year == i) %>% filter(jurisdiction_iso3 != former_countries$iso3c) %>%
    rename(from = jurisdiction_iso3, to = counterpart_iso3, weight = exports) %>%
    select(from, to, weight) 
  
  #Nodes
  imf_nodes <- country_set %>%
    rename(vertices = iso3c) %>%
    select(vertices, country.name.en , region, weo_group_region)
  
  #Network
  imf_graph <- graph_from_data_frame(d = imf_edges, vertices = imf_nodes, directed = TRUE)
  
  #Network Attributes
  imf_graph <- imf_graph %>%
    set.graph.attribute("year", value = i) %>%
    set.edge.attribute("n_weight", value = E(imf_graph)$weight/max(E(imf_graph)$weight)) %>%
    set.edge.attribute("dist_weight", value = 1/E(imf_graph)$weight) %>%
    set.vertex.attribute("n_region", value = as.integer(factor(imf_nodes$region, levels = unique(imf_nodes$region)))) %>%
    set.vertex.attribute("n_weo_group_region", value = as.integer(factor(imf_nodes$region, levels = unique(imf_nodes$region)))) %>%
    delete.vertices(c(names(which(igraph::degree(imf_graph)==0))))
  
  ##Network Measures----
  
  #Save network features
  imf_features <- tibble(Year = i, #Year
                         Nodes = gorder(imf_graph), #Number of nodes
                         Edges = gsize(imf_graph), #Number of links
                         Density = edge_density(imf_graph), #Density
                         Mean_distance = mean_distance(imf_graph, directed = TRUE), #Mean Distance
                         Transitivity_uw = transitivity(imf_graph, weights = NA), #Transitivity (Unweighted)
                         Transitivity_w = transitivity(imf_graph, weights = E(imf_graph)$dist_weight), #Transitivity (Weighted)
                         Reciprocity = reciprocity(imf_graph), #Reciprocity
                         Diameter_uw = diameter(imf_graph, directed = TRUE, weights = NA), #Diameter (Unweighted)
                         Diameter_w = diameter(imf_graph, directed = TRUE, weights = E(imf_graph)$dist_weight), #Diameter (Weighted)
                         Assortativity = assortativity_nominal(imf_graph, V(imf_graph)$n_region)) #Assortativity
  
  #Create network results
  imf_network_scores <- rbind(imf_network_scores, imf_features)
  
  #Erase features
  rm(imf_features)
  
  ##Jurisdiction measures -----
  
  #Intermediate tibble to save results
  imf_results <- tibble()
  
  ###Strength-----
  
  #Out
  V(imf_graph)$strength_out <- strength(imf_graph, mode = "out")
  
  #In
  V(imf_graph)$strength_in <- strength(imf_graph, mode = "in")
  
  #Total
  V(imf_graph)$strength_total <- strength(imf_graph, mode = "total")
  
  ###Centrality-----
  
  ####Degree----
  
  #Out-degree
  V(imf_graph)$degree_out <- igraph::degree(imf_graph, mode = "out")
  
  #In-degree
  V(imf_graph)$degree_in <- igraph::degree(imf_graph, mode = "in")
  
  ####Closeness----
  
  #Un-Weighted
  V(imf_graph)$closeness_uw <- closeness(imf_graph, mode = "all", weights = NA)
  
  #Weighted
  V(imf_graph)$closeness_w <- closeness(imf_graph, mode = "all", weights = E(imf_graph)$dist_weight)
  
  ####Betwenness----
  
  #Un-Weighted
  V(imf_graph)$betweenness_uw <- betweenness(imf_graph, weights = NA)
  
  #Weighted
  V(imf_graph)$betweenness_w <- betweenness(imf_graph, weights = E(imf_graph)$dist_weight)
  
  ####Eigenvector----
  
  #Un-Weighted
  V(imf_graph)$eigen_vector_uw <- eigen_centrality(imf_graph, directed = TRUE, weights = NA)$vector
  
  #Weighted
  V(imf_graph)$eigen_vector_w <- eigen_centrality(imf_graph, directed = TRUE, weights = E(imf_graph)$weight)$vector
  
  ####Alpha----
  
  #Un-Weighted
  V(imf_graph)$alpha_uw <- alpha_centrality(imf_graph, alpha = (1/eigen_centrality(imf_graph, directed = TRUE, weights = NA)$value)*0.5, weights = NA)
  
  #Weighted
  V(imf_graph)$alpha_w <- alpha_centrality(imf_graph, alpha = (1/eigen_centrality(imf_graph, directed = TRUE, weights = E(imf_graph)$weight)$value)*0.5)
  
  ####Page Rank----
  V(imf_graph)$page_rank <- page_rank(imf_graph, directed = TRUE)$vector
  
  
  #Add results
  imf_results <- igraph::as_data_frame(imf_graph, what = "vertices") %>% 
    mutate(year = i) %>%
    select(year, everything())
  
  imf_jurisdiction_scores <- rbind(imf_jurisdiction_scores, imf_results)
  
  #Erase intermediate tibble
  rm(imf_results)
  
  #Delete nodes and edges
  rm(imf_edges)
  rm(imf_nodes)
  
  ##Graph list----
  graph_year <- graph_year + 1
  graph_list[[graph_year]] <- imf_graph 
  names(graph_list)[graph_year] <- i
  
}

#Order imf network scores 
imf_network_scores <- imf_network_scores[order(imf_network_scores$Year),]

#INDICES----

##Annual normalization -------
imf_jurisdiction_scores_normalized <- tibble()

for(i in unique(imf_jurisdiction_scores$year)){
  
  temporary_dataframe <- imf_jurisdiction_scores %>% filter(year == i) %>%
    mutate(strength_out_norm = BBmisc::normalize(strength_out, method = "range",  range = c(0 , 1)),
           strength_in_norm = BBmisc::normalize(strength_in, method = "range",  range = c(0 , 1)),
           strength_total_norm = BBmisc::normalize(strength_total, method = "range",  range = c(0 , 1)),
           degree_out_norm = BBmisc::normalize(degree_out, method = "range",  range = c(0 , 1)), 
           degree_in_norm = BBmisc::normalize(degree_in, method = "range",  range = c(0 , 1)), 
           closeness_uw_norm = BBmisc::normalize(closeness_uw, method = "range",  range = c(0 , 1)),
           closeness_w_norm = BBmisc::normalize(closeness_w, method = "range",  range = c(0 , 1)),
           betweenness_uw_norm = BBmisc::normalize(betweenness_uw, method = "range",  range = c(0 , 1)),
           betweenness_w_norm = BBmisc::normalize(betweenness_w, method = "range",  range = c(0 , 1)),
           alpha_uw_norm = BBmisc::normalize(alpha_uw, method = "range",  range = c(0 , 1)),
           alpha_w_norm = BBmisc::normalize(alpha_w, method = "range",  range = c(0 , 1))) %>%
    mutate(index_str = rowMeans(select(., c(strength_out_norm, strength_in_norm, strength_total_norm))),
           index_uw = rowMeans(select(., c(degree_out_norm, degree_in_norm, closeness_uw_norm,  betweenness_uw_norm, eigen_vector_uw, alpha_uw_norm))),
           index_w = rowMeans(select(., c(closeness_w_norm,  betweenness_w_norm, eigen_vector_w, alpha_w_norm)))) %>%
    mutate(index_final_norm = rowMeans(select(., c(index_str, index_uw, index_w)))*100,
           index_rank_norm = rank(desc(index_final_norm))) %>%
    select(year, name, country.name.en, region, weo_group_region, 
           strength_out_norm, strength_in_norm, strength_total_norm, 
           degree_out_norm, degree_in_norm, closeness_uw_norm, closeness_w_norm,
           betweenness_uw_norm, betweenness_w_norm, eigen_vector_uw, eigen_vector_w, 
           alpha_uw_norm, alpha_w_norm, 
           index_str, index_uw, index_w, index_final_norm, index_rank_norm)
  
  imf_jurisdiction_scores_normalized <- rbind(imf_jurisdiction_scores_normalized, temporary_dataframe)
  
}

##Panel Normalization (Percentile)------

imf_results <- imf_jurisdiction_scores %>% 
  mutate(strength_out_per = percent_rank(strength_out),
         strength_in_per = percent_rank(strength_in),
         strength_total_per = percent_rank(strength_total),
         degree_out_per = percent_rank(degree_out), 
         degree_in_per = percent_rank(degree_in), 
         closeness_uw_per = percent_rank(closeness_uw),
         closeness_w_per = percent_rank(closeness_w),
         betweenness_uw_per = percent_rank(betweenness_uw),
         betweenness_w_per = percent_rank(betweenness_w),
         alpha_uw_per = percent_rank(alpha_uw),
         alpha_w_per = percent_rank(alpha_w),
         eigen_vector_uw_per = percent_rank(eigen_vector_uw),
         eigen_vector_w_per = percent_rank(eigen_vector_w)) %>%
  mutate(index_str_per = rowMeans(select(., c(strength_out_per, strength_in_per, strength_total_per))),
         index_uw_per = rowMeans(select(., c(degree_out_per, degree_in_per, closeness_uw_per, betweenness_uw_per, eigen_vector_uw_per, alpha_uw_per))),
         index_w_per = rowMeans(select(., c(closeness_w_per,  betweenness_w_per, eigen_vector_w_per, alpha_w_per)))) %>%
  mutate(index_final_per = rowMeans(select(., c(index_str_per, index_uw_per, index_w_per)))*100) %>%
  select(year, name, country.name.en, region, weo_group_region, 
         strength_out_per, strength_in_per, strength_total_per, 
         degree_out_per, degree_in_per, 
         closeness_uw_per, closeness_w_per,
         betweenness_uw_per, betweenness_w_per,
         eigen_vector_uw_per, eigen_vector_w_per, 
         alpha_uw_per, alpha_w_per, 
         index_str_per, index_uw_per, index_w_per, index_final_per)

imf_jurisdiction_scores_percentil <- tibble()

for(i in unique(imf_dots$year)){
  
  temporary_dataframe <- imf_results %>% filter(year == i) %>%
    mutate(index_rank_per = rank(desc(index_final_per)))
  
  imf_jurisdiction_scores_percentil <- rbind(imf_jurisdiction_scores_percentil, temporary_dataframe)
  
  rm(temporary_dataframe)
  
}

rm(imf_results)

#INFLATION ADJUSTED NETWORK ANALYSIS----

##Tibbles to save results-----

#Netowrk atributes (Network as a whole)
imf_network_scores_cons <- tibble()

#Jusrisdiction attributes (Measures for individual countries)
imf_jurisdiction_scores_cons <- tibble()

#Graph list
graph_list <- list()

#Graph year identifier
graph_year <- 0

i <- 2021
##Loop to analyse graphs----
for(i in unique(imf_dots$year)){
  
  #Graph construction
  
  #Edges
  imf_edges <- imf_dots %>%
    filter(year == i)  %>%
    rename(from = jurisdiction_iso3, to = counterpart_iso3, weight = exports_cons) %>%
    select(from, to, weight)
  
  #Nodes
  imf_nodes <- country_set  %>% 
    rename(vertices = iso3c) %>%
    select(vertices, country.name.en , region, weo_group_region)
  
  #Network
  imf_graph <- graph_from_data_frame(d = imf_edges, vertices = imf_nodes, directed = TRUE)
  
  #Network Attributes
  imf_graph <- imf_graph %>%
    set.graph.attribute("year", value = i) %>%
    set.edge.attribute("n_weight", value = E(imf_graph)$weight/max(E(imf_graph)$weight)) %>%
    set.edge.attribute("dist_weight", value = 1/E(imf_graph)$weight) %>%
    set.vertex.attribute("n_region", value = as.integer(factor(imf_nodes$region, levels = unique(imf_nodes$region)))) %>%
    set.vertex.attribute("n_weo_group_region", value = as.integer(factor(imf_nodes$region, levels = unique(imf_nodes$region)))) %>%
    delete.vertices(c(names(which(igraph::degree(imf_graph)==0))))
  
  ##Network Measures----
  
  #Save network features
  imf_features <- tibble(Year = i, #Year
                         Nodes = gorder(imf_graph), #Number of nodes
                         Edges = gsize(imf_graph), #Number of links
                         Density = edge_density(imf_graph), #Density
                         Mean_distance = mean_distance(imf_graph, directed = TRUE), #Mean Distance
                         Transitivity_uw = transitivity(imf_graph, weights = NA), #Transitivity (Unweighted)
                         Transitivity_w = transitivity(imf_graph, weights = E(imf_graph)$dist_weight), #Transitivity (Weighted)
                         Reciprocity = reciprocity(imf_graph), #Reciprocity
                         Diameter_uw = diameter(imf_graph, directed = TRUE, weights = NA), #Diameter (Unweighted)
                         Diameter_w = diameter(imf_graph, directed = TRUE, weights = E(imf_graph)$dist_weight), #Diameter (Weighted)
                         Assortativity = assortativity_nominal(imf_graph, V(imf_graph)$n_region)) #Assortativity
  
  #Create network results
  imf_network_scores_cons <- rbind(imf_network_scores_cons, imf_features)
  
  #Erase features
  rm(imf_features)
  
  ##Jurisdiction measures -----
  
  #Intermediate tibble to save results
  imf_results <- tibble()
  
  ###Strength-----
  
  #Out
  V(imf_graph)$strength_out <- strength(imf_graph, mode = "out")
  
  #In
  V(imf_graph)$strength_in <- strength(imf_graph, mode = "in")
  
  #Total
  V(imf_graph)$strength_total <- strength(imf_graph, mode = "total")
  
  ###Centrality-----
  
  ####Degree----
  
  #Out-degree
  V(imf_graph)$degree_out <- igraph::degree(imf_graph, mode = "out")
  
  #In-degree
  V(imf_graph)$degree_in <- igraph::degree(imf_graph, mode = "in")
  
  ####Closeness----
  
  #Un-Weighted
  V(imf_graph)$closeness_uw <- closeness(imf_graph, mode = "all", weights = NA)
  
  #Weighted
  V(imf_graph)$closeness_w <- closeness(imf_graph, mode = "all", weights = E(imf_graph)$dist_weight)
  
  ####Betwenness----
  
  #Un-Weighted
  V(imf_graph)$betweenness_uw <- betweenness(imf_graph, weights = NA)
  
  #Weighted
  V(imf_graph)$betweenness_w <- betweenness(imf_graph, weights = E(imf_graph)$dist_weight)
  
  ####Eigenvector----
  
  #Un-Weighted
  V(imf_graph)$eigen_vector_uw <- eigen_centrality(imf_graph, directed = TRUE, weights = NA)$vector
  
  #Weighted
  V(imf_graph)$eigen_vector_w <- eigen_centrality(imf_graph, directed = TRUE, weights = E(imf_graph)$weight)$vector
  
  ####Alpha----
  
  #Un-Weighted
  V(imf_graph)$alpha_uw <- alpha_centrality(imf_graph, alpha = (1/eigen_centrality(imf_graph, directed = TRUE, weights = NA)$value)*0.5, weights = NA)
  
  #Weighted
  V(imf_graph)$alpha_w <- alpha_centrality(imf_graph, alpha = (1/eigen_centrality(imf_graph, directed = TRUE, weights = E(imf_graph)$weight)$value)*0.5)
  
  ####Page Rank----
  V(imf_graph)$page_rank <- page_rank(imf_graph, directed = TRUE)$vector
  
  
  #Add results
  imf_results <- igraph::as_data_frame(imf_graph, what = "vertices") %>% 
    mutate(year = i) %>%
    select(year, everything())
  
  imf_jurisdiction_scores_cons <- rbind(imf_jurisdiction_scores_cons, imf_results)
  
  #Erase intermediate tibble
  rm(imf_results)
  
  #Delete nodes and edges
  rm(imf_edges)
  rm(imf_nodes)
  
  ##Graph list----
  graph_year <- graph_year + 1
  graph_list[[graph_year]] <- imf_graph 
  names(graph_list)[graph_year] <- i
  
}

#Order imf network scores 
imf_network_scores_cons <- imf_network_scores_cons[order(imf_network_scores_cons$Year),]

#INDICES----

##Annual normalization -------
imf_jurisdiction_scores_cons_normalized <- tibble()

for(i in unique(imf_jurisdiction_scores_cons$year)){
  
  temporary_dataframe <- imf_jurisdiction_scores_cons %>% filter(year == i) %>%
    mutate(strength_out_norm = BBmisc::normalize(strength_out, method = "range",  range = c(0 , 1)),
           strength_in_norm = BBmisc::normalize(strength_in, method = "range",  range = c(0 , 1)),
           strength_total_norm = BBmisc::normalize(strength_total, method = "range",  range = c(0 , 1)),
           degree_out_norm = BBmisc::normalize(degree_out, method = "range",  range = c(0 , 1)), 
           degree_in_norm = BBmisc::normalize(degree_in, method = "range",  range = c(0 , 1)), 
           closeness_uw_norm = BBmisc::normalize(closeness_uw, method = "range",  range = c(0 , 1)),
           closeness_w_norm = BBmisc::normalize(closeness_w, method = "range",  range = c(0 , 1)),
           betweenness_uw_norm = BBmisc::normalize(betweenness_uw, method = "range",  range = c(0 , 1)),
           betweenness_w_norm = BBmisc::normalize(betweenness_w, method = "range",  range = c(0 , 1)),
           alpha_uw_norm = BBmisc::normalize(alpha_uw, method = "range",  range = c(0 , 1)),
           alpha_w_norm = BBmisc::normalize(alpha_w, method = "range",  range = c(0 , 1))) %>%
    mutate(index_str = rowMeans(select(., c(strength_out_norm, strength_in_norm, strength_total_norm))),
           index_uw = rowMeans(select(., c(degree_out_norm, degree_in_norm, closeness_uw_norm,  betweenness_uw_norm, eigen_vector_uw, alpha_uw_norm))),
           index_w = rowMeans(select(., c(closeness_w_norm,  betweenness_w_norm, eigen_vector_w, alpha_w_norm)))) %>%
    mutate(index_final_norm = rowMeans(select(., c(index_str, index_uw, index_w)))*100,
           index_rank_norm = rank(desc(index_final_norm))) %>%
    select(year, name, country.name.en, region, weo_group_region, 
           strength_out_norm, strength_in_norm, strength_total_norm, 
           degree_out_norm, degree_in_norm, closeness_uw_norm, closeness_w_norm,
           betweenness_uw_norm, betweenness_w_norm, eigen_vector_uw, eigen_vector_w, 
           alpha_uw_norm, alpha_w_norm, 
           index_str, index_uw, index_w, index_final_norm, index_rank_norm)
  
  imf_jurisdiction_scores_cons_normalized <- rbind(imf_jurisdiction_scores_cons_normalized, temporary_dataframe)
  
}

##Panel Normalization (Percentile)------

imf_results <- imf_jurisdiction_scores_cons %>% 
  mutate(strength_out_per = percent_rank(strength_out),
         strength_in_per = percent_rank(strength_in),
         strength_total_per = percent_rank(strength_total),
         degree_out_per = percent_rank(degree_out), 
         degree_in_per = percent_rank(degree_in), 
         closeness_uw_per = percent_rank(closeness_uw),
         closeness_w_per = percent_rank(closeness_w),
         betweenness_uw_per = percent_rank(betweenness_uw),
         betweenness_w_per = percent_rank(betweenness_w),
         alpha_uw_per = percent_rank(alpha_uw),
         alpha_w_per = percent_rank(alpha_w),
         eigen_vector_uw_per = percent_rank(eigen_vector_uw),
         eigen_vector_w_per = percent_rank(eigen_vector_w)) %>%
  mutate(index_str_per = rowMeans(select(., c(strength_out_per, strength_in_per, strength_total_per))),
         index_uw_per = rowMeans(select(., c(degree_out_per, degree_in_per, closeness_uw_per, betweenness_uw_per, eigen_vector_uw_per, alpha_uw_per))),
         index_w_per = rowMeans(select(., c(closeness_w_per,  betweenness_w_per, eigen_vector_w_per, alpha_w_per)))) %>%
  mutate(index_final_per = rowMeans(select(., c(index_str_per, index_uw_per, index_w_per)))*100) %>%
  select(year, name, country.name.en, region, weo_group_region, 
         strength_out_per, strength_in_per, strength_total_per, 
         degree_out_per, degree_in_per, 
         closeness_uw_per, closeness_w_per,
         betweenness_uw_per, betweenness_w_per,
         eigen_vector_uw_per, eigen_vector_w_per, 
         alpha_uw_per, alpha_w_per, 
         index_str_per, index_uw_per, index_w_per, index_final_per)

imf_jurisdiction_scores_cons_percentil <- tibble()

for(i in unique(imf_dots$year)){
  
  temporary_dataframe <- imf_results %>% filter(year == i) %>%
    mutate(index_rank_per = rank(desc(index_final_per)))
  
  imf_jurisdiction_scores_cons_percentil <- rbind(imf_jurisdiction_scores_cons_percentil, temporary_dataframe)
  
  rm(temporary_dataframe)
  
}

rm(imf_results)

View(imf_network_scores_cons) 
 
#To excel 

write_xlsx(imf_jurisdiction_scores_cons_percentil, "jurisdiction_scores.xlsx")

write_xlsx(imf_network_scores_cons, "network.xlsx")

write_xlsx(imf_jurisdiction_scores, "jurisdiction_values.xlsx")

