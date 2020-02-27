##########################################
# Preprint tracker for SARS-Cov-2        #
# Liam Brierley, University of Liverpool #
# 27/2/20                                #
##########################################

rm(list=ls())

# Load required libraries
library(ggplot2)
library(htmlwidgets)
library(jsonlite)
library(plotly)
library(XML)

# Define datasets, functions
arxiv_cats <- data.frame(abb = c("astro-ph","astro-ph.CO","astro-ph.EP","astro-ph.GA","astro-ph.HE","astro-ph.IM","astro-ph.SR","cond-mat.dis-nn","cond-mat.mes-hall","cond-mat.mtrl-sci","cond-mat.other","cond-mat.quant-gas","cond-mat.soft","cond-mat.stat-mech","cond-mat.str-el","cond-mat.supr-con","cs.AI","cs.AR","cs.CC","cs.CE","cs.CG","cs.CL","cs.CR","cs.CV","cs.CY","cs.DB","cs.DC","cs.DL","cs.DM","cs.DS","cs.ET","cs.FL","cs.GL","cs.GR","cs.GT","cs.HC","cs.IR","cs.IT","cs.LG","cs.LO","cs.MA","cs.MM","cs.MS","cs.NA","cs.NE","cs.NI","cs.OH","cs.OS","cs.PF","cs.PL","cs.RO","cs.SC","cs.SD","cs.SE","cs.SI","cs.SY","econ.EM","eess.AS","eess.IV","eess.SP","gr-qc","hep-ex","hep-lat","hep-ph","hep-th","math.AC","math.AG","math.AP","math.AT","math.CA","math.CO","math.CT","math.CV","math.DG","math.DS","math.FA","math.GM","math.GN","math.GR","math.GT","math.HO","math.IT","math.KT","math.LO","math.MG","math.MP","math.NA","math.NT","math.OA","math.OC","math.PR","math.QA","math.RA","math.RT","math.SG","math.SP","math.ST","math-ph","nlin.AO","nlin.CD","nlin.CG","nlin.PS","nlin.SI","nucl-ex","nucl-th","physics.acc-ph","physics.ao-ph","physics.app-ph","physics.atm-clus","physics.atom-ph","physics.bio-ph","physics.chem-ph","physics.class-ph","physics.comp-ph","physics.data-an","physics.ed-ph","physics.flu-dyn","physics.gen-ph","physics.geo-ph","physics.hist-ph","physics.ins-det","physics.med-ph","physics.optics","physics.plasm-ph","physics.pop-ph","physics.soc-ph","physics.space-ph","q-bio.BM","q-bio.CB","q-bio.GN","q-bio.MN","q-bio.NC","q-bio.OT","q-bio.PE","q-bio.QM","q-bio.SC","q-bio.TO","q-fin.CP","q-fin.EC","q-fin.GN","q-fin.MF","q-fin.PM","q-fin.PR","q-fin.RM","q-fin.ST","q-fin.TR","quant-ph","stat.AP","stat.CO","stat.ME","stat.ML","stat.OT","stat.TH"),
                         lab = c("Astrophysics","Cosmology and Nongalactic Astrophysics","Earth and Planetary Astrophysics","Astrophysics of Galaxies","High Energy Astrophysical Phenomena","Instrumentation and Methods for Astrophysics","Solar and Stellar Astrophysics","Disordered Systems and Neural Networks","Mesoscale and Nanoscale Physics","Materials Science","Other Condensed Matter","Quantum Gases","Soft Condensed Matter","Statistical Mechanics","Strongly Correlated Electrons","Superconductivity","Artificial Intelligence","Hardware Architecture","Computational Complexity","Computational Engineering, Finance, and Science","Computational Geometry","Computation and Language","Cryptography and Security","Computer Vision and Pattern Recognition","Computers and Society","Databases","Distributed, Parallel, and Cluster Computing","Digital Libraries","Discrete Mathematics","Data Structures and Algorithms","Emerging Technologies","Formal Languages and Automata Theory","General Literature","Graphics","Computer Science and Game Theory","Human-Computer Interaction","Information Retrieval","Information Theory","Learning","Logic in Computer Science","Multiagent Systems","Multimedia","Mathematical Software","Numerical Analysis","Neural and Evolutionary Computing","Networking and Internet Architecture","Other Computer Science","Operating Systems","Performance","Programming Languages","Robotics","Symbolic Computation","Sound","Software Engineering","Social and Information Networks","Systems and Control","Econometrics","Audio and Speech Processing","Image and Video Processing","Signal Processing","General Relativity and Quantum Cosmology","High Energy Physics - Experiment","High Energy Physics - Lattice","High Energy Physics - Phenomenology","High Energy Physics - Theory","Commutative Algebra","Algebraic Geometry","Analysis of PDEs","Algebraic Topology","Classical Analysis and ODEs","Combinatorics","Category Theory","Complex Variables","Differential Geometry","Dynamical Systems","Functional Analysis","General Mathematics","General Topology","Group Theory","Geometric Topology","History and Overview","Information Theory","K-Theory and Homology","Logic","Metric Geometry","Mathematical Physics","Numerical Analysis","Number Theory","Operator Algebras","Optimization and Control","Probability","Quantum Algebra","Rings and Algebras","Representation Theory","Symplectic Geometry","Spectral Theory","Statistics Theory","Mathematical Physics","Adaptation and Self-Organizing Systems","Chaotic Dynamics","Cellular Automata and Lattice Gases","Pattern Formation and Solitons","Exactly Solvable and Integrable Systems","Nuclear Experiment","Nuclear Theory","Accelerator Physics","Atmospheric and Oceanic Physics","Applied Physics","Atomic and Molecular Clusters","Atomic Physics","Biological Physics","Chemical Physics","Classical Physics","Computational Physics","Data Analysis, Statistics and Probability","Physics Education","Fluid Dynamics","General Physics","Geophysics","History and Philosophy of Physics","Instrumentation and Detectors","Medical Physics","Optics","Plasma Physics","Popular Physics","Physics and Society","Space Physics","Biomolecules","Cell Behavior","Genomics","Molecular Networks","Neurons and Cognition","Other Quantitative Biology","Populations and Evolution","Quantitative Methods","Subcellular Processes","Tissues and Organs","Computational Finance","Economics","General Finance","Mathematical Finance","Portfolio Management","Pricing of Securities","Risk Management","Statistical Finance","Trading and Market Microstructure","Quantum Physics","Applications","Computation","Methodology","Machine Learning","Other Statistics","Statistics Theory"))

fetch_preprints <- function(query, server, page_size = 250){
  
  if (server=="biorxiv"){
    
    result_list <- rep(list(list()),length(query)) # Initalise empty list
    
    for(i in 1:length(query)){
      
      # Store number of pages of results
      n_pages <- fromJSON(txt=paste0("https://api.rxivist.org/v1/papers?q=",query[i],"&metric=downloads&page_size=",page_size))$query$final_page
      
      for(j in 0:n_pages){
        # Search using Rxivist API
        result_list[[i]][[j+1]] <- fromJSON(txt=paste0("https://api.rxivist.org/v1/papers?q=",query[i],"&metric=downloads&page_size=",page_size,"&page=",j))$results
      }
      
    }
    
    df <- data.table::rbindlist(lapply(result_list, function(x) rbind_pages(x)))
    return(df[!duplicated(df$id),]) # Remove pre-prints duplicated between search queries
    
  } else {
    
    
    # Search using arXiv API  
    result_list <- xmlToList(xmlParse(
      paste0("http://export.arxiv.org/api/query?search_query=all:",paste(query, collapse="+OR+"),"&start=0&max_results=", page_size)))
    
    # Create dataframe for arXiv results using same structure as to bioRxiv results
    df <- data.frame(title = unlist(lapply(result_list[8:length(result_list)], function(x) x$title)),
                     url = unlist(lapply(result_list[8:length(result_list)], function(x) x$link[1])),
                     doi = unlist(lapply(result_list[8:length(result_list)], function(x) x$id)),
                     category = unlist(lapply(result_list[8:length(result_list)], function(x) x$primary_category[1])),
                     first_posted = as.Date(unlist(lapply(result_list[8:length(result_list)], function(x) x$published))))
    return(df)
    
  }
  
}

process_preprints <- function(df, startdate, enddate = as.Date(Sys.time(), format="%Y-%m-%d")){
  df <- subset(df, first_posted > startdate & first_posted <= enddate)
  df$category <- as.factor(df$category)
  df$first_posted <- as.Date(df$first_posted, format="%Y-%m-%d")
  df <- df[with(df, order(first_posted)), ]
  df$total_papers <- 1:nrow(df)
  return(df)
}

js <- "function(el, x) {
                el.on('plotly_click', function(d) {
                var url = d.points[0].customdata;
                //url
                window.open(url);
                });
                }
"

# Extract preprint archive information (currently bioRxiv and arXiv)

biorxiv_cov <- fetch_preprints(query = c("coronavirus","coronaviruses","ncov","SARS-CoV-2","COVID-19"), server = "biorxiv")
arxiv_cov <- fetch_preprints(query = c("coronavirus","coronaviruses","ncov","SARS-CoV-2","COVID-19"), server = "arxiv")

biorxiv_cov <- process_preprints(biorxiv_cov, startdate="2019-12-29") # Start date defined as notification of first cluster
arxiv_cov <- process_preprints(arxiv_cov, startdate="2019-12-29")

g2 <- ggplot(biorxiv_cov, aes(x = first_posted, y = total_papers, label = title, label2 = doi)) +  
  geom_step(data = biorxiv_cov, mapping=aes(x = first_posted, y = total_papers, group=1), alpha=0.5) +
  geom_point(aes(color = category), size=3) +
  xlab('Date') +  
  ylab("bioRxiv pre-prints posted") +
  scale_x_date(limits = c(as.Date('2020-01-19'),as.Date(Sys.time()))) +
  annotate("text", x = as.Date('2020-01-24'), y = max(biorxiv_cov$total_papers)-1, 
           label = paste0("Data up to: ", max(biorxiv_cov$first_posted),"\nCurve produced: ",as.Date(Sys.time())), size = 6) +
  theme_bw(base_size = 17)
p2 <- ggplotly(g2)

# Add in hyperlink data to plot
p2$x$data[[1]]$customdata <- biorxiv_cov$biorxiv_url
for (i in 1:nlevels(biorxiv_cov$category)){
  p2$x$data[[1+i]]$customdata <- subset(biorxiv_cov, category == levels(biorxiv_cov$category)[i])$biorxiv_url
}

# Plot biorxiv curve
saveWidget(as_widget(onRender(p2, js)), "M:\\Git\\lbrierley.github.io\\cov_preprints\\biorxiv_cov_preprints.html", title = "bioRxiv COVID-19 preprint curve")

# Replace abbreviation with full category label before plotting arxiv
arxiv_cov$category <- factor(arxiv_cats$lab[match(arxiv_cov$category, arxiv_cats$abb)])

g3 <- ggplot(arxiv_cov, aes(x = first_posted, y = total_papers, label = title, label2 = doi)) +  
  geom_step(data = arxiv_cov, mapping=aes(x = first_posted, y = total_papers, group=1), alpha=0.5) +
  geom_point(aes(color = category), size=3) +
  xlab('Date') +  
  ylab("arXiv pre-prints posted") +
  scale_x_date(limits = c(as.Date('2020-01-19'),as.Date(Sys.time()))) +
  annotate("text", x = as.Date('2020-01-24'), y = max(arxiv_cov$total_papers)-1, 
           label = paste0("Data up to: ", max(arxiv_cov$first_posted),"\nCurve produced: ",as.Date(Sys.time())), size = 6) +
  theme_bw(base_size = 17)
p3 <- ggplotly(g3)

# Add in hyperlink data to plot
p3$x$data[[1]]$customdata <- arxiv_cov$url
for (i in 1:nlevels(arxiv_cov$category)){
  p3$x$data[[1+i]]$customdata <- subset(arxiv_cov, category == levels(arxiv_cov$category)[i])$url
}

# Plot arxiv curve
saveWidget(as_widget(onRender(p3, js)), "M:\\Git\\lbrierley.github.io\\cov_preprints\\arxiv_cov_preprints.html", title = "arXiv COVID-19 preprint curve")