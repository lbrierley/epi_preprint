#################################################################
#          Preprint tracker for SARS-Cov-2 / COVID-19           #
# Liam Brierley, Dept of Biostatistics, University of Liverpool #
#                            2/3/20                             #
#################################################################

rm(list=ls())

# Load required libraries
library(ggplot2)
library(htmlwidgets)
library(jsonlite)
library(medrxivr)
library(plotly)
library(XML)

# Define category translation for arXiv
arxiv_cats <- data.frame(abb = c("astro-ph","astro-ph.CO","astro-ph.EP","astro-ph.GA","astro-ph.HE","astro-ph.IM","astro-ph.SR","cond-mat.dis-nn","cond-mat.mes-hall","cond-mat.mtrl-sci","cond-mat.other","cond-mat.quant-gas","cond-mat.soft","cond-mat.stat-mech","cond-mat.str-el","cond-mat.supr-con","cs.AI","cs.AR","cs.CC","cs.CE","cs.CG","cs.CL","cs.CR","cs.CV","cs.CY","cs.DB","cs.DC","cs.DL","cs.DM","cs.DS","cs.ET","cs.FL","cs.GL","cs.GR","cs.GT","cs.HC","cs.IR","cs.IT","cs.LG","cs.LO","cs.MA","cs.MM","cs.MS","cs.NA","cs.NE","cs.NI","cs.OH","cs.OS","cs.PF","cs.PL","cs.RO","cs.SC","cs.SD","cs.SE","cs.SI","cs.SY","econ.EM","eess.AS","eess.IV","eess.SP","gr-qc","hep-ex","hep-lat","hep-ph","hep-th","math.AC","math.AG","math.AP","math.AT","math.CA","math.CO","math.CT","math.CV","math.DG","math.DS","math.FA","math.GM","math.GN","math.GR","math.GT","math.HO","math.IT","math.KT","math.LO","math.MG","math.MP","math.NA","math.NT","math.OA","math.OC","math.PR","math.QA","math.RA","math.RT","math.SG","math.SP","math.ST","math-ph","nlin.AO","nlin.CD","nlin.CG","nlin.PS","nlin.SI","nucl-ex","nucl-th","physics.acc-ph","physics.ao-ph","physics.app-ph","physics.atm-clus","physics.atom-ph","physics.bio-ph","physics.chem-ph","physics.class-ph","physics.comp-ph","physics.data-an","physics.ed-ph","physics.flu-dyn","physics.gen-ph","physics.geo-ph","physics.hist-ph","physics.ins-det","physics.med-ph","physics.optics","physics.plasm-ph","physics.pop-ph","physics.soc-ph","physics.space-ph","q-bio.BM","q-bio.CB","q-bio.GN","q-bio.MN","q-bio.NC","q-bio.OT","q-bio.PE","q-bio.QM","q-bio.SC","q-bio.TO","q-fin.CP","q-fin.EC","q-fin.GN","q-fin.MF","q-fin.PM","q-fin.PR","q-fin.RM","q-fin.ST","q-fin.TR","quant-ph","stat.AP","stat.CO","stat.ME","stat.ML","stat.OT","stat.TH"),
                         lab = c("Astrophysics","Cosmology and Nongalactic Astrophysics","Earth and Planetary Astrophysics","Astrophysics of Galaxies","High Energy Astrophysical Phenomena","Instrumentation and Methods for Astrophysics","Solar and Stellar Astrophysics","Disordered Systems and Neural Networks","Mesoscale and Nanoscale Physics","Materials Science","Other Condensed Matter","Quantum Gases","Soft Condensed Matter","Statistical Mechanics","Strongly Correlated Electrons","Superconductivity","Artificial Intelligence","Hardware Architecture","Computational Complexity","Computational Engineering, Finance, and Science","Computational Geometry","Computation and Language","Cryptography and Security","Computer Vision and Pattern Recognition","Computers and Society","Databases","Distributed, Parallel, and Cluster Computing","Digital Libraries","Discrete Mathematics","Data Structures and Algorithms","Emerging Technologies","Formal Languages and Automata Theory","General Literature","Graphics","Computer Science and Game Theory","Human-Computer Interaction","Information Retrieval","Information Theory","Learning","Logic in Computer Science","Multiagent Systems","Multimedia","Mathematical Software","Numerical Analysis","Neural and Evolutionary Computing","Networking and Internet Architecture","Other Computer Science","Operating Systems","Performance","Programming Languages","Robotics","Symbolic Computation","Sound","Software Engineering","Social and Information Networks","Systems and Control","Econometrics","Audio and Speech Processing","Image and Video Processing","Signal Processing","General Relativity and Quantum Cosmology","High Energy Physics - Experiment","High Energy Physics - Lattice","High Energy Physics - Phenomenology","High Energy Physics - Theory","Commutative Algebra","Algebraic Geometry","Analysis of PDEs","Algebraic Topology","Classical Analysis and ODEs","Combinatorics","Category Theory","Complex Variables","Differential Geometry","Dynamical Systems","Functional Analysis","General Mathematics","General Topology","Group Theory","Geometric Topology","History and Overview","Information Theory","K-Theory and Homology","Logic","Metric Geometry","Mathematical Physics","Numerical Analysis","Number Theory","Operator Algebras","Optimization and Control","Probability","Quantum Algebra","Rings and Algebras","Representation Theory","Symplectic Geometry","Spectral Theory","Statistics Theory","Mathematical Physics","Adaptation and Self-Organizing Systems","Chaotic Dynamics","Cellular Automata and Lattice Gases","Pattern Formation and Solitons","Exactly Solvable and Integrable Systems","Nuclear Experiment","Nuclear Theory","Accelerator Physics","Atmospheric and Oceanic Physics","Applied Physics","Atomic and Molecular Clusters","Atomic Physics","Biological Physics","Chemical Physics","Classical Physics","Computational Physics","Data Analysis, Statistics and Probability","Physics Education","Fluid Dynamics","General Physics","Geophysics","History and Philosophy of Physics","Instrumentation and Detectors","Medical Physics","Optics","Plasma Physics","Popular Physics","Physics and Society","Space Physics","Biomolecules","Cell Behavior","Genomics","Molecular Networks","Neurons and Cognition","Other Quantitative Biology","Populations and Evolution","Quantitative Methods","Subcellular Processes","Tissues and Organs","Computational Finance","Economics","General Finance","Mathematical Finance","Portfolio Management","Pricing of Securities","Risk Management","Statistical Finance","Trading and Market Microstructure","Quantum Physics","Applications","Computation","Methodology","Machine Learning","Other Statistics","Statistics Theory"))


# Define category translation for arXiv
arxiv_cats <- data.frame(abb = c("astro-ph","astro-ph.CO","astro-ph.EP","astro-ph.GA","astro-ph.HE","astro-ph.IM","astro-ph.SR","cond-mat.dis-nn","cond-mat.mes-hall","cond-mat.mtrl-sci","cond-mat.other","cond-mat.quant-gas","cond-mat.soft","cond-mat.stat-mech","cond-mat.str-el","cond-mat.supr-con","cs.AI","cs.AR","cs.CC","cs.CE","cs.CG","cs.CL","cs.CR","cs.CV","cs.CY","cs.DB","cs.DC","cs.DL","cs.DM","cs.DS","cs.ET","cs.FL","cs.GL","cs.GR","cs.GT","cs.HC","cs.IR","cs.IT","cs.LG","cs.LO","cs.MA","cs.MM","cs.MS","cs.NA","cs.NE","cs.NI","cs.OH","cs.OS","cs.PF","cs.PL","cs.RO","cs.SC","cs.SD","cs.SE","cs.SI","cs.SY","econ.EM","eess.AS","eess.IV","eess.SP","gr-qc","hep-ex","hep-lat","hep-ph","hep-th","math.AC","math.AG","math.AP","math.AT","math.CA","math.CO","math.CT","math.CV","math.DG","math.DS","math.FA","math.GM","math.GN","math.GR","math.GT","math.HO","math.IT","math.KT","math.LO","math.MG","math.MP","math.NA","math.NT","math.OA","math.OC","math.PR","math.QA","math.RA","math.RT","math.SG","math.SP","math.ST","math-ph","nlin.AO","nlin.CD","nlin.CG","nlin.PS","nlin.SI","nucl-ex","nucl-th","physics.acc-ph","physics.ao-ph","physics.app-ph","physics.atm-clus","physics.atom-ph","physics.bio-ph","physics.chem-ph","physics.class-ph","physics.comp-ph","physics.data-an","physics.ed-ph","physics.flu-dyn","physics.gen-ph","physics.geo-ph","physics.hist-ph","physics.ins-det","physics.med-ph","physics.optics","physics.plasm-ph","physics.pop-ph","physics.soc-ph","physics.space-ph","q-bio.BM","q-bio.CB","q-bio.GN","q-bio.MN","q-bio.NC","q-bio.OT","q-bio.PE","q-bio.QM","q-bio.SC","q-bio.TO","q-fin.CP","q-fin.EC","q-fin.GN","q-fin.MF","q-fin.PM","q-fin.PR","q-fin.RM","q-fin.ST","q-fin.TR","quant-ph","stat.AP","stat.CO","stat.ME","stat.ML","stat.OT","stat.TH"),
                         lab = c("Astrophysics","Cosmology and Nongalactic Astrophysics","Earth and Planetary Astrophysics","Astrophysics of Galaxies","High Energy Astrophysical Phenomena","Instrumentation and Methods for Astrophysics","Solar and Stellar Astrophysics","Disordered Systems and Neural Networks","Mesoscale and Nanoscale Physics","Materials Science","Other Condensed Matter","Quantum Gases","Soft Condensed Matter","Statistical Mechanics","Strongly Correlated Electrons","Superconductivity","Artificial Intelligence","Hardware Architecture","Computational Complexity","Computational Engineering, Finance, and Science","Computational Geometry","Computation and Language","Cryptography and Security","Computer Vision and Pattern Recognition","Computers and Society","Databases","Distributed, Parallel, and Cluster Computing","Digital Libraries","Discrete Mathematics","Data Structures and Algorithms","Emerging Technologies","Formal Languages and Automata Theory","General Literature","Graphics","Computer Science and Game Theory","Human-Computer Interaction","Information Retrieval","Information Theory","Learning","Logic in Computer Science","Multiagent Systems","Multimedia","Mathematical Software","Numerical Analysis","Neural and Evolutionary Computing","Networking and Internet Architecture","Other Computer Science","Operating Systems","Performance","Programming Languages","Robotics","Symbolic Computation","Sound","Software Engineering","Social and Information Networks","Systems and Control","Econometrics","Audio and Speech Processing","Image and Video Processing","Signal Processing","General Relativity and Quantum Cosmology","High Energy Physics - Experiment","High Energy Physics - Lattice","High Energy Physics - Phenomenology","High Energy Physics - Theory","Commutative Algebra","Algebraic Geometry","Analysis of PDEs","Algebraic Topology","Classical Analysis and ODEs","Combinatorics","Category Theory","Complex Variables","Differential Geometry","Dynamical Systems","Functional Analysis","General Mathematics","General Topology","Group Theory","Geometric Topology","History and Overview","Information Theory","K-Theory and Homology","Logic","Metric Geometry","Mathematical Physics","Numerical Analysis","Number Theory","Operator Algebras","Optimization and Control","Probability","Quantum Algebra","Rings and Algebras","Representation Theory","Symplectic Geometry","Spectral Theory","Statistics Theory","Mathematical Physics","Adaptation and Self-Organizing Systems","Chaotic Dynamics","Cellular Automata and Lattice Gases","Pattern Formation and Solitons","Exactly Solvable and Integrable Systems","Nuclear Experiment","Nuclear Theory","Accelerator Physics","Atmospheric and Oceanic Physics","Applied Physics","Atomic and Molecular Clusters","Atomic Physics","Biological Physics","Chemical Physics","Classical Physics","Computational Physics","Data Analysis, Statistics and Probability","Physics Education","Fluid Dynamics","General Physics","Geophysics","History and Philosophy of Physics","Instrumentation and Detectors","Medical Physics","Optics","Plasma Physics","Popular Physics","Physics and Society","Space Physics","Biomolecules","Cell Behavior","Genomics","Molecular Networks","Neurons and Cognition","Other Quantitative Biology","Populations and Evolution","Quantitative Methods","Subcellular Processes","Tissues and Organs","Computational Finance","Economics","General Finance","Mathematical Finance","Portfolio Management","Pricing of Securities","Risk Management","Statistical Finance","Trading and Market Microstructure","Quantum Physics","Applications","Computation","Methodology","Machine Learning","Other Statistics","Statistics Theory"))

# Define functions
fetch_preprints <- function(query, server, exc=NULL, page_size = 250){
  
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
    df$url <- df$biorxiv_url
    return(df[!duplicated(df$id),]) # Remove pre-prints duplicated between search queries
    
  } else if (server=="arxiv"){
    
    # Search using arXiv API  
    result_list <- xmlToList(xmlParse(
      paste0("http://export.arxiv.org/api/query?search_query=all:",paste(query, collapse="+OR+"),"&start=0&max_results=", page_size)))
    
    # Create dataframe for arXiv results using same structure as to bioRxiv results
    df <- data.frame(title = unlist(lapply(result_list[8:length(result_list)], function(x) x$title)),
                     url = unlist(lapply(result_list[8:length(result_list)], function(x) x$id)),
                     doi = gsub("http://arxiv.org/abs/", "arxiv:", unlist(lapply(result_list[8:length(result_list)], function(x) x$link[1]))),
                     category = unlist(lapply(result_list[8:length(result_list)], function(x) x$primary_category[1])),
                     first_posted = as.Date(unlist(lapply(result_list[8:length(result_list)], function(x) x$published))))
    
    df$category <- factor(arxiv_cats$lab[match(df$category, arxiv_cats$abb)])  # Replace abbreviation with full category label
    
    return(df)
    
  } else if (server=="medrxiv") {
    
    # Search using medrxiv R package  
    result_list <- mx_search(c(query, toupper(query), tolower(query), stringr::str_to_title(query)), deduplicate = FALSE, NOT = exc) # use all case variants
    
    # Create dataframe for arXiv results using same structure as to bioRxiv results
    df <- data.frame(title = result_list$title,
                     url = paste0("http://medrxiv.org", gsub("\\?.*", "",result_list$link)),
                     doi = gsub("v.*", "", gsub("/content/","",result_list$link)),
                     category = gsub("\n.*","",gsub("\\\\.*","",result_list$subject)),
                     first_posted = as.Date(strptime(result_list$date, "%Y%m%d")))

    df <- df[order(df$first_posted),] # Remove NAs in date
    df <- df[order(df$first_posted),] # Order by date
    
    # Replace missing categories
    df$category <- as.character(df$category)
    df$category[is.na(df$category)] <- "No category recorded"
    df$category <- factor(df$category)
    
    return(df[!duplicated(df$doi),]) # Remove revisions of preprints (keep only first upload date)
    
  } else {
    stop("no valid server selected")
  }
  
}

process_preprints <- function(df, startdate, enddate){
  df <- subset(df, first_posted > startdate & first_posted <= enddate)
  df$category <- factor(df$category)
  df$first_posted <- as.Date(df$first_posted, format="%Y-%m-%d")
  df <- df[with(df, order(first_posted)), ]
  if(nrow(df) != 0){
    df$total_papers <- 1:nrow(df)
  }
  return(df)
}

plot_preprints <- function(df, server, pointsize=3){
  
  g <- ggplot(df, aes(x = first_posted, y = total_papers, label = title, label2 = doi)) +  
    geom_step(data = df, mapping=aes(x = first_posted, y = total_papers, group=1), alpha=0.5) +
    geom_point(aes(color = category), size=pointsize) +
    xlab('Date') +  
    ylab(paste0(server, " pre-prints posted")) +
    scale_x_date(limits = c(as.Date('2020-01-19'),as.Date(Sys.time()))) +
    annotate("text", x = as.Date('2020-01-24'), y = max(df$total_papers)-1, 
             label = paste0("Data up to: ", max(df$first_posted),"\nCurve produced: ",as.Date(Sys.time())), size = 6) +
    theme_bw(base_size = 17)
  ggp <- ggplotly(g)
  
  # Add in hyperlink data to plot
  ggp$x$data[[1]]$customdata <- df$url
  for (i in 1:nlevels(df$category)){
    ggp$x$data[[1+i]]$customdata <- subset(df, category == levels(df$category)[i])$url
  }
  
  # Save curve
  saveWidget(as_widget(onRender(ggp, "function(el, x) {
                el.on('plotly_click', function(d) {
                var url = d.points[0].customdata;
                //url
                window.open(url);
                });
                }"
  )), 
  paste0("M:\\Git\\lbrierley.github.io\\cov_preprints\\",tolower(server),"_cov_preprints.html"), 
  title = paste0(server," COVID-19 preprint curve"))
  
}

# Complete function for ease
preprint_track <- function(query, server, startdate, enddate = as.Date(Sys.time(), format="%Y-%m-%d"), pointsize=3, exc=NULL){
  rxiv <- fetch_preprints(query, server = tolower(server), exc)
  rxiv <- process_preprints(rxiv, startdate, enddate)
  plot_preprints(rxiv, server, pointsize)
}

# Extract preprint archive information (currently bioRxiv, arXiv, medRxiv)

cov_query <- c("coronavirus","coronaviruses","ncov","SARS-CoV-2","COVID-19") # Set search query
cov_start <- "2019-12-29" # Set start date defined as notification of first cluster

preprint_track(cov_query, server = "bioRxiv", startdate = cov_start)
preprint_track(cov_query, server = "arXiv", startdate = cov_start)
preprint_track(cov_query, server = "medRxiv", startdate = cov_start, pointsize=1.5, exc="uncover") # Exclude false positive generated by *ncov*
