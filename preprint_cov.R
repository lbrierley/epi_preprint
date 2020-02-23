##########################################
# Preprint tracker for SARS-Cov-2        #
# Liam Brierley, University of Liverpool #
# 23/2/20                                #
##########################################

rm(list=ls())

# Load required libraries
library(ggplot2)
library(htmlwidgets)
library(jsonlite)
library(plotly)
library(XML)

# Define datasets, functions
arxiv_cats <- {
  structure(list(abb = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 
                                   8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 
                                   21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 
                                   34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 
                                   47L, 48L, 49L, 50L, 51L, 52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 
                                   60L, 61L, 62L, 63L, 64L, 65L, 67L, 68L, 69L, 70L, 71L, 72L, 73L, 
                                   74L, 75L, 76L, 77L, 78L, 79L, 80L, 81L, 82L, 83L, 84L, 85L, 86L, 
                                   87L, 88L, 89L, 90L, 91L, 92L, 93L, 94L, 95L, 96L, 97L, 98L, 66L, 
                                   99L, 100L, 101L, 102L, 103L, 104L, 105L, 106L, 107L, 108L, 109L, 
                                   110L, 111L, 112L, 113L, 114L, 115L, 116L, 117L, 118L, 119L, 120L, 
                                   121L, 122L, 123L, 124L, 125L, 126L, 127L, 128L, 129L, 130L, 131L, 
                                   132L, 133L, 134L, 135L, 136L, 137L, 138L, 139L, 140L, 141L, 142L, 
                                   143L, 144L, 145L, 146L, 147L, 148L, 149L, 150L, 151L, 152L, 153L
  ), .Label = c("astro-ph", "astro-ph.CO", "astro-ph.EP", "astro-ph.GA", 
                "astro-ph.HE", "astro-ph.IM", "astro-ph.SR", "cond-mat.dis-nn", 
                "cond-mat.mes-hall", "cond-mat.mtrl-sci", "cond-mat.other", "cond-mat.quant-gas", 
                "cond-mat.soft", "cond-mat.stat-mech", "cond-mat.str-el", "cond-mat.supr-con", 
                "cs.AI", "cs.AR", "cs.CC", "cs.CE", "cs.CG", "cs.CL", "cs.CR", 
                "cs.CV", "cs.CY", "cs.DB", "cs.DC", "cs.DL", "cs.DM", "cs.DS", 
                "cs.ET", "cs.FL", "cs.GL", "cs.GR", "cs.GT", "cs.HC", "cs.IR", 
                "cs.IT", "cs.LG", "cs.LO", "cs.MA", "cs.MM", "cs.MS", "cs.NA", 
                "cs.NE", "cs.NI", "cs.OH", "cs.OS", "cs.PF", "cs.PL", "cs.RO", 
                "cs.SC", "cs.SD", "cs.SE", "cs.SI", "cs.SY", "econ.EM", "eess.AS", 
                "eess.IV", "eess.SP", "gr-qc", "hep-ex", "hep-lat", "hep-ph", 
                "hep-th", "math-ph", "math.AC", "math.AG", "math.AP", "math.AT", 
                "math.CA", "math.CO", "math.CT", "math.CV", "math.DG", "math.DS", 
                "math.FA", "math.GM", "math.GN", "math.GR", "math.GT", "math.HO", 
                "math.IT", "math.KT", "math.LO", "math.MG", "math.MP", "math.NA", 
                "math.NT", "math.OA", "math.OC", "math.PR", "math.QA", "math.RA", 
                "math.RT", "math.SG", "math.SP", "math.ST", "nlin.AO", "nlin.CD", 
                "nlin.CG", "nlin.PS", "nlin.SI", "nucl-ex", "nucl-th", "physics.acc-ph", 
                "physics.ao-ph", "physics.app-ph", "physics.atm-clus", "physics.atom-ph", 
                "physics.bio-ph", "physics.chem-ph", "physics.class-ph", "physics.comp-ph", 
                "physics.data-an", "physics.ed-ph", "physics.flu-dyn", "physics.gen-ph", 
                "physics.geo-ph", "physics.hist-ph", "physics.ins-det", "physics.med-ph", 
                "physics.optics", "physics.plasm-ph", "physics.pop-ph", "physics.soc-ph", 
                "physics.space-ph", "q-bio.BM", "q-bio.CB", "q-bio.GN", "q-bio.MN", 
                "q-bio.NC", "q-bio.OT", "q-bio.PE", "q-bio.QM", "q-bio.SC", "q-bio.TO", 
                "q-fin.CP", "q-fin.EC", "q-fin.GN", "q-fin.MF", "q-fin.PM", "q-fin.PR", 
                "q-fin.RM", "q-fin.ST", "q-fin.TR", "quant-ph", "stat.AP", "stat.CO", 
                "stat.ME", "stat.ML", "stat.OT", "stat.TH"), class = "factor"), 
  lab = structure(c(9L, 37L, 48L, 10L, 68L, 80L, 135L, 45L, 
                    91L, 86L, 109L, 125L, 133L, 140L, 142L, 144L, 8L, 67L, 29L, 
                    30L, 32L, 28L, 38L, 35L, 36L, 41L, 46L, 43L, 44L, 40L, 51L, 
                    54L, 57L, 65L, 34L, 75L, 77L, 78L, 82L, 84L, 95L, 96L, 89L, 
                    103L, 98L, 97L, 108L, 104L, 113L, 122L, 130L, 145L, 136L, 
                    134L, 132L, 147L, 49L, 14L, 76L, 131L, 60L, 69L, 70L, 71L, 
                    72L, 25L, 3L, 5L, 4L, 22L, 24L, 17L, 26L, 42L, 47L, 55L, 
                    58L, 61L, 66L, 63L, 73L, 78L, 81L, 83L, 93L, 88L, 103L, 102L, 
                    105L, 107L, 121L, 124L, 128L, 127L, 146L, 138L, 141L, 88L, 
                    2L, 20L, 19L, 112L, 52L, 100L, 101L, 1L, 11L, 7L, 12L, 13L, 
                    15L, 21L, 23L, 33L, 39L, 115L, 53L, 59L, 64L, 74L, 79L, 90L, 
                    106L, 116L, 117L, 114L, 137L, 16L, 18L, 62L, 94L, 99L, 110L, 
                    118L, 123L, 143L, 148L, 31L, 50L, 56L, 87L, 119L, 120L, 129L, 
                    139L, 149L, 126L, 6L, 27L, 92L, 85L, 111L, 141L), .Label = c("Accelerator Physics", 
                                                                                 "Adaptation and Self-Organizing Systems", "Algebraic Geometry", 
                                                                                 "Algebraic Topology", "Analysis of PDEs", "Applications", 
                                                                                 "Applied Physics", "Artificial Intelligence", "Astrophysics", 
                                                                                 "Astrophysics of Galaxies", "Atmospheric and Oceanic Physics", 
                                                                                 "Atomic and Molecular Clusters", "Atomic Physics", "Audio and Speech Processing", 
                                                                                 "Biological Physics", "Biomolecules", "Category Theory", 
                                                                                 "Cell Behavior", "Cellular Automata and Lattice Gases", "Chaotic Dynamics", 
                                                                                 "Chemical Physics", "Classical Analysis and ODEs", "Classical Physics", 
                                                                                 "Combinatorics", "Commutative Algebra", "Complex Variables", 
                                                                                 "Computation", "Computation and Language", "Computational Complexity", 
                                                                                 "Computational Engineering, Finance, and Science", "Computational Finance", 
                                                                                 "Computational Geometry", "Computational Physics", "Computer Science and Game Theory", 
                                                                                 "Computer Vision and Pattern Recognition", "Computers and Society", 
                                                                                 "Cosmology and Nongalactic Astrophysics", "Cryptography and Security", 
                                                                                 "Data Analysis, Statistics and Probability", "Data Structures and Algorithms", 
                                                                                 "Databases", "Differential Geometry", "Digital Libraries", 
                                                                                 "Discrete Mathematics", "Disordered Systems and Neural Networks", 
                                                                                 "Distributed, Parallel, and Cluster Computing", "Dynamical Systems", 
                                                                                 "Earth and Planetary Astrophysics", "Econometrics", "Economics", 
                                                                                 "Emerging Technologies", "Exactly Solvable and Integrable Systems", 
                                                                                 "Fluid Dynamics", "Formal Languages and Automata Theory", 
                                                                                 "Functional Analysis", "General Finance", "General Literature", 
                                                                                 "General Mathematics", "General Physics", "General Relativity and Quantum Cosmology", 
                                                                                 "General Topology", "Genomics", "Geometric Topology", "Geophysics", 
                                                                                 "Graphics", "Group Theory", "Hardware Architecture", "High Energy Astrophysical Phenomena", 
                                                                                 "High Energy Physics - Experiment", "High Energy Physics - Lattice", 
                                                                                 "High Energy Physics - Phenomenology", "High Energy Physics - Theory", 
                                                                                 "History and Overview", "History and Philosophy of Physics", 
                                                                                 "Human-Computer Interaction", "Image and Video Processing", 
                                                                                 "Information Retrieval", "Information Theory", "Instrumentation and Detectors", 
                                                                                 "Instrumentation and Methods for Astrophysics", "K-Theory and Homology", 
                                                                                 "Learning", "Logic", "Logic in Computer Science", "Machine Learning", 
                                                                                 "Materials Science", "Mathematical Finance", "Mathematical Physics", 
                                                                                 "Mathematical Software", "Medical Physics", "Mesoscale and Nanoscale Physics", 
                                                                                 "Methodology", "Metric Geometry", "Molecular Networks", "Multiagent Systems", 
                                                                                 "Multimedia", "Networking and Internet Architecture", "Neural and Evolutionary Computing", 
                                                                                 "Neurons and Cognition", "Nuclear Experiment", "Nuclear Theory", 
                                                                                 "Number Theory", "Numerical Analysis", "Operating Systems", 
                                                                                 "Operator Algebras", "Optics", "Optimization and Control", 
                                                                                 "Other Computer Science", "Other Condensed Matter", "Other Quantitative Biology", 
                                                                                 "Other Statistics", "Pattern Formation and Solitons", "Performance", 
                                                                                 "Physics and Society", "Physics Education", "Plasma Physics", 
                                                                                 "Popular Physics", "Populations and Evolution", "Portfolio Management", 
                                                                                 "Pricing of Securities", "Probability", "Programming Languages", 
                                                                                 "Quantitative Methods", "Quantum Algebra", "Quantum Gases", 
                                                                                 "Quantum Physics", "Representation Theory", "Rings and Algebras", 
                                                                                 "Risk Management", "Robotics", "Signal Processing", "Social and Information Networks", 
                                                                                 "Soft Condensed Matter", "Software Engineering", "Solar and Stellar Astrophysics", 
                                                                                 "Sound", "Space Physics", "Spectral Theory", "Statistical Finance", 
                                                                                 "Statistical Mechanics", "Statistics Theory", "Strongly Correlated Electrons", 
                                                                                 "Subcellular Processes", "Superconductivity", "Symbolic Computation", 
                                                                                 "Symplectic Geometry", "Systems and Control", "Tissues and Organs", 
                                                                                 "Trading and Market Microstructure"), class = "factor")), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                               -153L))
}

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
  scale_x_date(limits = as.Date(c('2020-01-16','2020-02-21'))) +
  theme_bw(base_size = 17)
p2 <- ggplotly(g2)

# Add in hyperlink data to plot
p2$x$data[[1]]$customdata <- biorxiv_cov$biorxiv_url
for (i in 1:nlevels(biorxiv_cov$category)){
  p2$x$data[[1+i]]$customdata <- subset(biorxiv_cov, category == levels(biorxiv_cov$category)[i])$biorxiv_url
}

# Plot biorxiv curve
saveWidget(as_widget(onRender(p2, js)), "biorxiv_cov_preprints.html")

# Replace abbreviation with full category label before plotting arxiv
arxiv_cov$category <- factor(arxiv_cats$lab[match(arxiv_cov$category, arxiv_cats$abb)])

g3 <- ggplot(arxiv_cov, aes(x = first_posted, y = total_papers, label = title, label2 = doi)) +  
  geom_step(data = arxiv_cov, mapping=aes(x = first_posted, y = total_papers, group=1), alpha=0.5) +
  geom_point(aes(color = category), size=3) +
  xlab('Date') +  
  ylab("arXiv pre-prints posted") +
  scale_x_date(limits = as.Date(c('2020-01-16','2020-02-21'))) +
  theme_bw(base_size = 17)
p3 <- ggplotly(g3)

# Add in hyperlink data to plot
p3$x$data[[1]]$customdata <- arxiv_cov$url
for (i in 1:nlevels(arxiv_cov$category)){
  p3$x$data[[1+i]]$customdata <- subset(arxiv_cov, category == levels(arxiv_cov$category)[i])$url
}

# Plot arxiv curve
saveWidget(as_widget(onRender(p3, js)), "arxiv_cov_preprints.html")