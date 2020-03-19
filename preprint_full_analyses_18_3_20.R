#################################################################
# Supporting code for ms 'The role of research preprints in the #
#          academic response to the COVID-19 epidemic'          #
#                                                               #
# Liam Brierley, Dept of Biostatistics, University of Liverpool #
#                            18/3/20                            #
#################################################################

rm(list=ls())

# Load required libraries
library(data.table)
library(ggplot2)
library(gridExtra)
library(htmlwidgets)
library(jsonlite)
library(medrxivr)
library(plotly)
library(reshape2)
library(XML)



# Set working directory
setwd("M:\\Git\\lbrierley.github.io\\cov_preprints\\")

# Define category translation for arXiv
arxiv_cats <- data.frame(abb = c("astro-ph","astro-ph.CO","astro-ph.EP","astro-ph.GA","astro-ph.HE","astro-ph.IM","astro-ph.SR","cond-mat.dis-nn","cond-mat.mes-hall","cond-mat.mtrl-sci","cond-mat.other","cond-mat.quant-gas","cond-mat.soft","cond-mat.stat-mech","cond-mat.str-el","cond-mat.supr-con","cs.AI","cs.AR","cs.CC","cs.CE","cs.CG","cs.CL","cs.CR","cs.CV","cs.CY","cs.DB","cs.DC","cs.DL","cs.DM","cs.DS","cs.ET","cs.FL","cs.GL","cs.GR","cs.GT","cs.HC","cs.IR","cs.IT","cs.LG","cs.LO","cs.MA","cs.MM","cs.MS","cs.NA","cs.NE","cs.NI","cs.OH","cs.OS","cs.PF","cs.PL","cs.RO","cs.SC","cs.SD","cs.SE","cs.SI","cs.SY","econ.GN","econ.EM","eess.AS","eess.IV","eess.SP","gr-qc","hep-ex","hep-lat","hep-ph","hep-th","math.AC","math.AG","math.AP","math.AT","math.CA","math.CO","math.CT","math.CV","math.DG","math.DS","math.FA","math.GM","math.GN","math.GR","math.GT","math.HO","math.IT","math.KT","math.LO","math.MG","math.MP","math.NA","math.NT","math.OA","math.OC","math.PR","math.QA","math.RA","math.RT","math.SG","math.SP","math.ST","math-ph","nlin.AO","nlin.CD","nlin.CG","nlin.PS","nlin.SI","nucl-ex","nucl-th","physics.acc-ph","physics.ao-ph","physics.app-ph","physics.atm-clus","physics.atom-ph","physics.bio-ph","physics.chem-ph","physics.class-ph","physics.comp-ph","physics.data-an","physics.ed-ph","physics.flu-dyn","physics.gen-ph","physics.geo-ph","physics.hist-ph","physics.ins-det","physics.med-ph","physics.optics","physics.plasm-ph","physics.pop-ph","physics.soc-ph","physics.space-ph","q-bio.BM","q-bio.CB","q-bio.GN","q-bio.MN","q-bio.NC","q-bio.OT","q-bio.PE","q-bio.QM","q-bio.SC","q-bio.TO","q-fin.CP","q-fin.EC","q-fin.GN","q-fin.MF","q-fin.PM","q-fin.PR","q-fin.RM","q-fin.ST","q-fin.TR","quant-ph","stat.AP","stat.CO","stat.ME","stat.ML","stat.OT","stat.TH"),
                         lab = c("Astrophysics","Cosmology and Nongalactic Astrophysics","Earth and Planetary Astrophysics","Astrophysics of Galaxies","High Energy Astrophysical Phenomena","Instrumentation and Methods for Astrophysics","Solar and Stellar Astrophysics","Disordered Systems and Neural Networks","Mesoscale and Nanoscale Physics","Materials Science","Other Condensed Matter","Quantum Gases","Soft Condensed Matter","Statistical Mechanics","Strongly Correlated Electrons","Superconductivity","Artificial Intelligence","Hardware Architecture","Computational Complexity","Computational Engineering, Finance, and Science","Computational Geometry","Computation and Language","Cryptography and Security","Computer Vision and Pattern Recognition","Computers and Society","Databases","Distributed, Parallel, and Cluster Computing","Digital Libraries","Discrete Mathematics","Data Structures and Algorithms","Emerging Technologies","Formal Languages and Automata Theory","General Literature","Graphics","Computer Science and Game Theory","Human-Computer Interaction","Information Retrieval","Information Theory","Learning","Logic in Computer Science","Multiagent Systems","Multimedia","Mathematical Software","Numerical Analysis","Neural and Evolutionary Computing","Networking and Internet Architecture","Other Computer Science","Operating Systems","Performance","Programming Languages","Robotics","Symbolic Computation","Sound","Software Engineering","Social and Information Networks","Systems and Control","General Economics","Econometrics","Audio and Speech Processing","Image and Video Processing","Signal Processing","General Relativity and Quantum Cosmology","High Energy Physics - Experiment","High Energy Physics - Lattice","High Energy Physics - Phenomenology","High Energy Physics - Theory","Commutative Algebra","Algebraic Geometry","Analysis of PDEs","Algebraic Topology","Classical Analysis and ODEs","Combinatorics","Category Theory","Complex Variables","Differential Geometry","Dynamical Systems","Functional Analysis","General Mathematics","General Topology","Group Theory","Geometric Topology","History and Overview","Information Theory","K-Theory and Homology","Logic","Metric Geometry","Mathematical Physics","Numerical Analysis","Number Theory","Operator Algebras","Optimization and Control","Probability","Quantum Algebra","Rings and Algebras","Representation Theory","Symplectic Geometry","Spectral Theory","Statistics Theory","Mathematical Physics","Adaptation and Self-Organizing Systems","Chaotic Dynamics","Cellular Automata and Lattice Gases","Pattern Formation and Solitons","Exactly Solvable and Integrable Systems","Nuclear Experiment","Nuclear Theory","Accelerator Physics","Atmospheric and Oceanic Physics","Applied Physics","Atomic and Molecular Clusters","Atomic Physics","Biological Physics","Chemical Physics","Classical Physics","Computational Physics","Data Analysis, Statistics and Probability","Physics Education","Fluid Dynamics","General Physics","Geophysics","History and Philosophy of Physics","Instrumentation and Detectors","Medical Physics","Optics","Plasma Physics","Popular Physics","Physics and Society","Space Physics","Biomolecules","Cell Behavior","Genomics","Molecular Networks","Neurons and Cognition","Other Quantitative Biology","Populations and Evolution","Quantitative Methods","Subcellular Processes","Tissues and Organs","Computational Finance","Economics","General Finance","Mathematical Finance","Portfolio Management","Pricing of Securities","Risk Management","Statistical Finance","Trading and Market Microstructure","Quantum Physics","Applications","Computation","Methodology","Machine Learning","Other Statistics","Statistics Theory"))

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
    return(df[!duplicated(df$id),]) # Remove preprints duplicated between search queries
    
  } else if (server=="arxiv"){
    
    # Search using arXiv API  
    result_list <- xmlToList(xmlParse(
      paste0("http://export.arxiv.org/api/query?search_query=all:",paste(query, collapse="+OR+"),"&start=0&max_results=", page_size)))
    
    # Create dataframe for arXiv results using same structure as to bioRxiv results
    df <- data.frame(title = unlist(lapply(result_list[8:length(result_list)], function(x) x$title)),
                     abstract = unlist(lapply(result_list[8:length(result_list)], function(x) x$summary)),
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
                     abstract = result_list$abstract,
                     url = paste0("http://medrxiv.org", gsub("\\?.*", "",result_list$link)),
                     doi = gsub("v.*", "", gsub("/content/","",result_list$link)),
                     category = gsub("\n.*","",result_list$subject),
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
    ylab(paste0(server, " preprints posted")) +
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
  paste0(tolower(server),"_cov_preprints.html"), 
  title = paste0(server," COVID-19 preprint curve"))
  
}

# Complete functions for ease
preprint_track_plot <- function(query, server, startdate, enddate = as.Date(Sys.time(), format="%Y-%m-%d"), pointsize=3, exc=NULL){
  rxiv <- fetch_preprints(query, server = tolower(server), exc)
  rxiv <- process_preprints(rxiv, startdate, enddate)
  plot_preprints(rxiv, server, pointsize)
}

preprint_track_all <- function(query, startdate, enddate = as.Date(Sys.time(), format="%Y-%m-%d"), exc=NULL, name=NULL, return_data=FALSE){
  df <- data.table::rbindlist(
    lapply(c("biorxiv", "arxiv", "medrxiv"), function(x) {
      df <- fetch_preprints(query, server = x, exc)
      df$server <- x
      df <- process_preprints(df, startdate, enddate)
      df[,c("title","abstract","url","doi","category","first_posted","server")]
    }
    )
  )
  if (return_data == TRUE) {
    return(df) # Return full data frame of preprints
  } else {
    cumfreq <- as.data.frame(table(cut(as.Date(df$first_posted), "day")))
    cumfreq$days <- as.numeric(as.Date(cumfreq$Var1) - as.Date(startdate))
    cumfreq$total <- cumsum(cumfreq$Freq)
    cumfreq$virus <- name
    return(cumfreq) # Else return cumulative frequency 
  }
}



# Extract preprint information for COVID-19
cov_query <- c("coronavirus","coronaviruses","ncov","SARS-CoV-2","COVID-19") # Set search query
cov_start <- "2019-12-29" # Set start date defined as notification of first cluster

# Plot interactive preprint curves for COVID-19
preprint_track_plot(cov_query, server = "bioRxiv", startdate = cov_start)
preprint_track_plot(cov_query, server = "arXiv", startdate = cov_start)
preprint_track_plot(cov_query, server = "medRxiv", startdate = cov_start, pointsize=1.2, exc="uncover") # Exclude false positive generated by *ncov*

# Count totals
cov_all <- preprint_track_all(cov_query, cov_start, exc="uncover", return_data=TRUE)
table(cov_all$server) # totals per server
nrow(cov_all) # grand total
prop.table(table(cov_all$server)) # proportion per server



# Extract preprint information for additional pathogens and plot comparative curves
ebov_query <- c("ebola", "ebolavirus", "ebolaviruses", "ZEBOV")
ebov_start <- "2014-01-24"
ebov_end <- "2016-01-24"

zika_query <- c("zika", "ZIKV")
zika_start <- "2015-04-01"
zika_end <- "2017-04-01"

flu_query <- c("influenza")
flu_start <- "2019-09-30"

chol_query <- c("cholera", "vibrio+cholerae", "v.+cholerae")
chol_start <- "2016-10-06"
chol_end <- "2018-10-06"

total_curves <- rbind(preprint_track_all(cov_query, cov_start, exc="uncover", name="SARS-CoV-2, 2019"), 
                      preprint_track_all(ebov_query, ebov_start, ebov_end, name="Zaire ebolavirus, 2014"),
                      preprint_track_all(zika_query, zika_start, zika_end, name="Zika virus, 2015"),
                      preprint_track_all(flu_query, flu_start, name="Seasonal influenza, 2019"),
                      preprint_track_all(chol_query, chol_start, chol_end, name="Cholera, 2016"))

# Set plot legend order
total_curves$virus <- factor(total_curves$virus, levels = c("Zaire ebolavirus, 2014", "Zika virus, 2015", "Cholera, 2016", "Seasonal influenza, 2019", "SARS-CoV-2, 2019"))

# Produce comparative preprint curves
ggplot(total_curves, aes(days, total, color=virus)) +  
  geom_line(lwd=1) + 
  xlab('Days since first reported cluster') +  
  ylab("Total preprints posted") +
  theme_bw(base_size = 11) +
  theme(legend.justification=c(1,1), legend.position=c(.9,.9), legend.title=element_blank(), legend.box.background = element_rect(colour = "black")) +
  scale_color_manual(values=c("#009E73","#0072B2","#CC79A7","#F0E442","#D55E00"))

# Estimate rate of preprint posting through fitting OLS linear regression
dt <- data.table(total_curves)
rate_dt <- dt[,list(rate=lm(total~days)$coef["days"]),by=virus]
rate_dt$relrate <- max(rate_dt$rate)/rate_dt$rate
rate_dt$daysperpaper <- 1/rate_dt$rate
rate_dt[,2:4] <- round(rate_dt[,2:4],3)
rate_dt

# Extract preprint information for each year of seasonal influenza and plot comparative curves
flu_curves <- rbind(preprint_track_all(flu_query, "2019-09-30", name="2019"), 
                    preprint_track_all(flu_query, "2018-09-30", "2019-09-30", name="2018"),
                    preprint_track_all(flu_query, "2017-09-30", "2018-09-30", name="2017"),
                    preprint_track_all(flu_query, "2016-09-30", "2017-09-30", name="2016"),
                    preprint_track_all(flu_query, "2015-09-30", "2016-09-30", name="2015"),
                    preprint_track_all(flu_query, "2014-09-30", "2015-09-30", name="2014"))

ggplot(flu_curves, aes(days, total, color=virus)) +  
  geom_line(lwd=1) + 
  xlab('Days since seasonal flu period onset') +  
  ylab("Total influenza preprints posted") +
  theme_bw(base_size = 11) +
  theme(legend.justification=c(1,1), legend.position=c(.16,.91), legend.title=element_blank(), legend.box.background = element_rect(colour = "black")) +
  scale_color_manual(values=c("#D55E00","#F0E442","#009E73","#56B4E9","#0072B2","#CC79A7"))



# Read in and plot Google Trends values
gt <- read.csv("gt.csv")
gt$Date <- as.Date(gt$Date, format="%d/%m/%Y")
gt <- melt(gt, id.vars = "Date")
gt$variable <- gsub("\\.","+",gt$variable)

ggplot(gt, aes(x = Date, y = value)) +  
  geom_line(lwd=1, aes(color=variable)) + 
  scale_color_manual(values=c("#0072B2","#D55E00")) +
  geom_vline(xintercept=as.Date("2020-01-22"), color="#D55E00", lwd=0.8, lty="dashed", alpha=0.6) +
  geom_vline(xintercept=as.Date("2020-01-31"), color="#0072B2", lwd=0.8, lty="dashed", alpha=0.6) +
  xlab("Date") +  
  ylab("Relative Google Trends search interest") +
  scale_x_date(limits = c(as.Date('2019-12-29'),as.Date('2020-02-22'))) +
  labs(color = "search terms") +
  theme_bw(base_size = 11) +
  theme(legend.justification=c(1,1), legend.position=c(.95,.9), legend.box.background = element_rect(colour = "black"))
