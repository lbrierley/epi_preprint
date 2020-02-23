### Preprint uploads during the SARS-CoV-2 (COVID-19) epidemic

`preprint_cov.R` conducts search queries of preprint servers and extracts metadata using respective APIs to plot the rise in preprints addressing the current epidemic over time.

Searches are conducted for SARS-CoV-2 using keywords `"coronavirus", "coronaviruses", "ncov", "SARS-CoV-2", "COVID-19"`. Preprint counts are plotted as both step curves and interactive points hyperlinking to preprints upon click. Colour code denotes preprint server categorisation.

Interactive plot demo for [bioRxiv](https://lbrierley.github.io/cov_preprints/biorxiv_cov_preprints.html), [arXiv](https://lbrierley.github.io/cov_preprints/arxiv_cov_preprints.html). 

Only [bioRxiv](https://biorxiv.org) (using the [Rxivist API](https://rxivist.org/)) and [arXiv](https://arxiv.org) (using the [arXiv API](https://arxiv.org/help/api)) are supported. Further SARS-CoV-2 research is available from other preprint repositories, e.g. [medRxiv](https://medrxiv.org), though no API is currently available. Data are subject to timeliness of upload metadata reaching the API.
