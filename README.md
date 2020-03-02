### Preprint uploads during the COVID-19 (SARS-CoV-2) epidemic

`preprint_cov.R` conducts search queries of preprint servers and extracts metadata using respective APIs to plot the rise in preprints addressing the current epidemic over time.

Searches are conducted for COVID-19 using keywords `"coronavirus", "coronaviruses", "ncov", "SARS-CoV-2", "COVID-19"`. Preprint counts are plotted as both step curves and interactive points hyperlinking to preprints upon click. Colour code denotes preprint server categorisation.

Only [bioRxiv](https://biorxiv.org) via the [Rxivist API](https://rxivist.org/) and [arXiv](https://arxiv.org) via the [arXiv API](https://arxiv.org/help/api) are fully supported. Preprint data for [medRxiv](https://medrxiv.org) are extracted using via in-development [medrxivr](https://github.com/mcguinlu/medrxivr) package. 

Further COVID-19 research is available from other preprint repositories without currently available APIs, e.g. [preprints.org](https://www.preprints.org), or non-repository platforms, e.g., [virological.org](https://virological.org). Data are subject to latest availability of metadata through APIs.

### Interactive plot demos (using plotly)

[medRxiv](https://lbrierley.github.io/cov_preprints/medrxiv_cov_preprints.html) 

[bioRxiv](https://lbrierley.github.io/cov_preprints/biorxiv_cov_preprints.html)

[arXiv](https://lbrierley.github.io/cov_preprints/arxiv_cov_preprints.html)


