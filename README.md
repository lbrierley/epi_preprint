### Preprint uploads during the COVID-19 (SARS-CoV-2) pandemic

`preprint_full_analyses_6_1_21.R` conducts queries of preprint servers and extracts metadata using respective APIs for two purposes: plot the rise in preprints addressing the current pandemic over time as interactive visualisations, and quantifying rates of preprint posting for previous epidemics and Google search trends of interest (i.e. code supporting the article 'Lessons from the influx of preprints during the early COVID-19 pandemic').

Searches are conducted for COVID-19 using keywords `"coronavirus", "coronaviruses", "ncov", "SARS-CoV-2", "COVID-19"`. Preprint counts are plotted as both step curves and interactive points hyperlinking to preprints upon click. Colour code denotes preprint server categorisation.

Only [bioRxiv](https://biorxiv.org) via the [Rxivist API](https://rxivist.org/) and [arXiv](https://arxiv.org) via the [arXiv API](https://arxiv.org/help/api) are fully supported. Preprint data for [medRxiv](https://medrxiv.org) are extracted using via in-development [medrxivr](https://github.com/mcguinlu/medrxivr) package. 

Further COVID-19 research is available from other preprint repositories without currently available APIs, e.g. [preprints.org](https://www.preprints.org), or non-repository platforms, e.g., [virological.org](https://virological.org). Data are subject to latest availability of metadata through APIs.

Further COVID-19 bibliometrics and trends in preprints are available in [Fraser et al. 2020](https://www.biorxiv.org/content/10.1101/2020.05.22.111294v2).

### Interactive plot demos (using plotly)

[medRxiv](https://lbrierley.github.io/cov_preprints/medrxiv_cov_preprints.html) 

[bioRxiv](https://lbrierley.github.io/cov_preprints/biorxiv_cov_preprints.html)

[arXiv](https://lbrierley.github.io/cov_preprints/arxiv_cov_preprints.html)


