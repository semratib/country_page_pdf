## About
This code creates country profile reports for 194 WHO Member States. The report reflects the data presented in the country profiles on the WHO DataDot platform (ie. https://data.who.int/countries/004).

## Code
1. "prep datasets.R" assembles all the data needed to run the reports (creates data_req.rda)
2. "run reports.R" produces all the reports - there are two versions of the report, based on data availability (report.Rmd & report_version_smallpop.Rmd)

## Data sources
- Global Health Estimates, cause of death: https://extranet.who.int/xmart4/DEX_CMS/data/GHE_FULL
- Life & healthy life expectancy: https://www.who.int/data/gho/data/themes/mortality-and-global-health-estimates/ghe-life-expectancy-and-healthy-life-expectancy
- Population data: https://population.un.org/wpp/downloads?folder=Standard%20Projections&group=Population
- Triple Billion: https://data.who.int/dashboards/global-progress/triple-billion?n=o
