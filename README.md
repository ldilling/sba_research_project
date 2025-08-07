View the Research Results and Basic ML Guide Here

- [SBA Machine Learning Guide](https://ldilling.github.io/sba_research_project/sba_ml_guide.html)
- [SBA Research Results](https://ldilling.github.io/sba_research_project/sba_research_results.html)

The majority of data cleaning, analysis, and visualization comes from the sba_analysis.R file. This code is annotated at each step, and the loan-Level 7(a) and PPP Loan Data can be found on the [SBA Open Data Page]([url](https://data.sba.gov/en/dataset/)), and the 7(a) loan data is updated quarterly. The federal funds rate data are from the [Federal Reserve Economic Data (FRED) site]([url](https://fred.stlouisfed.org/series/FEDFUNDS)). The institution size variables in the SBA were added manually based on institutional assets. The fintech category is taken from the SBA's list of Authorized FinTech Lenders.

| Institution Type | Classification |
| ------------- |  -------------|
| National | Assets > $100 Billion |
| Regional | $100 Billion > Assets > $10 Billion |
| Community | Assets < $10 Billion |
| Fintech | SBA Fintech List |
| Other | Non-Fintech and Not FDIC Insured |


- For a Copy of the SBA 7(a) data with financial institution size variables, contact me at: ldilling@umich.edu
