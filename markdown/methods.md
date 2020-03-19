---
title: "COVID-19 Analysis Report"
date: "13 March, 2020"
output:
  html_document:
    keep_md: yes
    df_print: paged
    number_sections: yes
    toc: yes
    toc_depth: 2
    toc_float:
      collapsed: no
  pdf_document:
    toc: yes
    toc_depth: '2'
---

# Methods

Patient details were submitted electronically by participating sites.  Relevant background and presenting symptoms were recorded on the day of study recruitment.  Daily follow-up was then completed until recovery or death.  A final form was completed with details of treatments received and outcomes. All categories that represent fewer than five individuals have been suppressed to avoid the potential for identification of participants.

We fitted Gamma distributions to the observed data on admission to discharge, admission to ICU, NIMV duration, and ICU duration, allowing for observations for which final outcomes have not yet been observed by the time of the analysis.

A statistical methodology  based on the relative risk ratio for pre-peak to post-peak incidence can be applied in analyzing the roles played by mutually exclusive population subgroups in the propagation of an epidemic [1].  The  key  principle  underlying  the  method  is  the  comparison  of  relative  risks  for  pre-  versus  post-peak periods for each population subgroup of interest. This is particularly relevant as a type of retrospective analysis, i.e. at the end of the epidemic, to inform strategies for future intervention mechanisms.




