---
title: 'OpenRepGrid: An R Package for the Analysis of Repertory Grid Data'
tags:
- Repertory grid technique
- Personal Construct Psychology
- R package
authors:
- name: Mark Heckmann
  orcid: 0000-0002-0736-7417
  affiliation: 1
affiliations:
- name: DB InfraGO AG, Germany
  index: 1
date: 20 July 2024
bibliography: paper.bib
output:
  html_document:
    keep_md: yes
  word_document: default
---

<style type="text/css">
code.r{
  font-size: 12px;
}
pre {
  font-size: 12px
}
</style>




# Introduction

[OpenRepGrid](https://cran.r-project.org/web/packages/OpenRepGrid/index.html) is an R package designed to analyze [repertory grid](https://en.wikipedia.org/wiki/Repertory_grid) (also referred to as *grid* or *repgrid*) data. The software is open-source and available on all major operating systems. The package is also the workhorse on which other packages of the [OpenRepGrid project](http://openrepgrid.org/), for example, [gridsampler](https://openresearchsoftware.metajnl.com/articles/10.5334/jors.150/) [@heckmann_gridsampler_2017] or [OpenRepGrid.ic](https://joss.theoj.org/papers/10.21105/joss.03292) [@heckmann_openrepgridic_2023], partially rely. 


# Repertory Grid Technique 

The repertory grid technique (RGT) is a data collection method that originated from *Personal Construct Theory (PCT)* [@kelly_psychology_1955]. It was originally designed as an instrument for psychotherapy to shed light on a client’s construction of the world. Over subsequent decades, the technique has been adopted in many other fields, including market, organizational, political, educational or sensory research [@fransella_manual_2004]. The data the RGT generates is *qualitative* and *quantitative*. On the qualitative side, the technique elicits the repertory of bipolar attributes (e.g. *smart vs. dull*, so-called *constructs* in PCT terminology) that an individual uses to make distinctions between entities of the world (e.g. different people, referred to as *elements* in PCT terminolgy). On the quantitative side, it requires rating each element on each elicited personal construct (e.g. *Martin* has a score of 2 on the *quarrelsome = 1 vs. peaceful = 6* construct, indicating that Martin is rather quarrelsome). The result of the data collection procedure is a matrix. The constructs are presented as rows, the elements as columns, and each cell contains the corresponding rating score. Figure 1 depicts a grid data set, with the rows (constructs) and columns (elements)  clustered by similarity (see below for details). A thorough introduction to the repertory grid technique is given by @fransella_manual_2004.

![**Figure 1.** Example of a repertory grid dataset (with rows and columns clustered by similarity).](img/01-bertin-clustered.png)


# Available Software

While it is possible to work with repertory grids directly without further processing, it is common to submit grid data to statistical analysis [e.g. @fransella_manual_2004]. For this purpose, software packages have been developed since the 1960s [@sewell_computerized_1992], including Enquire Within [@mayes_enquire_2008], GridStat [@bell_gridstat_2009], GridCor [@feixas_gridcor:_2002], Idiogrid [@grice_idiogrid:_2002], Rep 5 [@gaines_rep_2009], GridSuite [@fromm_gridsuite_2006], rep:grid [@rosenberger_vademecum_2015]. Despite the numerous software packages available, several issues are common among them: 

* All listed softwares programs are closed source or at least not available in a public repository.
* No software offers all analysis methods devised in the literature. 
* None of the programs can be extended by the user, for example, by adding or modifying features. 
* The output of most grid programs does not easily lend itself to subsequent computation.
* There is no joint community effort to improve a grid program. Users do not typically participate in this process.
* A lack of community participation and its closed-source nature often lead to the problem of discontinued development once their creators move on or retire.


# Statement of Need

The OpenRepGrid project was started with the idea of overcoming the above-mentioned issues. It was designed as an open-source project allowing researchers to contribute, for example, by implementing new features. R was chosen as a programming language because it runs on all major operating systems, has become increasingly popular among academics and is nowadays taught to undergraduates at many universities. Its open-source nature makes it transparent how analysis methods are implemented. Also, most R packages are distributed under a permissive license, allowing researchers to adapt code to their own needs and redistribute it. In total, OpenRepGrid makes experimenting and contributing significantly easier compared to other software solutions on the market. 

The open-source nature of the project may also facilitate scientific progress. Currently, there may exist a substantial latency between the publication of analysis methods and their implementation in software tools. For example, the *structural quadrant method* [SQM, @gallifa_structural_2000], devised over 20 years ago as an approach to assess construct system complexity, serves as an example. To date, the SQM has not been implemented in any grid program, hindering discussion of the method. OpenRepGrid helps to improve this situation. If researchers decide to build a new method in R, adding the method to the package is only a small step. This  facilitates the dissemination of new methods in the research community, leading to a reduction in time-to-market for new methodological ideas.


# Features

An overview of all implemented features can be found on the project’s website (http://docs.openrepgrid.org.) and in the R package’s documentation files, accessible via [R Help](https://www.r-project.org/help.html). The implemented features include the following:

* *Data handling*: Importing and exporting grid data from different formats, sorting grids, several included datasets (e.g. the `boeker` dataset, see below)
* *Analyzing constructs*: Descriptive statistics, correlations, distances, PCA of construct correlations, cluster analysis, aligning constructs
* *Analyzing elements*: Descriptive statistics, correlations, distances, standardized element distances, cluster analysis
* *Visualization*: (Clustered) Bertin plots (i.e. heatmaps), biplots, clustering dendrograms
* *Indexes*: Intensity, complexity, PVAFF, measures of cognitive conflict, implicative dilemmas, etc.

In the remainder, some analysis features frequently used in publications are outlined as examples. Figure 1 shows a *Bertin diagram* (i.e. heatmap) of a grid administered to a patient undergoing psychotherapy [@boker_reconstruction_1996]. The dataset is included in the package. The ratings in the grid are color-coded to allow spotting similar rating patterns. Also, the grid was submitted to hierarchical cluster analysis, thereby reordering the constructs and elements by similarity as indicated by the dendrograms printed alongside the diagram. The following code creates the diagram shown in Figure 1.


```r
bertinCluster(boeker, colors = c("white", "darkred"))
```


Figure 2 shows a *biplot* of the same grid. A biplot is the generalization of a scatterplot thats shows more than two axes in a single plot. It allows readers to determine the approximate score of each element on each construct by projecting an element's position on a construct axis [@greenacre_biplots_2010;@slater_measurement_1977]. It can, for example, be seen that the "father" is the element construed most closely to the "ideal self". Biplots are useful for gaining an understanding of the individual's overall construction of the elements and their similarity. Figure 2 is created by the following code. 


```r
biplot2d(boeker)
```

![**Figure 2.** Biplot of Böker's dataset.](img/02-biplot.png)

Figure 3 shows the dendrogram for the elements, which is the result of a hierarchical cluster analysis using Ward's method with a Euclidean distance measure. Using an approach suggested by @heckmann_new_2016, the dendrogram structures are also tested for stability. Stable or significant structures are framed in a rectangle, indicating that "childhood self", "self before illness", "self with delusion", and "self as dreamer" form a stable group of elements. Figure 3 is created by the following code: 


```r
s <- clusterBoot(boeker, along = 2, seed = 123)
plot(s)
pvrect(s, max.only = FALSE)
```

![**Figure 3.** Dendrogram of clustering results.](img/03-clusterboot.png)

The last example showcases the detection of *implicative dilemmas*. Implicative dilemmas represent a form of cognitive conflict. An implicative dilemma arises when a desired change on one construct is associated with an undesired change on another construct. For example, a *timid* person may wish to become more *socially skilled* but associates being more socially skilled with several negative characteristics (selfish, insensitive, etc.). The person might, for example, construe the implication of becoming less timid (desired) as also being more selfish (undesired) [@winter_construct_1982]. As a consequence, the person may resist the desired change if the presumed implications are perceived to threaten the person's identity. The investigation of the role of implicative dilemmas in different mental disorders is an active field of research in PCP [e.g. @feixas_multi-center_2004; @dorough_implicative_2007; @rouco_measurement_2019]. Implicative dilemmas can be detected using the `indexDilemma` function. For the dataset above, the results show that a desired change on the discrepant construct *balanced - get along with conflicts*  towards the *get along with conflicts* pole implies four undesired changes, such as becoming more *indifferent* and less *peaceful*.


```r
id <- indexDilemma(boeker, self = 1, ideal = 2)
plot(id)
```

![**Figure 4.** Network graph of implicative dilemmas.](img/04-implicative-dilemmas.png)


# Contributing

In order to maximize the package's usefulness for the grid community, we invite users to participate in its development. R programmers are asked to submit pull requests to the [OpenRepGrid github repository](https://github.com/markheckmann/OpenRepGrid) or commit code snippets. Users without programming knowledge may [report issues](https://github.com/markheckmann/OpenRepGrid/issues), send us feature requests, or offer suggestions for collaboration, for example, to jointly develop and implement a new repgrid analysis method.

# Acknowledgements

Many thanks to the contributors and supporters of this package: Richard C. Bell, Alejandro García, Diego Vitali, José Antonio González Del Puerto, and Jonathan D. Raskin.

# References


