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
date: 01 January 2024
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




Burr, V., King, N., & Heckmann, M. (2020). The qualitative analysis of repertory grid data: Interpretive Clustering. Qualitative Research in Psychology. https://doi.org/10.1080/14780887.2020.1794088

# Introduction

The [OpenRepGrid R package](https://cran.r-project.org/web/packages/OpenRepGrid/index.html) is a software to analyze and visualize [repertory grid](https://en.wikipedia.org/wiki/Repertory_grid) (often abbreviated *grid* or *repgrid*) data. The software is open source and available on all major operating systems. The package is also the workhorse on which other packages of the [OpenRepGrid project](http://openrepgrid.org/), for example, [gridsampler](https://openresearchsoftware.metajnl.com/articles/10.5334/jors.150/) [@heckmann_gridsampler_2017] or [OpenRepGrid.ic](https://joss.theoj.org/papers/10.21105/joss.03292) [@heckmann_openrepgridic_2023], partially build upon. 


# Repertory Grid Technique 

The repertory grid technique (RGT) is a data collection method which originated from *Personal Construct Theory (PCT)* [@kelly_psychology_1955]. It was originally designed as an instrument for psychotherapy to shed light on a client’s construction of the world. Over subsequent decades, the technique has been adopted in many other fields, including market, organizational, political, educational and sensory research [@fransella_manual_2004]. The data the RGT generates is *qualitative* and *quantitative*. On the qualitative side, the technique elicits the repertory of bipolar attributes (e.g. *smart vs. dull*, so called *constructs* in PCT terminology) an individual uses to make distinctions between entities of the world (e.g. different people, so called *elements* in PCT terminolgy). On the quatitative side, it requires rating each element on each elicited personal construct (e.g. *Martin* gets a score of 2 on the *quarrelsome = 1 vs. peaceful = 6* construct, indicating that Martin is quite quarrelsome). The result of the data collection procedure is a data matrix. The constructs are usually presented as matrix rows, the elements as columns and each cell contains the corresponding rating score. Figure 1 depicts a repertory grid data set, with the rows (constructs) and columns (elements) being clustered by similarity (see below for details). A thorough introduction to the repertory grid technique is given by @fransella_manual_2004.

![**Figure 1.** Example of a repertory grid dataset (with rows and columns clustered by similarity).](img/01-bertin-clustered.png)


# Available Software

While it is possible to work with repertory grids directly without further processing, it is common to submit grid data to statistical or mathematical analysis [e.g. @fransella_manual_2004]. For this purpose, software packages have been developed since the 1960s [@sewell_computerized_1992]. Today, several softwares are available on the market, e.g. Enquire Within [@mayes_enquire_2008], GridStat [@bell_gridstat_2009], GridCor [@feixas_gridcor:_2002], Idiogrid [@grice_idiogrid:_2002], Rep 5 [@gaines_rep_2009], GridSuite [@fromm_gridsuite_2006], rep:grid [@rosenberger_vademecum_2015]. Despite the numerous software packages being available, several issues are common among them: 

* No grid software offers all methods of grid analysis that have been devised in the literature. 
* None of the available grid programs can be extended by the user, i.e., the user cannot add or modify features. All listed softwares are closed source or at least not available in a public repository.
* There is no computational framework integrated into the available grid programs to support experimental types of analysis.
* The output of most grid analysis programs does not easily lend itself to subsequent computation.
* There is no joint community effort to improve a grid program: The development and documentation is delegated to the software providers, while users or researcher do usually not participate in this process.
* A lack of community participation in the software development and its closed source nature leads to the problem of discontinued development once its initiators have moved on or retired.


# Statement of Need

The OpenRepGrid project was started with the idea of overcoming above mentioned issues. It was designed as an open source project allowing other researchers to contribute, for example, by implementing new features. R was chosen as the programming language as it runs on all major operating systems, gets increasingly popular among academics and is nowadays already taught to undergrads at many universities. The open source nature of R makes it transparent how functions (i.e. methods of grid analysis) are implemented. Also, R and most contributed packages are distributed under a copyleft license. This allows reseachers to use or modify existing code for their own needs and redistribute the code under the same license. In total, the obstancles to experimenting and contributing are significantly lowered compared to other softwares on the market. 

The open source and collaborative stance of the project may bear another important benefit in terms of scientific progress. Currently, there appears to exist a substantial latency between publication of new grid analysis methods and them being made available to researchers as software features. For example, the *structural quadrant method* (SQM), a method to assess construct system complexity, devised almost 20 years ago by @gallifa_structural_2000, may serve as an example. The SQM has not been implemented in any grid program, hindering research and discussion of the method. The OpenRepGrid project may help to improve this situation. If researchers decide to build their new method in R from the beginning on, adding their method to the OpenRepGrid package will only be a small additional step. This will facilitate the dissemination of new methods in the research community, leading to a reduction in time-to-market for new methodological ideas. Once the method's code has been tested and documented, it can immediately become part of the OpenRepGrid package and instantly be used by all researchers using grids.

Another reason for the choice of R is its growing ability to easily build graphical user interfaces (GUI) using, for example, the *shiny* [@chang_shiny:_2019] and other related R packages. The PCP community is on average not well-versed in programming. This translates into the need for easy to operate, GUI-based software. As shiny does not require knowledge of other web languages (i.e. CSS, HTML, JavaScript) to build a fully operational web application, R is also a suitable choice to fullfill this community need.


# Features

An up-to-date overview of all features implemented in the OpenRepGrid package can be found on the project’s documentation site (http://docs.openrepgrid.org.) and in the R package’s documentation files, accessible via [R Help](https://www.r-project.org/help.html). The implemented features include the following:

* *Data handling*: Importing and exporting grid data from different formats, sorting grids, several included datasets (e.g. the *boeker* dataset, see below)
* *Analyzing constructs*: Descriptive statistics, correlations, distances, PCA of construct correlations, cluster analysis, aligning constructs
* *Analyzing elements*: Descriptive statistics, correlations, distances, standardized element distances, cluster analysis
* *Visualization*: (Clustered) Bertin plots (i.e. heatmaps), biplots, clustering dendrograms
* *Indexes*: Intensity, complexity, PVAFF, measures of cognitive conflict, implicative dilemmas, etc.

In the remainder, three repgrid visualizations which are frequently used in publications and two types of statistical grid analyses are briefly outlined as feature examples. Figure 1 shows a Bertin diagram (i.e. heatmap) of a grid administered to a schizophrenic patient undergoing psychoanalytically oriented psychotherapy [@boker_reconstruction_1996]. The data was taken during the last stage of therapy. The data for this example is already included in the package. The ratings in the grid are color-coded allowing to spot similar rating patterns. Also, the grid was submitted to hierarchical cluster analysis, thereby reordering the constructs and elements by similarity as indicated by the dendrograms printed alongside the diagram. The following code creates the diagram shown in Figure 1.


```r
bertinCluster(boeker, colors = c("white", "darkred"))
```


Figure 2 shows a biplot of the grid data from Figure 1. A biplot is the generalization of a scatterplot from two to many axes, all displayed in a single plot. It allows reading off the approximate score of each element on each construct by projecting an element's position in the plot on the construct axes [@greenacre_biplots_2010;@slater_measurement_1977]. In the biplot, it can, for example, be seen that the "father" is the element construed most closely to the "ideal self". Biplots of grid data are generally useful to generate transparency of the individual's overall construction of the elements and their similarity. Figure 2 is created by the following code. 


```r
biplot2d(boeker)
```

![**Figure 2.** Biplot of Böker's dataset.](img/02-biplot.png)

Figure 3 shows the dendrogram for the elements, here the result of a hierarchical cluster analysis using Ward's method with a Euclidean distances measure. Using an approach suggested by @heckmann_new_2016, the dendrogram structures are also tested for stability. Stable or significant structures are framed in a rectangle, indicating that "childhood self", "self before illness", "self with delusion", and "self as dreamer" forms a stable group of elements. Figure 3 is created by the following code. 


```r
s <- clusterBoot(boeker, along = 2, seed = 123)
plot(s)
pvrect(s, max.only = FALSE)
```

![**Figure 3.** Dendrogram of clustering results.](img/03-clusterboot.png)

Inter-element distances are a commonly applied measure in the statistical analysis of grid data [@fransella_manual_2004]. As already shown in the biplot example above, distances between elements indicate which elements (i.e. persons) are construed as similar. One distance of particular intererest in psychotherapy research is the self-ideal distance as it may provide useful clinical indications [e.g. @taylor_investigating_2020]. But also in other areas, for example, in market research element distances are frequently used in the analysis [e.g. @hauser_measuring_2011]. In most cases, the Euclidean distance is selected as a distance measure. As the maximal Euclidean distances between two elements depends on the rating scale and the number of constructs in a grid, several approaches to standardizing inter-element distances have been suggested. One well known approach which has come to be known as *Slater distances*, divides the inter-element distance by its expected value [@slater_measurement_1977]. However, @hartmann_element_1992 showed in a simulation study that Slater distances have a skewed distribution, as well as a mean and a standard deviation depending on the number of elicited constructs. Hartmann suggested an improvement measure by applying a transformation to standardize Slater distances across different grid sizes. This development serves as another example of above mentioned situation, as to the best of my knowledge, Hartmann distances are currently only implemented in OpenRepGrid and no other grid software. Hartmann distances can be calculated using the following code. 


```r
distanceHartmann(boeker)
```

```

##########################
Distances between elements
##########################

Distance method:  Hartmann (standardized Slater distances)
Normalized:
                                1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
(1) self                  1       -0.28  1.58  1.92  0.80 -1.33  1.20 -0.29 -0.04  2.62 -5.24  2.66  2.87  2.28  2.89
(2) ideal self            2             -0.78  1.36 -0.47 -2.09 -0.56  0.12 -1.02  0.12 -3.69 -1.50 -1.45 -1.63 -1.71
(3) mother                3                    1.70  2.99  0.22  2.82  1.15  2.27  2.09 -3.84  1.91  1.06  1.44  1.92
(4) father                4                          2.31 -1.04  2.23  0.55  1.00  1.92 -4.39  0.96  0.50  0.08  0.63
(5) kurt                  5                                0.63  2.72  1.27  2.69  1.74 -3.37  1.30  0.35  0.79  1.01
(6) karl                  6                                      0.29  1.63  2.14 -0.66  0.10 -1.21 -1.53 -0.60 -1.04
(7) george                7                                            0.45  2.19  1.17 -3.39  1.70  0.54  0.42  1.35
(8) martin                8                                                  2.03  1.22 -1.85 -0.67 -0.73 -0.13 -0.53
(9) elizabeth             9                                                        0.76 -2.07 -0.08 -0.91 -0.29  0.05
(10) therapist           10                                                             -4.91  2.20  2.35  1.97  2.22
(11) irene               11                                                                   -5.47 -5.65 -4.79 -5.52
(12) childhood self      12                                                                          3.66  3.16  4.22
(13) self before illness 13                                                                                3.60  3.79
(14) self with delusion  14                                                                                      3.52
(15) self as dreamer     15                                                                                          

For calculation the parameters from Hartmann (1992) were used. Use 'method=new' or method='simulate' for a more accurate version.
```

The last feature example concers the detection of implicative dilemmas. Implicative dilemmas represent a form of cognitive conflict. An implicative dilemma arises when a desired change on one construct is associated with an undesired change on another construct. For example, a *timid* person may wish to become more *socially skilled* but associates being more socially skilled with several negative characteristics (selfish, insensitive etc.). The person might, for example, construe the implication of becoming less timid (desired) as becoming more selfish (undesired) at the same time [@winter_construct_1982]. As a consequence, the person may resist to the desired change if the presumed implications will threaten the person's identity and the predictive power of his construct system. The investigation of the role of implicative dilemmas in different mental disorders is an active field of research in Personal Construct Psychology [e.g. @feixas_multi-center_2004; @dorough_implicative_2007; @rouco_measurement_2019]. Implicative dilemma can be detected using the `indexDilemma` function. For the dataset above, the results show that a desired change on the discrepant contruct *balanced - get along with conflicts*  towards the *get along with conflicts* pole implies four undesired changes, for example, to become more *indifferent* and less *peaceful*. 


```r
id <- indexDilemma(boeker, self = 1, ideal = 2)
id
```

```

####################
Implicative Dilemmas
####################

-------------------------------------------------------------------------------

SUMMARY:

No. of Implicative Dilemmas (IDs): 4
No. of possible construct pairs: 91
Percentage of IDs (PID): 4.4% (4/91)
Intensity of IDs (IID): 61.3
Proportion of the intensity of constructs of IDs (PICID): 2.7

-------------------------------------------------------------------------------

PARAMETERS:

Self: Element No. 1 = self
Ideal: Element No. 2 = ideal self

Correlation Criterion: >= 0.35
Note: Correlation calculated including elements Self & Ideal

Criteria (for construct classification):
Discrepant if Self-Ideal difference: >= 3
Congruent if Self-Ideal difference: <= 1

-------------------------------------------------------------------------------

CLASSIFICATION OF CONSTRUCTS:

   Note: Constructs aligned so 'Self' corresponds to left pole

                             Construct Self Ideal Difference Classification
1  balanced - get along with conflicts    1     4          3     discrepant
2                  isolated - sociable    3     6          3     discrepant
3        closely integrated - excluded    2     2          0      congruent
4                 passive - discursive    3     6          3     discrepant
5            open minded - indifferent    2     1          1      congruent
6               dispassionate - dreamy    3     2          1      congruent
7     practically oriented - depressed    2     1          1      congruent
8                    serious - playful    3     2          1      congruent
9            socially minded - selfish    2     1          1      congruent
10              peaceful - quarrelsome    2     2          0      congruent
11                technical - artistic    2     6          4     discrepant
12              scientific - emotional    2     1          1      congruent
13               extrovert - introvert    3     2          1      congruent
14          wanderlust - home oriented    1     1          0      congruent

-------------------------------------------------------------------------------

IMPLICATIVE DILEMMAS:

   Note: Congruent constructs on the left - Discrepant constructs on the right

                       Congruent                             Discrepant    R            RexSI
1   5. open minded - indifferent 1. balanced - get along with conflicts 0.53             0.63
2   9. socially minded - selfish 1. balanced - get along with conflicts 0.36             0.43
3     10. peaceful - quarrelsome 1. balanced - get along with conflicts 0.84 *Not implemented
4 14. wanderlust - home oriented 1. balanced - get along with conflicts 0.72             0.79

	R = Correlation including Self & Ideal
	RexSI = Correlation excluding Self & Ideal
	R was used as criterion
```

The implied dilemmas can also be visualized as a network graph.


```r
plot(id)
```

![**Figure 4.** Network graph of implicative dilemmas.](img/04-implicative-dilemmas.png)


# Contributing

In order to maximize the package's usefulness for the grid research community, we welcome participation in the package's further development. Experienced R programmers are asked to make pull requests to the [OpenRepGrid github repository](https://github.com/markheckmann/OpenRepGrid), [report issues](https://github.com/markheckmann/OpenRepGrid/issues), or commit code snippets by email. Non-technical oriented researchers without programming knowledge are invited to send us feature requests or suggestions for collaboration, for example, to jointly to develop and implement a new repgrid analysis method. The goal is to make OpeneRepGrid useful for the majority of the repgrid community which will only be possible via research community participation.


# Acknowledgements

A lot of thanks to the contributors and supporters of this package: Richard C. Bell, Alejandro García, Diego Vitali, José Antonio González Del Puerto, and Jonathan D. Raskin.

# References


