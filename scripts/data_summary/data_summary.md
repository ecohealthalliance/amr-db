exploratory\_data
================
emmamendelsohn
2019-04-23

—————–Locations—————–

Study Locations

![](data_summary_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

Travel Locations

![](data_summary_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Residence Locations

![](data_summary_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

—————–Bacteria—————–

Count by rank

<table>

<thead>

<tr>

<th style="text-align:right;">

family

</th>

<th style="text-align:right;">

genus

</th>

<th style="text-align:right;">

species

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

263

</td>

</tr>

</tbody>

</table>

Count by name and
parent

| bacteria\_rank | bacteria\_preferred\_label | bacteria\_preferred\_label\_abbr | bacteria\_parent\_rank | bacteria\_parent\_name                        |  n | percent |
| :------------- | :------------------------- | :------------------------------- | :--------------------- | :-------------------------------------------- | -: | ------: |
| species        | klebsiella pneumoniae      | k. pneumoniae                    | genus                  | klebsiella                                    | 43 |    16.0 |
| species        | escherichia coli           | e. coli                          | genus                  | escherichia                                   | 23 |     8.6 |
| species        | staphylococcus aureus      | s. aureus                        | genus                  | staphylococcus                                | 21 |     7.8 |
| species        | acinetobacter baumannii    | a. baumannii                     | species group          | acinetobacter calcoaceticus/baumannii complex | 10 |     3.7 |
| species        | pseudomonas aeruginosa     | p. aeruginosa                    | species group          | pseudomonas aeruginosa group                  | 10 |     3.7 |

![](data_summary_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

—————–Drugs—————–

Count by rank

<table>

<thead>

<tr>

<th style="text-align:right;">

drug group

</th>

<th style="text-align:right;">

drug group + drug group

</th>

<th style="text-align:right;">

drug group + drug name

</th>

<th style="text-align:right;">

drug name

</th>

<th style="text-align:right;">

drug name + drug name

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

544

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

1254

</td>

<td style="text-align:right;">

4

</td>

</tr>

</tbody>

</table>

Count by name and parent

<table>

<thead>

<tr>

<th style="text-align:left;">

drug\_rank

</th>

<th style="text-align:left;">

drug\_preferred\_label\_abbr

</th>

<th style="text-align:left;">

drug\_parent\_name\_abbr

</th>

<th style="text-align:right;">

n

</th>

<th style="text-align:right;">

percent

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

drug name

</td>

<td style="text-align:left;">

ceftazidime

</td>

<td style="text-align:left;">

cephaloridine

</td>

<td style="text-align:right;">

92

</td>

<td style="text-align:right;">

5.1

</td>

</tr>

<tr>

<td style="text-align:left;">

drug group

</td>

<td style="text-align:left;">

ciprofloxacin

</td>

<td style="text-align:left;">

fluoroquinolones

</td>

<td style="text-align:right;">

87

</td>

<td style="text-align:right;">

4.8

</td>

</tr>

<tr>

<td style="text-align:left;">

drug name

</td>

<td style="text-align:left;">

imipenem

</td>

<td style="text-align:left;">

thienamycins

</td>

<td style="text-align:right;">

86

</td>

<td style="text-align:right;">

4.8

</td>

</tr>

<tr>

<td style="text-align:left;">

drug group

</td>

<td style="text-align:left;">

gentamicins

</td>

<td style="text-align:left;">

aminoglycosides

</td>

<td style="text-align:right;">

83

</td>

<td style="text-align:right;">

4.6

</td>

</tr>

<tr>

<td style="text-align:left;">

drug name

</td>

<td style="text-align:left;">

meropenem

</td>

<td style="text-align:left;">

thienamycins

</td>

<td style="text-align:right;">

79

</td>

<td style="text-align:right;">

4.4

</td>

</tr>

</tbody>

</table>

![](data_summary_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Count by group only

<table>

<thead>

<tr>

<th style="text-align:left;">

drug\_group

</th>

<th style="text-align:right;">

n

</th>

<th style="text-align:right;">

percent

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

thienamycins

</td>

<td style="text-align:right;">

165

</td>

<td style="text-align:right;">

9.1

</td>

</tr>

<tr>

<td style="text-align:left;">

cefotaxime

</td>

<td style="text-align:right;">

124

</td>

<td style="text-align:right;">

6.9

</td>

</tr>

<tr>

<td style="text-align:left;">

ampicillin

</td>

<td style="text-align:right;">

95

</td>

<td style="text-align:right;">

5.3

</td>

</tr>

<tr>

<td style="text-align:left;">

cephaloridine

</td>

<td style="text-align:right;">

93

</td>

<td style="text-align:right;">

5.1

</td>

</tr>

<tr>

<td style="text-align:left;">

cephalosporins

</td>

<td style="text-align:right;">

92

</td>

<td style="text-align:right;">

5.1

</td>

</tr>

</tbody>

</table>

![](data_summary_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

—————–Bacteria + Drugs Paired—————–

![](data_summary_files/figure-gfm/r%20bacteria_drugs-1.png)<!-- -->

Count by linkages (12 most common linkages)

| drug                                      | bacteria      |  n |
| :---------------------------------------- | :------------ | -: |
| ceftazidime                               | k. pneumoniae | 23 |
| meropenem                                 | k. pneumoniae | 22 |
| imipenem                                  | k. pneumoniae | 21 |
| ertapenem                                 | k. pneumoniae | 18 |
| piperacillin, tazobactam drug combination | k. pneumoniae | 16 |
| cefotaxime                                | k. pneumoniae | 16 |
| cefepime                                  | k. pneumoniae | 16 |
| imipenem                                  | e. coli       | 16 |
| ciprofloxacin                             | k. pneumoniae | 16 |
| meropenem                                 | e. coli       | 15 |

![](data_summary_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Count by pub
date

![](data_summary_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->![](data_summary_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->![](data_summary_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

Count by exclusion criteria

![](data_summary_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
