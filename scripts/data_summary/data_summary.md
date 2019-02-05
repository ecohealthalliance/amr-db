exploratory\_data
================
emmamendelsohn
Tue Feb 5 14:44:57 2019

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

<th style="text-align:right;">

\<NA\>

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

299

</td>

<td style="text-align:right;">

9

</td>

</tr>

</tbody>

</table>

Count by name and
parent

| bacteria\_rank | bacteria\_preferred\_label | bacteria\_preferred\_label\_abbr | bacteria\_parent\_rank | bacteria\_parent\_name                        |  n | percent |
| :------------- | :------------------------- | :------------------------------- | :--------------------- | :-------------------------------------------- | -: | ------: |
| species        | klebsiella pneumoniae      | k. pneumoniae                    | genus                  | klebsiella                                    | 50 |    15.9 |
| species        | escherichia coli           | e. coli                          | genus                  | escherichia                                   | 27 |     8.6 |
| species        | staphylococcus aureus      | s. aureus                        | genus                  | staphylococcus                                | 23 |     7.3 |
| species        | acinetobacter baumannii    | a. baumannii                     | species group          | acinetobacter calcoaceticus/baumannii complex | 14 |     4.5 |
| species        | pseudomonas aeruginosa     | p. aeruginosa                    | species group          | pseudomonas aeruginosa group                  | 14 |     4.5 |

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

<th style="text-align:right;">

\<NA\>

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

608

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

1397

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

144

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

drug group

</td>

<td style="text-align:left;">

ciprofloxacin

</td>

<td style="text-align:left;">

fluoroquinolones

</td>

<td style="text-align:right;">

99

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

ceftazidime

</td>

<td style="text-align:left;">

cephaloridine

</td>

<td style="text-align:right;">

97

</td>

<td style="text-align:right;">

4.5

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

93

</td>

<td style="text-align:right;">

4.3

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

92

</td>

<td style="text-align:right;">

4.3

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

84

</td>

<td style="text-align:right;">

3.9

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

176

</td>

<td style="text-align:right;">

8.2

</td>

</tr>

<tr>

<td style="text-align:left;">

cefotaxime

</td>

<td style="text-align:right;">

139

</td>

<td style="text-align:right;">

6.4

</td>

</tr>

<tr>

<td style="text-align:left;">

cephalosporins

</td>

<td style="text-align:right;">

112

</td>

<td style="text-align:right;">

5.2

</td>

</tr>

<tr>

<td style="text-align:left;">

ampicillin

</td>

<td style="text-align:right;">

107

</td>

<td style="text-align:right;">

5.0

</td>

</tr>

<tr>

<td style="text-align:left;">

ciprofloxacin

</td>

<td style="text-align:right;">

99

</td>

<td style="text-align:right;">

4.6

</td>

</tr>

</tbody>

</table>

![](data_summary_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

—————–Bacteria + Drugs Paired—————–

Count by linkages (12 most common linkages)

| drug                                      | bacteria      |  n |
| :---------------------------------------- | :------------ | -: |
| ceftazidime                               | k. pneumoniae | 22 |
| imipenem                                  | k. pneumoniae | 20 |
| meropenem                                 | k. pneumoniae | 19 |
| ciprofloxacin                             | k. pneumoniae | 17 |
| imipenem                                  | e. coli       | 17 |
| piperacillin, tazobactam drug combination | k. pneumoniae | 16 |
| meropenem                                 | e. coli       | 16 |
| ertapenem                                 | k. pneumoniae | 16 |
| cefepime                                  | k. pneumoniae | 16 |
| cefotaxime                                | k. pneumoniae | 15 |

![](data_summary_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->![](data_summary_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

Count by pub
date

![](data_summary_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->![](data_summary_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->![](data_summary_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

Count by exclusion criteria

![](data_summary_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
