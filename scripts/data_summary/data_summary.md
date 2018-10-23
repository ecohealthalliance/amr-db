exploratory\_data
================
emmamendelsohn
2018-10-23

-----------------Bacteria-----------------

Count by rank

``` r
bacteria %>%
  group_by(ncbi_rank) %>%
  count() %>% 
  spread(ncbi_rank, n) %>%
  kable() 
```

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
&lt;NA&gt;
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
467
</td>
<td style="text-align:right;">
2
</td>
</tr>
</tbody>
</table>
Count by name and parent

``` r
bacteria_sum <- bacteria %>%
  group_by(ncbi_rank, ncbi_preferred_label, ncbi_parent_rank, ncbi_parent_name) %>%
  count(sort = TRUE) %>%
  mutate(percent = round(100*n/nrow(bacteria), 1)) %>%
  ungroup()

kable(bacteria_sum %>% slice(1:10))
```

<table>
<thead>
<tr>
<th style="text-align:left;">
ncbi\_rank
</th>
<th style="text-align:left;">
ncbi\_preferred\_label
</th>
<th style="text-align:left;">
ncbi\_parent\_rank
</th>
<th style="text-align:left;">
ncbi\_parent\_name
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
species
</td>
<td style="text-align:left;">
klebsiella pneumoniae
</td>
<td style="text-align:left;">
genus
</td>
<td style="text-align:left;">
klebsiella
</td>
<td style="text-align:right;">
82
</td>
<td style="text-align:right;">
17.3
</td>
</tr>
<tr>
<td style="text-align:left;">
species
</td>
<td style="text-align:left;">
escherichia coli
</td>
<td style="text-align:left;">
genus
</td>
<td style="text-align:left;">
escherichia
</td>
<td style="text-align:right;">
58
</td>
<td style="text-align:right;">
12.2
</td>
</tr>
<tr>
<td style="text-align:left;">
species
</td>
<td style="text-align:left;">
staphylococcus aureus
</td>
<td style="text-align:left;">
genus
</td>
<td style="text-align:left;">
staphylococcus
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:right;">
8.0
</td>
</tr>
<tr>
<td style="text-align:left;">
species
</td>
<td style="text-align:left;">
pseudomonas aeruginosa
</td>
<td style="text-align:left;">
species group
</td>
<td style="text-align:left;">
pseudomonas aeruginosa group
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
4.6
</td>
</tr>
<tr>
<td style="text-align:left;">
species
</td>
<td style="text-align:left;">
neisseria gonorrhoeae
</td>
<td style="text-align:left;">
genus
</td>
<td style="text-align:left;">
neisseria
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
4.2
</td>
</tr>
<tr>
<td style="text-align:left;">
species
</td>
<td style="text-align:left;">
acinetobacter baumannii
</td>
<td style="text-align:left;">
species group
</td>
<td style="text-align:left;">
acinetobacter calcoaceticus/baumannii complex
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
4.0
</td>
</tr>
<tr>
<td style="text-align:left;">
species
</td>
<td style="text-align:left;">
salmonella enterica
</td>
<td style="text-align:left;">
genus
</td>
<td style="text-align:left;">
salmonella
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
3.8
</td>
</tr>
<tr>
<td style="text-align:left;">
species
</td>
<td style="text-align:left;">
citrobacter freundii
</td>
<td style="text-align:left;">
species group
</td>
<td style="text-align:left;">
citrobacter freundii complex
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
2.1
</td>
</tr>
<tr>
<td style="text-align:left;">
species
</td>
<td style="text-align:left;">
enterobacter cloacae
</td>
<td style="text-align:left;">
species group
</td>
<td style="text-align:left;">
enterobacter cloacae complex
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
2.1
</td>
</tr>
<tr>
<td style="text-align:left;">
species
</td>
<td style="text-align:left;">
enterococcus faecium
</td>
<td style="text-align:left;">
genus
</td>
<td style="text-align:left;">
enterococcus
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
2.1
</td>
</tr>
</tbody>
</table>
``` r
ggplot(bacteria_sum[bacteria_sum$n > 4,], aes(x = reorder(ncbi_preferred_label, -n), y = n)) +
  geom_bar(stat = "identity", fill = "green3") +
  labs(x = "Most common species", y = "Number of studies") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
```

![](data_summary_files/figure-markdown_github/unnamed-chunk-2-1.png)

-----------------Drugs-----------------

Count by rank

``` r
drugs %>%
  group_by(mesh_rank) %>%
  count() %>% 
  spread(mesh_rank, n) %>%
  kable() 
```

<table>
<thead>
<tr>
<th style="text-align:right;">
drug group
</th>
<th style="text-align:right;">
drug name
</th>
<th style="text-align:right;">
&lt;NA&gt;
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
572
</td>
<td style="text-align:right;">
1417
</td>
<td style="text-align:right;">
9
</td>
</tr>
</tbody>
</table>
count by name and parent (note that dups have not yet been handled 10/23/18)

``` r
drugs_sum <- drugs %>%
  group_by(mesh_rank, mesh_preferred_label, mesh_parent_name) %>%
  count(sort = TRUE) %>%
  mutate(percent = round(100*n/nrow(drugs), 1)) %>%
  ungroup()

kable(drugs_sum %>% slice(1:10)) 
```

<table>
<thead>
<tr>
<th style="text-align:left;">
mesh\_rank
</th>
<th style="text-align:left;">
mesh\_preferred\_label
</th>
<th style="text-align:left;">
mesh\_parent\_name
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
105
</td>
<td style="text-align:right;">
5.3
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
98
</td>
<td style="text-align:right;">
4.9
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
96
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
ciprofloxacin
</td>
<td style="text-align:left;">
fluoroquinolones
</td>
<td style="text-align:right;">
92
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
87
</td>
<td style="text-align:right;">
4.4
</td>
</tr>
<tr>
<td style="text-align:left;">
drug name
</td>
<td style="text-align:left;">
cefepime
</td>
<td style="text-align:left;">
cephalosporins
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:right;">
3.9
</td>
</tr>
<tr>
<td style="text-align:left;">
drug name
</td>
<td style="text-align:left;">
piperacillin, tazobactam drug combination
</td>
<td style="text-align:left;">
penicillanic acid
</td>
<td style="text-align:right;">
68
</td>
<td style="text-align:right;">
3.4
</td>
</tr>
<tr>
<td style="text-align:left;">
drug group
</td>
<td style="text-align:left;">
cefotaxime
</td>
<td style="text-align:left;">
cephacetrile
</td>
<td style="text-align:right;">
67
</td>
<td style="text-align:right;">
3.4
</td>
</tr>
<tr>
<td style="text-align:left;">
drug name
</td>
<td style="text-align:left;">
amikacin
</td>
<td style="text-align:left;">
kanamycin
</td>
<td style="text-align:right;">
67
</td>
<td style="text-align:right;">
3.4
</td>
</tr>
<tr>
<td style="text-align:left;">
drug group
</td>
<td style="text-align:left;">
ampicillin
</td>
<td style="text-align:left;">
penicillin g
</td>
<td style="text-align:right;">
66
</td>
<td style="text-align:right;">
3.3
</td>
</tr>
</tbody>
</table>
``` r
ggplot(drugs_sum[drugs_sum$percent > 1,], aes(x = reorder(mesh_preferred_label, -n), y = n, fill = mesh_rank)) +
  geom_bar(stat = "identity") +
  labs(x = "Most common drugs", y = "Number of studies") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
```

![](data_summary_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
# next: 
# drug + disease pairs 
```
