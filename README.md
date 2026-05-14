# SexEVEthOmics

## Small RNA-seq Mapping and Counting Pipeline

This repository contains the workflow used to process and analyze small RNA-seq data from the project:

- **Data Source**: [PRJNA1291119](https://www.ncbi.nlm.nih.gov/bioproject/PRJNA1291119)

---

### 📊 Workflow Overview
The pipeline performs:
1. Adapter trimming and quality filtering  
2. Mapping to the reference genome/index  
3. Counting **unique** mapped reads

#### 1. Adapter Trimming & Quality Filtering
Remove Illumina adapters, trim low-quality bases, and retain reads of 17–35 nt.

```bash
cutadapt -a TGGAATTCTCGGGTGCCAAGG -q 30 -m 17 -l 35 \
    -o sample_filtered.fastq.gz sample_raw.fastq.gz
```
Key parameters

`-a TGGAATTCTCGGGTGCCAAGG` → Illumina adapter sequence

`-q 30` → Trim bases with Phred quality < 30

`-m 17` → Minimum read length = 17 nt

`-l 35` → Maximum read length = 35 nt

#### 2. Mapping with Bowtie2

Align filtered reads to the reference index.
```bash
bowtie2 --no-unal -p 40 -L 6 -i S,0,0.5 \
    --ignore-quals --norc --score-min L,-1,-0.6 -D 20 \
    -x folder/reference_index \
    -U sample_filtered.fastq.gz \
    -S sample.sam 2> log.sample.txt
```
Key parameters

`--no-unal` → Exclude unmapped reads

`-p 40` → Use 40 threads

`-L 6` → Seed length = 6

`-i S,0,0.5` → Seed interval function

`--ignore-quals` → Ignore quality scores

`--norc` → Map only to the forward strand

`--score-min L,-1,-0.6` → Custom alignment score threshold

`-D 20` → Max seed extension attempts

#### 3. Unique Mapping & Counting

Keep only uniquely mapped reads and generate a count table.
```bash
grep -v '^@' sample.sam | grep -v 'XS' > sample_unique.sam
cat sample_unique.sam | cut -f 3 | sort | uniq -c | awk '{print $2 "\t" $1}' > sample_unique_counts.txt
```
Key parameters
`grep -v '^@'` → Remove SAM header lines

`grep -v 'XS'` → Exclude reads with secondary alignments (unique mappers only)

`cut -f 3`→ Extract reference name (e.g., chromosome/gene/miRNA)

`sort | uniq -c` → Count unique reads per reference

`awk '{print $2 "\t" $1}'` → Output as:
```ngix
reference_name    count
```
✅ Final output:
sample_unique_counts.txt → Tab-delimited file containing unique read counts per reference (ready for normalization or differential expression).

Example:
```python-repl
hsa-miR-148a-3p    1342
hsa-miR-21-5p       982
hsa-let-7b-5p       643
...
```
---
## 🔎 miRNA Downstream Analyses

Once the unique counts (`*_unique_counts.txt`) have been generated, the analysis continues in **R**.  
The following sections link to the scripts or notebooks available in this repository.

### [📊 Preprocessing and Differential Expression Analysis](miRNA_DESEQ2.html)
Description of the **preprocessing** and **differential expression analysis** steps performed in R:
- Import of count tables
- Low-expression filtering
- Normalization
- Exploratory Data Analysis
- Model design
-`DESeq2`
- Results

👉 See the file [`preprocessing/README.md`](miRNA_DESEQ2.html)

**Note:** An additional log2 fold-change shrinkage step was applied after DESeq2 analysis using `lfcShrink()` from DESeq2 with the `ashr` method.

Example (male comparison):

```r
resMale_shrunk <- lfcShrink(
  dds,
  contrast = c("Condition", "AUD_Male", "Control_Male"),
  res = resMale,
  type = "ashr"
)
```

### [🔬 Functional Analysis](functional_analysis/README.md)
Investigation of biological functions and pathways:
- Prediction of target genes
- Enrichment analysis (GO, KEGG, Reactome)

👉 See the file [`functional_enrichment_mirna_v2.R`](functional_enrichment_mirna_v2.R)

---

## [🙏 miRNA-lipid integration (mixOmics)](AUD_REPORT.Rmd)
Integration of miRNA and lipidomics datasets using **mixOmics**:
- Lipid data available in [Zenodo](https://doi.org/10.5281/zenodo.8360144)
- Partial Least Squares (**PLS**) to explore miRNA–lipid correlations
- **block.sPLS** for multi-omics integration and feature selection

👉 See the file [`integration_mixomics/README.md`](AUD_REPORT.Rmd)

---

## [💻 Shiny App](web_AUD_Carla)
Interactive web application built with **Shiny** to explore:
- Normalized counts and sample metadata
- Differential expression tables and volcano plots
- Correlations between miRNAs and lipids (mixOmics integration)
- Custom searches (e.g., by specific miRNA name)

👉 See the folder [`web_AUD_Carla/`](web_AUD_Carla/) for:
- [`app_AUD.R`](web_AUD_Carla/app.R): Main Shiny application:`ui.R`, `server.R`
- Supporting data files (`data`, `www`, etc.)

