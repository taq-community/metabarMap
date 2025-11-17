##### Environmental DNA (eDNA) Metabarcoding

This application displays species detected through environmental DNA (eDNA) metabarcoding analysis of water samples collected at **13 sampling stations** 

##### Molecular Methods

The analysis uses the **12S ribosomal RNA (12S rRNA)** mitochondrial gene as a barcode marker. This primer is particularly effective for detecting:

- **Fish species** (primary target group)
- **Amphibians** (frogs, salamanders)
- **Birds** (waterfowl and other aquatic birds)
- **Aquatic mammals** (beavers, muskrats, etc.)

##### Sample and data Processing

- DNA extraction from filtered water samples
- PCR amplification of the 12S rRNA gene region
- High-throughput sequencing with Illumina 
- Bioinformatics analysis using **BARQUE** (Barcoding and Automated Recognition of QUality in Environmental DNA)
- Taxonomic assignment using reference databases of 2829 species

##### Species Classification

**Detected:** Species with clear taxonomic assignments based on sequence matches.

**Ambiguous:** Groups where sequences match multiple species equally well, indicating potential presence of any species within the group.

##### Read Counts

The number of DNA sequence reads detected for each species. Higher read counts could means higher DNA concentrations, __but most of the time this is not directly proportional to organism abundance because it also related on the laboratory process__.

##### Species Status

<span style='color: #00A000;'>✓ Native:</span> Indigenous to Quebec
<span style='color: #D9534F;'>⚠ Exotic:</span> Non-native to Quebec
