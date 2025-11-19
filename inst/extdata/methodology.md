<h4 style="font-weight:bold;margin-top:2rem;margin-bottom:1rem"> Study design </h3>

This application displays species detected through environmental DNA (eDNA) metabarcoding analysis of water samples collected at **13 sampling stations** in October 2025. No replicates have been considered in laboratory.

<h4 style="font-weight:bold;margin-top:2rem;margin-bottom:1rem"> Methods </h3>

##### Molecular methods

The analysis uses the **12S ribosomal RNA (12S rRNA)** mitochondrial gene as a barcode marker. This primer is particularly effective for detecting:

- **Fish species** (primary target group)
- **Amphibians** (frogs, salamanders)
- **Birds** (waterfowl and other aquatic birds)
- **Aquatic mammals** (beavers, muskrats, etc.)

##### Sample and data Processing

DNA is extracted from filtered water samples, followed by PCR amplification of the 12S rRNA gene region. The amplified DNA undergoes high-throughput sequencing with Illumina technology. Bioinformatics analysis is performed using **BARQUE** (Barcoding and Automated Recognition of QUality in Environmental DNA), and taxonomic assignment is conducted using the BARQUE internal reference databases pertaining 12S rRNA reference sequences for 2829 species.

<h4 style="font-weight:bold;margin-top:2rem;margin-bottom:1rem"> Terms explanation </h3>

##### Species tab panel 

- **Detected:** Species with clear taxonomic assignments based on sequence matches.
- **Ambiguous:** Groups where sequences match multiple species equally well, indicating potential presence of any species within the group.

##### Detection status

Species detections are classified into three confidence levels based on the number of DNA sequence reads:

| Detection status | Read Count | Interpretation |
|-----------------|------------|----------------|
| **Confident** | > 100 reads | High confidence detection with strong DNA signal |
| **Probable** | 10-100 reads | Moderate confidence detection |
| **Uncertain** | < 10 reads | Low confidence detection, may require additional verification |

__Note:__ Read counts reflect DNA concentrations detected in the sample, but are not directly proportional to organism abundance due to variations in the laboratory process and species autoecology.

##### Species Status

<span style='color: #00A000;'>✓ Native:</span> Indigenous to Quebec
<span style='color: #D9534F;'>⚠ Exotic:</span> Non-native to Quebec
