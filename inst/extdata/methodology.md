# Rouyn-Noranda

Cette application présente les espèces identifiées à plusieurs stations
d'échantillonnage sur le territoire de Rouyn-Noranda en juillet 2025.
Pour ce faire, de l'eau a été récoltée à 9 stations (8 lacs) puis analysée à
l'Institut national de la recherche scientifique (INRS) pour déterminer
la présence des espèces retrouvées dans les échantillons à l\'aide de
l\'ADN environnemental (ADNe).

L\'ADN environnemental est l\'ADN laissé dans l\'environnement par les
organismes vivants à travers leurs cellules, leurs excréments, leur
mucus, etc. En analysant seulement un litre d\'eau, il est possible
d\'identifier la grande majorité des espèces présentes dans un plan
d\'eau à un moment donné, sans avoir à capturer ou à observer
directement les organismes.

Pour ce projet, deux approches complémentaires ont été utilisées.
D'abord, le metabarcoding permet de brosser un portrait global de la
biodiversité, puis la PCR quantitative (qPCR) vise à détecter la
présence d\'une espèce à la fois.

L\'extraction d\'ADNe des échantillons et l\'analyse génomique ont été
réalisées au laboratoire de Valérie Langlois par Catherine Dubuc, Julie
Couillard (Ph. D.) et Tuan Anh To à l'INRS. L\'analyse bioinformatique a
été conduite par Steve Vissault (M. Sc.) à l\'aide de
[Barque](https://github.com/enormandeau/barque) (*Barcoding and
Automated Recognition of QUality in Environmental DNA*) v1.8.5.

## Méthodes

### Metabarcoding

La méthode consiste à extraire l\'ADN de l\'eau filtrée, puis à
amplifier par PCR une région ciblée d'un gène connu. Cette technique qui
permet de produire des millions de copies de ce fragment d\'ADN afin de
le rendre détectable. L\'ADN amplifié est ensuite soumis à un séquençage
à haut débit grâce à la technologie Illumina, permettant de lire les
différents fragments d'ADN. Les séquences obtenues sont finalement
comparées à des bases de données existantes afin d\'identifier les
espèces auxquelles elles appartiennent.

Différentes régions de gènes peuvent être analysées selon les espèces
que l\'on cherche à détecter, chaque région étant plus efficace pour un
groupe d\'espèces donné.

#### 12S rRNA

Cette analyse utilise un marqueur génétique spécifique,
soit le gène de l\'ARN ribosomique 12S (ARNr 12S), situé dans les
mitochondries des cellules.

Ce marqueur est particulièrement efficace pour détecter les espèces de
poissons, mais permet également d\'identifier certains amphibiens,
oiseaux et mammifères. Les séquences d\'ADNe obtenues sont ensuite
comparées à celles répertoriées dans
[MitoFish](https://mitofish.aori.u-tokyo.ac.jp/), une base de données
génomiques de référence spécialisée dans les poissons.

#### COI 

Cette analyse repose sur un second marqueur
génétique, soit le gène de la sous-unité I du cytochrome c oxydase
(COI), lui aussi situé dans les mitochondries des cellules. Ce marqueur
est complémentaire au précédent et offre une vision plus large de la
biodiversité.

En effet, le COI est particulièrement efficace pour détecter de nombreux
groupes d\'animaux, aussi bien aquatiques que terrestres. Les séquences
d\'ADN obtenues sont ensuite comparées à celles répertoriées dans la
version 2023 de [BOLD](https://www.boldsystems.org/) (*Barcode of Life
Data Systems*), une base de données de référence qui regroupe des
centaines de milliers d\'espèces.

### qPCR

La méthode consiste aussi à extraire l\'ADN de l\'eau
filtrée, puis à amplifier par PCR une région de l\'ADN propre à une
espèce ciblée. Cette approche permet de déterminer si l'espèce ciblée
est présente ou absente dans le milieu, en vérifiant pour chaque
échantillon si une amplification d\'ADN a eu lieu.

Le cas échéant, il est également possible de quantifier cet ADN,
c\'est-à-dire d\'estimer le nombre de copies d\'ADN par litre d\'eau.
Cette estimation est réalisée à l\'aide d\'une courbe standard,
construite à partir de quantités connues d\'ADN synthétique, qui sert de
référence.

Cette technique est plus sensible que le metabarcoding. Elle permet donc
de confirmer avec plus de précision la présence ou l\'absence d\'une
espèce dans le milieu analysé, tout en donnant une idée plus fiable de
l'abondance, bien qu'elle reste approximative.

