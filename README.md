# Global Dataset on Blue Nutrients in Seagrass Meadows

## Dataset Metadata

**Title of dataset**  
Global Dataset on Blue Nutrients in Seagrass Meadows

**URL of dataset**  
Forthcoming upon decision at the first review stage (planned public release via GitHub and an assigned DOI through Zenodo)

**Abstract**  
This dataset compiles global observations of sedimentary nitrogen (N) and phosphorus (P) stocks beneath seagrass meadows to quantify their magnitude, variability, and vulnerability to meadow loss. Data were synthesized from published literature spanning nearly all seagrass bioregions and major taxa. Measurements include sediment nutrient concentrations, bulk density (when available), sampling depth, geographic location, and species composition, standardized to areal stocks (Mg ha⁻¹) and normalized to a 1-m sediment depth. The dataset supports analyses of global nutrient inventories, stoichiometric relationships, and modeling of nutrient release under different seagrass loss scenarios, providing a foundation for integrating “blue nitrogen” and “blue phosphorus” into coastal biogeochemical and management frameworks. All data are stored in the *NP Literature Search Data.xlsx* data file.

**Keywords**  
blue nutrients, nitrogen, phosphorus, sediment biogeochemistry, coastal ecosystems, eutrophication, blue carbon

---

## Authorship and Attribution

**Lead author for the dataset**  
Kyle A. Capistrant-Fossa

**Title and position of lead author**  
Postdoctoral Researcher

**Organization and address of lead author**  
Virginia Institute of Marine Science, College of William & Mary  
1370 Greate Road  
Gloucester Point, Virginia 23062, USA

**Email address of lead author**  
kacapistrantfossa@vims.edu

**Additional authors or contributors**  
Alexandra L. Bijak, Adrian Draughn Willingham, Christopher J. Patrick

**Organization associated with the data**  
N/A

**Funding**  
Kyle A. Capistrant-Fossa, RAISE Postdoctoral Fellowship, Virginia Institute of Marine Science  
Alexandra L. Bijak, Ocean Sciences Postdoctoral Fellowship, National Science Foundation

**License**  
CC BY 4.0 (Creative Commons Attribution)

---

## Spatial and Temporal Coverage

**Geographic location – verbal description**  
Global coastal regions containing seagrass meadows, including temperate, subtropical, and tropical shorelines across North and South America, Europe, Africa, Asia, and Australia.

**Geographic coverage bounding coordinates**  
Not specified

**Time frame – Begin date**  
1980

**Time frame – End date**  
2025

---

## Study Design and Methods

**General study design**  
Global literature synthesis and data compilation of published field measurements of sedimentary nitrogen and phosphorus in seagrass meadows, combined with numerical modeling to assess nutrient stability under meadow loss scenarios.

**Methods description**  
A systematic literature review following PRISMA-EcoEVO guidelines was conducted using Google Scholar, Scopus, and PubMed to identify studies reporting sediment N and/or P in seagrass meadows. Data were extracted from text, tables, figures, and supplemental materials. When bulk density was not reported, a global average value for seagrass sediments was applied (Fourqurean et al. 2012). All measurements were converted to areal stocks (Mg ha⁻¹) and standardized to a 1-m sediment depth. Associated metadata (location, species, depth, bioregion) were recorded for each observation. Data were aggregated for global analyses and used to parameterize a box model evaluating nutrient sequestration and release under different seagrass loss scenarios.

**Laboratory, field, or analytical methods**  
Sediment nitrogen and phosphorus concentrations were originally measured in source studies using standard elemental analysis techniques (e.g., combustion elemental analyzers for N, colorimetric or ICP-based methods for P following digestion). Areal stocks were calculated by multiplying concentration by dry bulk density and sediment depth. Log-normal transformations were applied where appropriate for statistical analyses. Modeled outputs were generated using Monte Carlo simulations parameterized with literature-derived burial rates, loss rates, and sediment disturbance depths.

**Taxonomic species or groups**  
Seagrasses (marine angiosperms), including genera *Zostera*, *Thalassia*, *Halodule*, *Syringodium*, *Cymodocea*, *Posidonia*, *Halophila*, and *Enhalus*.

**Quality control**  
All extracted data were checked for unit consistency, transcription errors, and geographic accuracy. Values were standardized to common units and depths. Outliers were evaluated against original sources.

---

# Table 1. Dataset Description

## Dataset filename
**NP Literature Search Data.xlsx – Sheet: NPSeagrass**

**Dataset description**  
This dataset contains N and P data collected from global seagrass meadows.

| Column name | Description | Units | Data format | Missing data code |
|------------|------------|-------|-------------|-------------------|
| UniqueIdentifier | Arbitrary identifier for each record | none | character | Blank Cell |
| Title | Title of the source study | none | character | Blank Cell |
| DOI Link | DOI or stable link to source | none | character | Blank Cell |
| Geographic Location | Aggregated geographic identifier | none | character | Blank Cell |
| Country | Country where samples were collected | none | character | Blank Cell |
| Continent | Continent of study location | none | character | Blank Cell |
| Core Diameter (cm) | Inner diameter of sediment core | cm | numeric | Blank Cell |
| Core Depth (cm) | Depth of sediment core | cm | numeric | Blank Cell |
| Latitude | Representative latitude | decimal degrees | numeric | Blank Cell |
| Longitude | Representative longitude | decimal degrees | numeric | Blank Cell |
| Species 1–5 | Seagrass species present at site | none | character | Blank Cell |
| DBD1–DBD5 | Dry bulk density per species | g cm⁻³ | numeric | Blank Cell |
| N Stock Units | Units of nitrogen data | none | character | Blank Cell |
| NS1–NS5 | Sedimentary nitrogen content | varies | numeric | Blank Cell |
| P Stock Units | Units of phosphorus data | none | character | Blank Cell |
| PS1–PS5 | Sedimentary phosphorus content | varies | numeric | Blank Cell |
| Notes | Notes from data extraction | none | character | Blank Cell |

---

## Dataset filename
**NP Literature Search Data.xlsx – Sheet: NPSeagrass (Non-seagrass systems)**

**Dataset description**  
Observations of N and P from marsh, mangrove, and terrestrial ecosystems.

| Column name | Description | Units | Data format | Missing data code |
|------------|------------|-------|-------------|-------------------|
| Name | Sample name from original manuscript | none | character | Blank Cell |
| Genus | Ecosystem type grouping | none | character | Blank Cell |
| Region2 | Broad climatic region | none | character | Blank Cell |
| stock_Mg_ha_100cm | Total nutrient stock (1 m depth) | Mg ha⁻¹ | numeric | Blank Cell |
| element | Nutrient type (N or P) | none | character | Blank Cell |
| Reference | Source of data | none | character | Blank Cell |

---

## Dataset filename
**NP Literature Search Data.xlsx – Sheet: Bibliography**

**Dataset description**  
Bibliographic information exported from Google Scholar.

| Column name | Description | Units | Data format | Missing data code |
|------------|------------|-------|-------------|-------------------|
| Authors | Authors of reviewed manuscript | none | character | Blank Cell |
| Title | Manuscript title | none | character | Blank Cell |
| Publication | Journal or publication name | none | character | Blank Cell |
| Volume | Journal volume | none | character | Blank Cell |
| Number | Journal issue number | none | character | Blank Cell |
| Pages | Page range | none | character | Blank Cell |
| Year | Year of publication | year | numeric | Blank Cell |
| Publisher | Publisher name | none | character | Blank Cell |
