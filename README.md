# Code for creating Ecological Niche Models (ENMs) for 5 wild Bovidae species. 

Reference: Horpiencharoen et al. *submitted*. Mapping threatened bovids provides opportunities for improved conservation outcomes in Asia.

 * *Bos gaurus* (Gaur)
 * *Bos javanicus* (Banteng)
 * *Bubalus arnee* (Wild water buffalo)
 * *Capricornis sumatraensis* (Mainland serow)
 * *Naemorhedus griseus* (Chinese goral)



We used the package [ENMTML](https://github.com/andrefaa/ENMTML) for model building.

# How to use these scripts:
 - Download the dataset: [Google Drive](https://drive.google.com/drive/folders/1tKMkeltE1eSnrmTh4obpIPjYEJaJTChX?usp=sharing) 
   
   **Warning: These are large files  (~10 GB) and download can be slow.** 
   
      * folders and subfolder structure: 
        * 1) data_preparation: the dataset use for modelling
             * 1.1 `acc`: the polygons (.shp) of accessible areas, it contains 6 subfolders large accessible areas (named: acc_la), and the others for species-specific accessible areas (accbanteng, accbuffalo, accgaur, accserow, accgoral). Selecting the accessible areas using [ecoregions](https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world)
             * 1.2 `adm_border`: the polygons (.shp) of admistrative border using for mapping and creating figures.
             * 1.3 `env`: contains all environmental variables, cropped as two accessible areas 1) species-specific accessible areas (envgaur, envbanteng, eenvbuffalo, envserow, envgoral) and 2) large accessible area (env_la)
             * 1.4 `iucn`: IUCN range (.shp) for all the species
             * 1.5 `PA_and_country`: contains the data of countries and [WDPA](https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA) protected areas boundary (.shp, .tif). 
             * 1.6 `text files` (.txt): some of the occurrence data from the open source ([gbif.org](https://www.gbif.org/)) 
        * 2) result_sample: Some of the results (habitat suitability maps, richness maps[`.tif`]) from modelling, contains 3 subfolders.
             * 2.1 `binary`: The best binary model of the habitat suitability (.tif).
             * 2.2 `ensembles`: The best ensemble models of the habitat suitability (.tif).
             * 2.3 `dataset_combine`: The dataset extracting and combining from rasters results contains habitat suitability values (binary and probability), protected areas category, environmental variables and observation data.
 - Unzip the downloaded folder
 - Set up a working directory in R before running the code
 - Run the scripts according to the numerical order
