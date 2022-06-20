#27 April 2022

# Code for creating Ecological Niche Models (ENMs) of 5 wild Bovidae species. 
 * *Bos gaurus* (Gaur)
 * *Bos javanicus* (Banteng)
 * *Bubalus arnee* (Wild water buffalo)
 * *Capricornis sumatraensis* (Mainland serow)
 * *Naemorhedus griseus* (Chinese goral)

These codes are contain the some parts of data from the paper: 
"Mapping threatened Thai Bovidae species provides opportunities for improved conservation outcomes in Asia"
(unpublishing paper). 

We were using the package [ENMTML](https://github.com/andrefaa/ENMTML) for model building.

# How to use these scripts:
 - download the dataset: [
](https://drive.google.com/drive/u/2/folders/1Zp-2At0YeP9QZKxtOb4yGundLvU4VdFW)
      * folders and subfolder structure: 
      * 1) 1. data_preparation: the dataset use for modelling
            * 1.1 env: contains all environmental variables, cropped as two accessible areas 1) species-specific accessible areas (envgaur, envbanteng, eenvbuffalo, envserow, envgoral) and 2) a large accessible areas (named: env_la)
            * 1.2 
      * 2) 2. result_sample: Some of the results (habitat suitability maps, richness maps) from modelling, contains 3 subfolders.
            * 2.1 binary: The best binary model of the habitat suitability (* .tif).
            * 2.2 ensembles: The best ensemble models of the habitat suitability (* .tif).
            * 2.3 dataset_combine: The dataset extracting and combining from rasters results, contains habitat suitability values (binary and probability), protected areas category, environmental variables, occurrence data (from gbif.org).   
 - setwd the working directory 
 - run the scripts according to the numerical numbers
