Dietary supplements and biological performance in abalone: A systematic evidence map

This repository stores the data, bibliometric files, and code used for this study. 

Please find the description for each folder below. Kindly contact Rebecca Pedler (Rebecca.pedler@yumbah.com) for any queries, or to reuse any data or analysis for future studies.

Data

This folder contains data and metadata for the systematic evidence map: 

•	Systematic Evidence Map - Dietary Supplements and Abalone Performance.csv: This csv contains extracted data from all eligible articles. 

•	Systematic Evidence Map - Dietary Supplements and Abalone Performance - Metadata.csv: This csv contains the metadata description of information extracted from eligible articles. 

R_scripts

This folder contains R scripts used throughout the project: 

•	Deduplicate.R: This R script was used to remove duplicate hits from the WOS 05062025.csv and SCOPUS 05062025.csv searches. 

•	Systematic Evidence Map - Dietary Supplements and Abalone Performance.R: this R script was used to analyse extracted data from eligible articles and generate figures. 

•	Word cloud script 15042025.R: This R script was used to generate a wordcloud from most used phrases within the title and keywords of benchmark papers. These terms were categorised into PICO elements to devise an initial search string using a series of Boolean operators.

Searches

This folder contains bibliometric files created during primary and grey literature searches. These files are stored in the following subfolders:

•	primary_literature:

within this folder, the following files are stored:

-	WOS 05062025.csv: This file contains all hits returned from the search string on Web of Science Core Collection (05062025).
-	SCOPUS 05062025.csv: This file contains all hits returned from the search string on SCOPUS (05062025).
-	WOS AND SCOPUS DUPLICATES REMOVED 1 – 1000.csv: This file contains hits 1 – 1000 returned from Web of Science Core Collection and SCOPUS (05062025) after deduplication using Deduplicate.R.
-	WOS AND SCOPUS DUPLICATES REMOVED 1001 - 2134.csv: This file contains hits 1001 - 2134 returned from Web of Science Core Collection and SCOPUS (05062025) after deduplication using Deduplicate.R.
  
  
•	Grey_literature:

within this folder, the following files are stored:

-	BASES ALL 22052025.csv: This file contains all hits returned from BASES (22052025) using a modified string “Haliot” AND “Diet”.
-	BASES DUPLICATES REMOVED 22052025.csv: This file contains all hits returned from BASES (22052025) using a modified string “Haliot” AND “Diet” and after deduplication using Deduplicate.R.
-	physical_grey_literature.csv: This file contains details of physical grey literature documents which proceeded to full-text screening. The results of screening and inclusion/exclusion reason are also included.

Screening

This folder contains abstract and full-text screening files: 

•	RP_abstracts.csv: This csv contains bibliometric information and outcomes for articles proceeding through abstract screening. 

•	RP_fulltext.csv: This csv contains bibliometric information and outcomes for articles proceeding through full-text screening. 


