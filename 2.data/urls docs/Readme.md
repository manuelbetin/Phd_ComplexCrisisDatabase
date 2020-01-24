
# description of datasets containing the urls of IMF archive documents


consolidated_urls_by_ctry.RData: contains all the imf links to pdf documents available from https://archivescatalog.imf.org/search/simple


each separate urls_imf_XXX.RData contains a subsample of urls filter by type of document as determined by the information contained in the title

Final_urls_extraction_with_hierarchies.RData correspond to the urls that have been download in the first version of the extraction and that contain more precise metadata of the documents but that are not available in the consolidated_urls_by_ctry files. We we combine those two database to obtain the metadata for at leas the files that have it.


urls_consultations_from_clean_report.RData is a cleaned version of consolidated_urls_by_ctry.RData after running the file 1.2. clean_urls_reports.rmd


