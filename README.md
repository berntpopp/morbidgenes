# MorbidGenes panel: a monthly updated list of diagnostically relevant genes derived from diverse sources

Identifying clinically relevant genetic variants is crucial for a fast and reliable genetic diagnosis. With exome sequencing now standard, diagnostic labs are in need of a, in principle, to-the-day-accurate list of genes associated with rare diseases. Manual curation efforts are slow and often disease specific, while efforts relying on single sources are too inaccurate and may result in false-positive genes.

We established the MorbidGenes panel based on a list of publicly available databases: OMIM, PanelApp, SysNDD, ClinVar, HGMD and GenCC. A simple logic allows inclusion of genes with sufficient evidence based on a voting algorithm. By providing an API endpoint, users can directly access the list and metadata for all relevant information on their genes of interest.

The panel currently includes 4,677 genes (v.2022-02.1, as of February 2022) with minimally sufficient evidence on disease causality to classify them as diagnostically relevant. Reproducible filtering and versioning allow the integration into diagnostic pipelines. In-house implementation successfully removed false positive genes and reduced time requirements in routine exome diagnostics. The panel is updated monthly, and we will integrate novel sources on a regular basis. The panel is freely available at https://morbidgenes.org/.

The MorbidGenes panel is a comprehensive and open overview of clinically relevant genes based on a growing list of sources. It supports genetic diagnostics labs by providing diagnostically relevant genes in a QM conform format on a monthly basis with more frequent updates planned. Once genomes are standard, diagnostically relevant non-coding regions will also be included.


## Authors

Contributors names and contact info

* Bernt Popp, [ORCID ID: 0000-0002-3679-1081](https://orcid.org/0000-0002-3679-1081), [@berntpopp](https://twitter.com/berntpopp)
* Rami Abou Jamra, [ORCID ID: 0000-0002-1542-1399](https://orcid.org/0000-0002-1542-1399)
* Konrad Platzer, [ORCID ID: 0000-0001-6127-6308](https://orcid.org/0000-0001-6127-6308)
* Robin-Tobias Jauss, [ORCID ID: 0000-0002-8285-9155](https://orcid.org/0000-0002-8285-9155)

## Version History

* 0.1.0
    * Initial GitHub Release

## License

This project is licensed under the "BSD 3-Clause" License - see the LICENSE.md file for details

## Acknowledgments
