<template>
       <v-container>
        <v-row>

          <v-col
            cols="12"
            sm="12"
          >
            <v-sheet
              min-height="70vh"
              rounded="lg"
            >
            <div class="pa-2">  
              <v-expansion-panels>
                <v-expansion-panel>
                    <v-expansion-panel-header>About MorbidGenes</v-expansion-panel-header>
                    <v-expansion-panel-content>
                      <p>Identifying clinically relevant genetic variants is crucial for a fast and reliable genetic diagnosis. With exome sequencing now standard, diagnostic labs are in need of a, in principle, to-the-day-accurate list of genes associated with rare diseases. Manual curation efforts are slow and often disease specific, while efforts relying on single sources are too inaccurate and may result in false-positive genes.</p>
                      <p>We established the MorbidGenes panel based on a list of publicly available databases: OMIM, PanelApp, SysNDD, ClinVar, HGMD and GenCC. A simple logic allows inclusion of genes with sufficient evidence based on a voting algorithm. By providing an API endpoint, users can directly access the list and metadata for all relevant information on their genes of interest.</p>
                      <p>The panel currently includes 4,677 genes (v.2022-02.1, as of February 2022) with minimally sufficient evidence on disease causality to classify them as diagnostically relevant. Reproducible filtering and versioning allow the integration into diagnostic pipelines. In-house implementation successfully removed false positive genes and reduced time requirements in routine exome diagnostics. The panel is updated monthly, and we will integrate novel sources on a regular basis. The panel is freely available at https://morbidgenes.org/.</p>
                      <p>The MorbidGenes panel is a comprehensive and open overview of clinically relevant genes based on a growing list of sources. It supports genetic diagnostics labs by providing diagnostically relevant genes in a QM conform format on a monthly basis with more frequent updates planned. Once genomes are standard, diagnostically relevant non-coding regions will also be included.</p>
                    </v-expansion-panel-content>
                  </v-expansion-panel>
                  <v-expansion-panel>
                    <v-expansion-panel-header>Citation policy</v-expansion-panel-header>
                    <v-expansion-panel-content>
                      <p>In addition to this website, the curret MorbidGenes panel can be cited using the Zenodo DOI:</p>
                      <p><a href="https://doi.org/10.5281/zenodo.6136995"><img src="https://zenodo.org/badge/DOI/10.5281/zenodo.6136995.svg" alt="DOI"></a></p>
                      <p>We are currently working on a manuscript describing development of MorbidGenes. Until then please cite above DOI if you use MorbidGenes in your work.</p>
                    </v-expansion-panel-content>
                  </v-expansion-panel>
                  <v-expansion-panel>
                    <v-expansion-panel-header>News and Updates</v-expansion-panel-header>
                    <v-expansion-panel-content>
                      2022-02-19: The MorbidGenes webtools is currently in alpha development status and changes rapidely.
                    </v-expansion-panel-content>
                  </v-expansion-panel>
                  <v-expansion-panel>
                    <v-expansion-panel-header>Help and FAQ</v-expansion-panel-header>
                    <v-expansion-panel-content>
                      Here we will present Help and FAQ once the website and API functionallity is complete.
                    </v-expansion-panel-content>
                </v-expansion-panel>
              </v-expansion-panels>
            </div>
            </v-sheet>
          </v-col>

        </v-row>
      </v-container>
</template>
