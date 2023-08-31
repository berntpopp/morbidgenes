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
                  <p>In addition to this website, the current MorbidGenes panel can be cited using the Zenodo DOI:</p>
                  <p>
                    <a
                      href="https://doi.org/10.5281/zenodo.6136995"
                      target="_blank"
                    >
                      <img
                        src="../../public/zenodo-gradient-round.svg"
                        height="30px"
                        alt="Zenodo Logo"
                      >
                    </a>
                  </p>
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
              <v-expansion-panel>
                <v-expansion-panel-header>Impressum</v-expansion-panel-header>
                <v-expansion-panel-content>
                  <v-btn
                    text
                    color="primary"
                    @click="toggleLanguage"
                  >
                    {{ language ? 'German' : 'English' }}
                  </v-btn><br><br>
                  <v-card
                    v-if="language"
                    elevation="2"
                  >
                    Gemäß § 28 BDSG widersprechen wir jeder kommerziellen Verwendung und Weitergabe der Daten.<br><br>

                    Verantwortunsbereich:<br>
                    Das Impressum gilt nur für die Internetpräsenz unter der Adresse: https://morbidgenes.org

                    Abgrenzung:<br>
                    Die Web-Präsenz ist Teil des WWW und dementsprechend mit fremden, sich jederzeit wandeln könnenden Web-Sites verknüpft, die folglich auch nicht diesem Verantwortungsbereich unterliegen und für die nachfolgende Informationen nicht gelten. Dass die Links weder gegen Sitten noch Gesetze verstoßen, wurde genau ein Mal geprüft (bevor sie hier aufgenommen wurden).<br><br>

                    Diensteanbieter:<br>
                    Bernt Popp<br><br>

                    Ansprechpartner für die Webseite:<br>
                    Bernt Popp (bernt.popp@medizin.uni-leipzig.de)<br><br>

                    Verantwortlicher:<br>
                    Bernt Popp (bernt.popp@medizin.uni-leipzig.de)<br><br>

                    Anschrift:<br>
                    Sekretariat<br>
                    Philipp-Rosenthal-Str. 55<br>
                    04103 Leipzig<br>
                    Telefon: 0341 - 97 23800<br>

                    Urheberschutz und Nutzung:<br>
                    Der Urheber räumt Ihnen ganz konkret das Nutzungsrecht ein, sich eine private Kopie für persönliche Zwecke anzufertigen. Nicht berechtigt sind Sie dagegen, die Materialien zu verändern und/oder weiter zu geben oder gar selbst zu veröffentlichen. Wenn nicht ausdrücklich anders vermerkt, liegen die Urheberrechte bei Johann Lieberwirth Datenschutz Personenbezogene Daten werden nur mit Ihrem Wissen und Ihrer Einwilligung erhoben. Auf Antrag erhalten Sie unentgeltlich Auskunft zu den über Sie gespeicherten personenbezogenen Daten. Wenden Sie sich dazu bitte an den Administrator.<br><br>

                    Keine Haftung:<br>
                    Die Inhalte dieses Webprojektes wurden sorgfältig geprüft und nach bestem Wissen erstellt. Aber für die hier dargebotenen Informationen wird kein Anspruch auf Vollständigkeit, Aktualität, Qualität und Richtigkeit erhoben. Es kann keine Verantwortung für Schäden übernommen werden, die durch das Vertrauen auf die Inhalte dieser Website oder deren Gebrauch entstehen.<br><br>

                    Schutzrechtsverletzung:<br>
                    Falls Sie vermuten, dass von dieser Website aus eines Ihrer Schutzrechte verletzt wird, teilen Sie das bitte umgehend per elektronischer Post mit, damit zügig Abhilfe geschafft werden kann. Bitte nehmen Sie zur Kenntnis: Die zeitaufwändigere Einschaltung eines Anwaltes zur für den Diensteanbieter kostenpflichtigen Abmahnung entspricht nicht dessen wirklichen oder mutmaßlichen Willen.<br><br>

                    lt. Urteil vom 12. Mai 1998 - 312 O 85/98 - "Haftung für Links" hat das Landgericht Hamburg entschieden, dass man durch die Anbringung eines Links, die Inhalte der gelinkten Seite ggf. mit zu verantworten hat. Dies kann nur dadurch verhindert werden, dass man sich ausdrücklich von diesen Inhalten distanziert. 'Hiermit distanzieren wir uns ausdrücklich von allen Inhalten aller gelinkten Seiten auf unserer Website und machen uns diese Inhalte nicht zu eigen. Diese Erklärung gilt für alle auf unsere Website angebrachten Links.'
                  </v-card>
                  <v-card
                    v-if="!language"
                    elevation="2"
                  >
                    The Institute for Human Genetics (University Medical Center Leipzig) makes no representation about the suitability or accuracy of this software or data for any purpose, and makes no warranties, including fitness for a particular purpose or that the use of this software will not infringe any third party patents, copyrights, trademarks or other rights.<br><br>

                    Responsible for this website:<br>
                    Bernt Popp (bernt.popp@medizin.uni-leipzig.de)<br><br>

                    Responsible for this project:<br>
                    Bernt Popp (bernt.popp@medizin.uni-leipzig.de)<br><br>

                    Address:<br>
                    Sekretariat<br>
                    Philipp-Rosenthal-Str. 55<br>
                    04103 Leipzig<br>
                    GERMANY<br>
                    Telefon: 0341 - 97 23800
                  </v-card>
                </v-expansion-panel-content>
              </v-expansion-panel>
            </v-expansion-panels>
          </div>
        </v-sheet>
      </v-col>
    </v-row>
  </v-container>
</template>

<script>
  export default {
    data: () => ({
      language: true,
    }),
    methods: {
      async toggleLanguage () {
        this.language = !this.language
      },
    },
  }
</script>
