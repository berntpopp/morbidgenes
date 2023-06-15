<!-- Banner.vue -->
<template>
  <div v-if="!banner_acknowledged">
    <v-banner
      color="warning"
      elevation="3"
      two-line
      transition="slide-y-transition"
      :icon="icons.mdiAlert"
    >
      {{language ? 'The information on this website is not intended for direct diagnostic use or medical decision-making without review by a genetics professional. Individuals should not change their health behavior on the basis of information contained on this website. MorbidGenes does not independently verify the information gathered from external sources. Though the information is obtained from carefully reviewed sources which we believe to be reliable, no warranty, expressed or implied, is made regarding accuracy, adequacy, completeness, reliability or usefulness of any information. This disclaimer applies to both isolated and aggregate uses of the information. The information is provided on an "as is" basis, collected through periodic recalucation from external sources and therefore may not represent the most up-to-date information from these sources. If you have questions about the medical relevance of information contained on this website, please see a healthcare professional. If you have questions about specific gene-disease claims, please contact the respective primary sources. If you have questions about the representation of the data on this website, please contact morbidgenes@morbidgenes.org.' : 'Die Informationen auf dieser Website sind nicht für den direkten diagnostischen Gebrauch oder die medizinische Entscheidungsfindung ohne Überprüfung durch eine genetische Fachkraft bestimmt. Einzelpersonen sollten ihr Gesundheitsverhalten nicht auf der Grundlage der auf dieser Website enthaltenen Informationen ändern. MorbidGenes überprüft nicht unabhängig die Informationen, die aus externen Quellen stammen. Obwohl die Informationen aus sorgfältig geprüften Quellen stammen, die wir für zuverlässig halten, wird weder ausdrücklich noch impliziert eine Garantie für die Richtigkeit, Angemessenheit, Vollständigkeit, Zuverlässigkeit oder Nützlichkeit der Informationen übernommen. Dieser Haftungsausschluss gilt sowohl für die isolierte als auch für die aggregierte Nutzung der Informationen. Die Informationen werden auf der Grundlage des Ist-Zustandes zur Verfügung gestellt und in regelmäßigen Abständen aus externen Quellen neu berechnet, so dass sie möglicherweise nicht die aktuellsten Informationen aus diesen Quellen darstellen. Wenn Sie Fragen zur medizinischen Relevanz der auf dieser Website enthaltenen Informationen haben, wenden Sie sich bitte an einen Arzt. Wenn Sie Fragen zu bestimmten Behauptungen über Genkrankheiten haben, wenden Sie sich bitte an die entsprechenden Primärquellen. Wenn Sie Fragen zur Darstellung der Daten auf dieser Website haben, wenden Sie sich bitte an morbidgenes@morbidgenes.org.'}}
        <v-btn
          text
          color="primary"
          @click="toggleLanguage"
        >
          {{language ? 'German' : 'English'}}
        </v-btn>
        <v-btn
          text
          color="primary"
          @click="acknowledgeBanner"
        >
          Dismiss
        </v-btn>
    </v-banner>
  </div>
</template>

<script>
import { mdiAccount, mdiRefresh, mdiLogout, mdiDotsVertical, mdiGithub, mdiApi, mdiCopyright, mdiAlert } from '@mdi/js';

export default {
  data() {
    return {
      banner_acknowledged: localStorage.getItem('banner_acknowledged'),
      language: true,
      icons: {
        mdiAccount,
        mdiRefresh,
        mdiLogout,
        mdiDotsVertical,
        mdiGithub,
        mdiApi,
        mdiCopyright,
        mdiAlert
      },
    };
  },
  methods: {
    // Function to acknowledge the banner.
    // This sets a value in localStorage to remember the user's action.
    acknowledgeBanner() {
      localStorage.setItem('banner_acknowledged', true);
      this.banner_acknowledged = localStorage.getItem('banner_acknowledged');
    },
    toggleLanguage() {
      this.language = !this.language;
    },
  },
};
</script>
