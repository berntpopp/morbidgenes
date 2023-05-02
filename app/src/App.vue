<template>
  <v-app id="morbidgenes">

<v-app-bar
  app
>
  <v-toolbar-title>
    <v-tab
    to="/"
    >
      <img
        src="../public/morbidgenes_logo.png"
        height="50px"
        alt="MorbidGenes Logo"
      />
    </v-tab>
  </v-toolbar-title>

  <v-spacer></v-spacer>

  <v-toolbar-items class="hidden-sm-and-down">

    <v-btn text
      v-for="item in items"
      :key="item.title"
      :to="item.to"
    >
      {{ item.title }}
    </v-btn>
  </v-toolbar-items>
  
  <!-- // collapsed toolbar for small screen devices //-->
  <v-toolbar-items class="hidden-md-and-up">
    <v-menu 
    offset-y
    >
        <template v-slot:activator="{ on }">
        <v-btn
            text
            v-on="on"
        >
        <v-icon
        dark
        right
      >
        mdi-dots-vertical
      </v-icon>
        </v-btn>
        </template>
    
        <v-card>
        <v-list dense>
            <v-list-item
            v-for="item in items_small"
            :key="`notification-key-${item.id}`"
            :to="item.to"
            link
            >
            <v-list-item-title>
                {{ item.title }}
            </v-list-item-title>
            </v-list-item>
        </v-list>
        </v-card>
    </v-menu>
  </v-toolbar-items>
  <!-- // collapsed toolbar for small screen devices //-->

</v-app-bar>


    <v-main class="grey lighten-3">
      <router-view></router-view>
    </v-main>


    <v-banner
      color="warning"
      elevation="3"
      two-line
      transition="slide-y-transition"
      icon="mdi-alert"
    >
      {{language ? 'The information on this website is not intended for direct diagnostic use or medical decision-making without review by a genetics professional. Individuals should not change their health behavior on the basis of information contained on this website. MorbidGenes does not independently verify the information gathered from external sources. Though the information is obtained from carefully reviewed sources which we believe to be reliable, no warranty, expressed or implied, is made regarding accuracy, adequacy, completeness, reliability or usefulness of any information. This disclaimer applies to both isolated and aggregate uses of the information. The information is provided on an "as is" basis, collected through periodic recalucation from external sources and therefore may not represent the most up-to-date information from these sources. If you have questions about the medical relevance of information contained on this website, please see a healthcare professional. If you have questions about specific gene-disease claims, please contact the respective primary sources. If you have questions about the representation of the data on this website, please contact morbidgenes@morbidgenes.org.' : 'Die Informationen auf dieser Website sind nicht für den direkten diagnostischen Gebrauch oder die medizinische Entscheidungsfindung ohne Überprüfung durch eine genetische Fachkraft bestimmt. Einzelpersonen sollten ihr Gesundheitsverhalten nicht auf der Grundlage der auf dieser Website enthaltenen Informationen ändern. MorbidGenes überprüft nicht unabhängig die Informationen, die aus externen Quellen stammen. Obwohl die Informationen aus sorgfältig geprüften Quellen stammen, die wir für zuverlässig halten, wird weder ausdrücklich noch impliziert eine Garantie für die Richtigkeit, Angemessenheit, Vollständigkeit, Zuverlässigkeit oder Nützlichkeit der Informationen übernommen. Dieser Haftungsausschluss gilt sowohl für die isolierte als auch für die aggregierte Nutzung der Informationen. Die Informationen werden auf der Grundlage des Ist-Zustandes zur Verfügung gestellt und in regelmäßigen Abständen aus externen Quellen neu berechnet, so dass sie möglicherweise nicht die aktuellsten Informationen aus diesen Quellen darstellen. Wenn Sie Fragen zur medizinischen Relevanz der auf dieser Website enthaltenen Informationen haben, wenden Sie sich bitte an einen Arzt. Wenn Sie Fragen zu bestimmten Behauptungen über Genkrankheiten haben, wenden Sie sich bitte an die entsprechenden Primärquellen. Wenn Sie Fragen zur Darstellung der Daten auf dieser Website haben, wenden Sie sich bitte an morbidgenes@morbidgenes.org.'}}
      <template v-slot:actions="{ dismiss }">
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
          @click="dismiss"
        >
          Dismiss
        </v-btn>
      </template>
    </v-banner>

    <v-footer padless>
      <v-card-text class="text-center">
        <v-btn
          v-for="footer_item in footer_items"
          :key="footer_item.icon"
          class="mx-4"
          icon
        >
        <v-btn 
        icon 
        :href="footer_item.to" 
        :target="footer_item.target"
        >
          <v-icon size="24px" aria-hidden="false">
            {{ footer_item.icon }}
          </v-icon>
        </v-btn>
        </v-btn>
      </v-card-text>
    </v-footer>
    
  </v-app>
</template>


<script>
  export default {
    data: () => ({
      items: [
        {title: 'About', to: '/about', id: 'about'},
      ],
      items_small: [
        {title: 'About', to: '/about', id: 'about'},
      ],
      footer_items: [
        {icon: 'mdi-github', to: 'https://github.com/berntpopp/morbidgenes', target: '_blank', alt: 'GitHub repository'},
        {icon: 'mdi-api', to: '/API', target: '_self', alt: 'OpenAPI Swagger frontend'},
        {icon: 'mdi-copyright', to: 'https://creativecommons.org/licenses/by/4.0/', target: '_blank', alt: 'License'}
      ],
      language: true
    }),
      methods: {
        async toggleLanguage() {
          this.language = !this.language;
      },
      }
  }
</script>