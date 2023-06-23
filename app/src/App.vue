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
          {{ icons.mdiDotsVertical }}
        </v-icon>
          </v-btn>
          </template>
      
          <v-card>
          <v-list dense>
              <v-list-item
              v-for="item in items"
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

    <v-footer padless>  
      <v-card-text class="text-center">  
        <v-btn  
          v-for="footer_item in footer_items"
          :key="footer_item.icon"
          class="mx-4"  
          icon
          :href="footer_item.to"
          :target="footer_item.target"
        :aria-label="footer_item.alt"
        >
          <v-icon size="24px" aria-hidden="false">
            {{ footer_item.icon }}  
          </v-icon>  
        </v-btn>  
      </v-card-text>  
    </v-footer>  

    <!-- The Banner component -->
    <Banner />
    <!-- The Banner component -->    

  </v-app>
</template>


<script>  
import { mdiDotsVertical, mdiGithub, mdiApi, mdiCopyright } from '@mdi/js';

// Import the Banner component  
import Banner from '@/components/small/Banner.vue';  

export default {  
  components: {  
    Banner,  
  },  
  data: () => ({  
    icons: {
      mdiDotsVertical,   
      mdiGithub,   
      mdiApi,   
      mdiCopyright   
    },
    items: [
      {id: 'about', title: 'About', to: '/about'},  
    ],
    footer_items: [
      {icon: mdiGithub, to: 'https://github.com/berntpopp/morbidgenes', target: '_blank', alt: 'GitHub repository'},  
      {icon: mdiApi, to: '/API', target: '_self', alt: 'OpenAPI Swagger frontend'},  
      {icon: mdiCopyright, to: 'https://creativecommons.org/licenses/by/4.0/', target: '_blank', alt: 'License'}  
    ],
  })
}  
</script> 