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
          <v-icon size="24px">
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
        {icon: 'mdi-github', to: 'https://github.com/berntpopp/morbidgenes', target: '_blank'},
        {icon: 'mdi-api', to: '/API', target: '_self'},
        {icon: 'mdi-copyright', to: 'https://creativecommons.org/licenses/by/4.0/', target: '_blank'}
      ],
    }),
  }
</script>