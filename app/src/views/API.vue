<template>
  <div class="container-fluid text-left py-2">
    <div
      id="swagger"
      class="swagger"
    />
  </div>
</template>

<script>
  import SwaggerUI from 'swagger-ui'
  import 'swagger-ui/dist/swagger-ui.css'

  export default {
    name: 'API',
    metaInfo: {
      // if no subcomponents specify a metaInfo.title, this title will be used
      title: 'API documentation',
      // all titles will be injected into this template
      titleTemplate:
        '%s | HNF1B-db - The curated database for the HNF1B gene and associated diseases',
      htmlAttrs: {
        lang: 'en',
      },
      meta: [
        {
          vmid: 'description',
          name: 'description',
          content: 'This is the Swagger / OpenAPI documentation HNF1B-db.',
        },
      ],
    },
    mounted () {
      this.loadAPIInfo()
    },
    methods: {
      async loadAPIInfo () {
        const apiURL = process.env.VUE_APP_API_URL + '/openapi.json'
        SwaggerUI({
          dom_id: '#swagger',
          url: apiURL,
          docExpansion: 'none',
          displayRequestDuration: true,
          syntaxHighlight: { activate: true, theme: 'obsidian' },
        })
      },
    },
  }
</script>

<style scoped>
:deep(.info) {
  background-color: #ffffff !important;
  color: #000 !important;
}
</style>
