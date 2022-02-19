<template>
<div class="iframe-container">
    <iframe :src="source" allowfullscreen></iframe>
</div>
</template>

<script>
export default {
  name: "Swagger",
  data() {
        return {
          source: 'http://127.0.0.1:10918/__docs__/',
        }
  }
}
</script>

<style scoped>
.iframe-container {
  overflow: hidden;
  padding-top: 56.25%; /* 16:9*/
  position: relative;
}

.iframe-container iframe {
   border: 0;
   height: 100%;
   left: 0;
   position: absolute;
   top: 0;
   width: 100%;
}
</style>