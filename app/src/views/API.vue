<template>
  <div>
    <iframe :src="source"></iframe>
  </div>
</template>

<script>
export default {
  name: "Swagger",
  data() {
        return {
          source: process.env.VUE_APP_API_URL + '/__docs__/',
        }
  }
}
</script>