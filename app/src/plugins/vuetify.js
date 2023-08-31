import Vue from 'vue'
import Vuetify from 'vuetify/lib/framework'

Vue.use(Vuetify)

export default new Vuetify({
    icons: {
      iconfont: 'mdiSvg', // default - only for display purposes
    },
    // based on https://v2.vuetifyjs.com/en/features/theme/#custom-properties
    theme: {
      options: { cspNonce: 'dQw4w9WgXcQ' },
    },
  })
