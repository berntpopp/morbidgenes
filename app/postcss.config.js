// postcss.config.js
// based on https://nofluffweb.com/purgecss-usage-to-remove-unused-css-from-bundle
// and https://github.com/nocookie-analytics/core/blob/2dc44215ee8335ce8cde790cad8e0c24019e3658/frontend/purgecss.conf.js
// and https://gaganpreet.in/posts/vue-vuetify-performance/

const IN_PRODUCTION = process.env.NODE_ENV === 'production';

module.exports = {
  plugins: [
    IN_PRODUCTION
      && require('@fullhuman/postcss-purgecss')({
        content: [
          './public/**/*.html',
          './src/**/*.vue'
        ],
        defaultExtractor(content) {
          const contentWithoutStyleBlocks = content.replace(
            /<style[^]+?<\/style>/gi,
            '',
          );
          return (
            contentWithoutStyleBlocks.match(
              /[A-Za-z0-9-_/:]*[A-Za-z0-9-_/]+/g,
            ) || []
          );
        },
        keyframes: true,
        variables: true,
        safelist: {
          standard: [
            /col-.*/,
            /row-.*/,
            'v-application',
            'v-application--wrap',
            'spacer',
            'button',
            'text-start',
            'sortable',
          ],
          deep: [
            /-(leave|enter|appear)(|-(to|from|active))$/,
            /^(?!(|.*?:)cursor-move).+-move$/,
            /^router-link(|-exact)-active$/,
            /data-v-.*/,
            /^layout.*/,
            /^container.*/,
            /^v-((?!application).)*$/,
            /^theme--light.*/,
            ///.*-transition/,
            /^justify-.*/,
            /^p-[0-9]/,
            /^m-[0-9]/,
            ///^text--*/,
            ///--text$/,
            /^row-.*/,
            ///^col-*/,
            ///^v-row-*/,
            ///^v-col-*/,
          ],
          greedy: [
            /v-list-item/,
            /v-label/,
            /v-text-field/,
            /v-input/,
            /v-divider/,
            /v-card/,
            /v-expansion/,
            /v-chip/,
            /flex/,
            /mdi/,
            /deep-/,
            /elevation/,
            /row/,
            /col/,
            /red/,
            /orange/,
            /green/,
            /teal/,
            /white/,
            /blue/,
          ],
        },
      }),
  ],
};