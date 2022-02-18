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
  
            <v-overlay
              :absolute="absolute"
              :opacity="opacity"
              :value="loading"
              :color="color"
            >
              <v-progress-circular
                indeterminate
                color="primary"
              ></v-progress-circular>
            </v-overlay>

            <div class="pa-2">
                <v-text-field
                  v-model="search"
                  append-icon="mdi-magnify"
                  label="Search"
                  single-line
                  hide-details
                ></v-text-field>

                <v-data-table
                  dense
                  :items="variants"
                  :headers="headers"
                  :search="search"
                  item-key="name"
                  class="elevation-1"
                ></v-data-table>
            </div>
            </v-sheet>
          </v-col>

        </v-row>
      </v-container>
</template>


<script>
export default {
  name: 'Tables',
  data() {
        return {
          variants: [],
          headers:[
            { text:'Panel', value: 'panel_version' },
            { text:"HGNC ID", value:"hgnc_id" },
            { text:"Symbol", value:"symbol" },
            { text:"MG Score", value:"mg_score" },
          ],
          search: '',
          totalRows: 1,
          absolute: true,
          opacity: 1,
          color: "#FFFFFF",
          loading: true
        }
      },
      computed: {
      },
      mounted() {
        this.loadPanelData();
      },
      methods: {
        async loadPanelData() {
          this.loading = true;
          let apiUrl = process.env.VUE_APP_API_URL + '/api/panel/';
          console.log(apiUrl);
          try {
            let response = await this.axios.get(apiUrl);
            this.variants = response.data.data;
            this.totalRows = response.data.data.length;
          } catch (e) {
            console.error(e);
          }
          this.loading = false;
        }
      }
  }
</script>