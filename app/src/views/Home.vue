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

            <div class="text-lg-h6 pa-2">
            MorbidGenes panel: a monthly updated  list of diagnostically relevant genes derived from diverse sources 
            </div>

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
                  :items="panel"
                  :headers="headers"
                  :search="search"
                  item-key="name"
                  class="elevation-1"
                >
                
                <template v-slot:[`item.mg_score`]="{ item }">
                  <v-chip
                    :color="getColor(item.mg_score)"
                    dark
                    x-small
                  >
                    {{ item.mg_score }}
                  </v-chip>
                </template>

                </v-data-table>
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
          panel: [],
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
          try {
            let response = await this.axios.get(apiUrl);
            this.panel = response.data.data;
            this.totalRows = response.data.data.length;
          } catch (e) {
            console.error(e);
          }
          this.loading = false;
        },
        getColor (calories) {
        if (calories < 3) return 'red'
        else if (calories < 5) return 'orange'
        else return 'green'
      },
      }
  }
</script>