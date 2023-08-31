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
            />
          </v-overlay>

          <div class="text-lg-h6 pa-2">
            MorbidGenes panel: a monthly updated  list of diagnostically relevant genes derived from diverse sources
          </div>

          <div class="text-lg-h6 pa-2">
            <v-btn
              block
              size="sm"
              @click="requestExcel"
            >
              <v-icon v-if="!downloading">
                {{ icons.mdiTableLarge }}
              </v-icon>
              <v-icon v-if="!downloading">
                {{ icons.mdiCloudDownload }}
              </v-icon>
              <v-progress-circular
                v-if="downloading"
                indeterminate
                color="primary"
                size="25"
              />
              .xlsx
            </v-btn>
          </div>

          <div class="pa-2">
            <v-text-field
              v-model="search"
              :append-icon="icons.mdiMagnify"
              label="Search"
              single-line
              hide-details
            />

            <v-data-table
              dense
              :items="panel"
              :headers="headers"
              :search="search"
              item-key="name"
              class="elevation-1"
            >
              <template #[`item.mg_score`]="{ item }">
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
  import { mdiTableLarge, mdiCloudDownload, mdiMagnify } from '@mdi/js'

  // Importing URLs from a constants file to avoid hardcoding them in this component
  import URLS from '@/assets/js/constants/url_constants'

  export default {
    name: 'Tables',
    data: () => ({
      icons: {
        mdiTableLarge,
        mdiCloudDownload,
        mdiMagnify,
      },
      panel: [],
      headers: [
        { text: 'Panel', value: 'panel_version' },
        { text: 'HGNC ID', value: 'hgnc_id' },
        { text: 'Symbol', value: 'symbol' },
        { text: 'MG Score', value: 'mg_score' },
      ],
      search: '',
      totalRows: 1,
      sort: '',
      filter_string: '',
      absolute: true,
      opacity: 1,
      color: '#FFFFFF',
      loading: true,
      downloading: false,
    }),
    computed: {
    },
    mounted () {
      this.loadPanelData()
    },
    methods: {
      async loadPanelData () {
        this.loading = true
        const apiUrl = URLS.API_URL + '/api/panel'
        try {
          const response = await this.axios.get(apiUrl)
          this.panel = response.data.data
          this.totalRows = response.data.data.length
        } catch (e) {
          console.error(e)
        }
        this.loading = false
      },
      async requestExcel () {
        this.downloading = true

        const urlParam = `sort=${
          this.sort
        }&filter=${
          this.filter_string
        }&page_after=` +
          '0' +
          '&page_size=' +
          'all' +
          '&format=xlsx'

        const apiUrl = `${URLS.API_URL
        }/api/panel?${
          urlParam}`

        try {
          const response = await this.axios({
            url: apiUrl,
            method: 'GET',
            responseType: 'blob',
          })

          const fileURL = window.URL.createObjectURL(new Blob([response.data]))
          const fileLink = document.createElement('a')

          fileLink.href = fileURL
          fileLink.setAttribute('download', 'morbidgenes_panel.xlsx')
          document.body.appendChild(fileLink)

          fileLink.click()
        } catch (e) {
          console.log(e, 'Error', 'danger')
        }

        this.downloading = false
      },
      getColor (calories) {
        if (calories < 3) return 'red'
        else if (calories < 5) return 'orange'
        else return 'green'
      },
    },
  }
</script>
