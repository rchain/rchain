import axios from 'axios'

const branch = 'dev'
const path = 'docs/rholang/rholangtut-0.3.md'

const sourceUrl = 'https://raw.githubusercontent.com/rchain/rchain/' + branch + '/' + path
const editUrl = 'https://github.com/rchain/rchain/blob/' + branch + '/' + path

// noinspection JSUnusedGlobalSymbols
export default {
  getSiteData: () => ({
    title: 'Rholang Tutorial',
    sourceUrl,
    editUrl
  }),
  getRoutes: async () => {
    const {data: content} = await axios.get(sourceUrl)

    return [
      {
        path: '/',
        component: 'src/containers/Home',
        getData: () => ({content})
      }
    ]
  }
}
