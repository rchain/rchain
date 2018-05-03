import axios from 'axios'

const sourceUrl = 'https://raw.githubusercontent.com/rchain/rchain/master/docs/rholang/rholangtut-0.2.md'
const editUrl = 'https://github.com/rchain/rchain/blob/master/docs/rholang/rholangtut-0.2.md'

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
