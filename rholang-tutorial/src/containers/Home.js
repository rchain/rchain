import React from 'react'
import { withRouteData } from 'react-static'

import Markdown from '../components/Markdown'

export default withRouteData(({content, onTOC = () => {}}) => (
  <Markdown onTOC={(toc) => onTOC(toc)} source={content}/>
))
