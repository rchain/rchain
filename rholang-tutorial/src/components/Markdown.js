import React from 'react'
import ReactHtmlParser, { convertNodeToElement, processNodes } from 'react-html-parser'
import generatePropsFromAttributes from 'react-html-parser/lib/utils/generatePropsFromAttributes'
import compiler from 'react-smackdown/lib/compiler'

import Code from './Code'
import SubHeading from './SubHeading'

const renderers = {
  'h2': (props) => (<SubHeading level={2} {...props}/>),
  'h3': (props) => (<SubHeading level={3} {...props}/>)
}

let setToc = false

export default function Markdown ({source, onTOC = () => {}}) {
  let toc = []
  const transform = (node, index) => {
    // Transform any component renderer as react component
    if (node.type === 'tag' && renderers[node.name]) {
      const Component = renderers[node.name]
      const props = generatePropsFromAttributes(node.attribs, index)
      const children = processNodes(node.children, transform)

      const flattenedChildren = node.children && node.children[0] && node.children[0].data
        ? node.children[0].data : null

      if(!setToc) {
        toc.push({type: node.name, id: props.id, value: flattenedChildren})
      }

      return React.createElement(Component, props, children)
    }

    // Transform any code examples
    if (node.type === 'tag' && node.name === 'pre') {
      if (node.children && node.children.length === 1 && node.children[0].name === 'code') {
        node = node.children[0]
        convertNodeToElement(node, index, transform)
        const props = {
          ...generatePropsFromAttributes(node.attribs, index),
          isInPre: node.parent && node.parent.name === 'pre'
        }
        const children = node.children && node.children[0] && node.children[0].data
          ? node.children[0].data : null

        return React.createElement(Code, props, children)
      }
    }
  }

  const component = ReactHtmlParser(compiler.makeHtml(source), {transform})

  if (onTOC && !setToc) {
    setToc = true
    setTimeout(() => {
      onTOC(toc)
    }, 10)
  }

  return component
}
