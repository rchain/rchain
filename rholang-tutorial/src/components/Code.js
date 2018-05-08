import React from 'react'
import PropTypes from 'prop-types'
import { UnControlled as CodeMirror } from 'react-codemirror2'
import rholang from 'codemirror-rholang'

import RunButton from './RunButton'

export default class Code extends React.PureComponent {
  static propTypes = {
    children: PropTypes.string
  }

  constructor (props) {
    super(props)

    // Remove line numbers in front of some of the code examples
    const regex = /^\s?\d+\s/gm
    let value = props.children
    const hasLineNumbers = value.match(regex)
    if (hasLineNumbers) {
      value = props.children.replace(regex, '')
    }

    this.state = {
      originalValue: value,
      hasLineNumbers,
      value
    }
  }

  render () {
    const {hasLineNumbers, originalValue, value} = this.state

    return (
      <div className='code-editor'>
        <CodeMirror
          value={originalValue}
          defineMode={{name: 'rholang', fn: rholang}}
          options={{
            lineNumbers: hasLineNumbers,
            lineWrapping: true,
            tabSize: 2,
            mode: 'rholang',
            viewportMargin: Infinity,
            theme: 'solarized'
          }}
          onChange={(editor, data, value) => {
            this.setState({value})
          }}
        />
        {hasLineNumbers && <RunButton value={value}/>}
      </div>
    )
  }
}
