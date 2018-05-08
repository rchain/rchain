import React from 'react'
import PropTypes from 'prop-types'

import MaterialIcon from 'material-icons-react'

export default class RunButton extends React.PureComponent {
  static propTypes = {
    value: PropTypes.string,
    title: PropTypes.string
  }

  static defaultProps = {
    title: 'Run example'
  }

  render () {
    return (
      <div>
        <a onClick={() => this.form.submit()} className='code-editor-run'>
          <MaterialIcon icon='build' size={16} color='#fff'/>
          {this.props.title}
        </a>
        <form ref={(form) => {this.form = form}} target='_blank' method='POST' action='http://rchain.cloud/'>
          <input type='hidden' name='body' value={this.props.value}/>
        </form>
      </div>
    )
  }
}
