import React from 'react'
import PropTypes from 'prop-types'
import { withSiteData } from 'react-static'
import MaterialIcon from 'material-icons-react'

class Header extends React.PureComponent {
  static propTypes = {
    onToggleTOC: PropTypes.func
  }

  render () {
    return (
      <header>
        <a className="expand-toc" onClick={() => this.props.onToggleTOC()}>
          <MaterialIcon icon="menu"/>
        </a>
        <a className="edit" target="_blank" href={this.props.editUrl}>
          <MaterialIcon size={16} icon="settings"/>
          suggest a change
        </a>
      </header>
    )
  }
}

export default withSiteData(Header)
