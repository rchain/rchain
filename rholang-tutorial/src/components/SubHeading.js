import React from 'react'
import PropTypes from 'prop-types'

export default class SubHeading extends React.PureComponent {
  static propTypes = {
    level: PropTypes.number,
    children: PropTypes.node
  }

  render () {
    const {id, level, children} = this.props
    const Element = 'h' + level

    return (
      <div>
        <div style={{position: 'absolute', marginTop: -96}}>
          <a id={id} name={id}/>
        </div>
        <Element>
          {children}
          {level > 1 && (<a href={'#' + id} className="anchor">#</a>)}
        </Element>
      </div>
    )
  }
}
