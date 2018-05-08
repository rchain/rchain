import React from 'react'
import { Router } from 'react-static'
import { hot } from 'react-hot-loader'
import 'codemirror/lib/codemirror.css'

import Header from './components/Header'

import './styles/app.css'
import './styles/editor.css'
import Home from './containers/Home'

class App extends React.PureComponent {
  constructor (props) {
    super(props)
    this.state = {
      tocOpen: false,
      toc: []
    }
  }

  render () {
    const {toc, tocOpen} = this.state

    return (
      <Router>
        <div className={(tocOpen ? 'toc-open' : '')}>
          <nav className='toc'>
            <h4>Table of contents</h4>
            <div>
              {toc.map(node => (
                <a
                  key={node.id}
                  className={node.type}
                  href={'#' + node.id}>
                  {node.value}
                </a>
              ))}
            </div>
          </nav>
          <main>
            <Header onToggleTOC={() => this.setState({tocOpen: !tocOpen})}/>
            <div className='content'>
              <Home onTOC={(toc) => this.setState({toc})}/>
            </div>
          </main>
        </div>
      </Router>
    )
  }
}

export default hot(module)(App)
