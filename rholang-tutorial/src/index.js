import React from 'react'
import ReactDOM from 'react-dom'
import App from './App'

// Export top level component as JSX (for static rendering)
export default App

// Render the app
if (typeof document !== 'undefined') {
  const renderMethod = module.hot ? ReactDOM.render : ReactDOM.hydrate || ReactDOM.render
  const render = Comp => {
    renderMethod(<Comp/>, document.getElementById('root'))
  }
  render(App)
}
