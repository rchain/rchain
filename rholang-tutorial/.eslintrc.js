module.exports = {
  extends: [
    'standard-react'
  ],
  parser: 'babel-eslint',
  env: {
    'es6': true,
    'browser': true
  },
  rules: {
    'react/jsx-tag-spacing': 'off',
    'react/forbid-prop-types': 'off',
    'max-len': ['warn', {'code': 100}]
  }
}
