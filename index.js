import { Elm } from './src/Main.elm'

import './styles'

Elm.Main.init({
  node: document.querySelector('main'),
  flags: process.env.API_TOKEN,
})
