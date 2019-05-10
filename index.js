import './styles'
import { Elm } from './src/Main.elm'

const getRandomInts = n => {
  const randInts = new Uint32Array(n)
  crypto.getRandomValues(randInts)
  return Array.from(randInts)
}

const [seed, ...seedExtension] = getRandomInts(5)

Elm.Main.init({
  node: document.querySelector('main'),
  flags: [process.env.API_TOKEN, seed, seedExtension],
})
