import { Elm } from './Main.elm'

const app = Elm.Main.init({
  node: document.getElementById('elm-root'),
})

window.addEventListener('scroll', () => {
  app.ports.onWindowScroll.send(null)
})
