const tw = require('tailwindcss')

module.exports = {
  future: {
    removeDeprecatedGapUtilities: true,
    purgeLayersByDefault: true,
  },
  purge: [],
  theme: {
    extend: {},
  },
  variants: {
    display: ['group-hover', ...tw.variants.display],
  },
  plugins: [],
}
