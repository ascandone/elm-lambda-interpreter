const config = require('tailwindcss/defaultConfig')

module.exports = {
  future: {
    removeDeprecatedGapUtilities: true,
    purgeLayersByDefault: true,
  },
  purge: ['src/index.html', 'src/Main.elm'],
  theme: {
    fontFamily: {
      sans: ['Inter', config.theme.fontFamily.sans],
      mono: ['Roboto mono', ...config.theme.fontFamily.mono],
      serif: config.theme.fontFamily.serif,
    },
    extend: {},
  },
  variants: {
    textColor: ['group-hover', ...config.variants.textColor],
  },
  plugins: [
    //
    require('@tailwindcss/ui/'),
  ],
}
