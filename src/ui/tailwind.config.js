const defaultTheme = require('tailwindcss/defaultTheme');

const eucalyptus = {
  '50': '#effaf4',
  '100': '#d9f2e3',
  '200': '#b5e5ca',
  '300': '#85d0ab',
  '400': '#52b588',
  '500': '#30996c',
  '600': '#207955',
  '700': '#1a6247',
  '800': '#174e3a',
  '900': '#144031',
};

/** @type {import('tailwindcss').Config} */
module.exports = {
  mode: 'jit',
  content: ['./src/**/*.html', './src/**/*.tsx'],
  theme: {
    extend: {
      fontFamily: {
        sans: ['Lato', ...defaultTheme.fontFamily.sans],
      },
      colors: {
        eucalyptus: {
          ...eucalyptus
        }
      }
    },
  },
  plugins: [],
}
