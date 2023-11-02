/** @type {import("snowpack").SnowpackUserConfig } */

console.log('Build Environment: ', process.env.NODE_ENV);
const isProdution = process.env.NODE_ENV === 'production';
process.env.SNOWPACK_PUBLIC_JS_FILE = isProdution ? 'goban.min.js' : 'goban.js';
process.env.SNOWPACK_PUBLIC_CABLE_URL = isProdution
  ? 'wss://shoutan.herokuapp.com/cable/'
  : 'ws://localhost:3000/cable/';

export default {
  installOptions: {
    dest: 'web_modules',
    clean: true,
    env: {},
  },
  mount: {
    src: '/',
  },
  devOptions: {
    hostname: 'devd.io',
    port: 8000,
    open: 'none',
    output: 'stream',
    hmrErrorOverlay: false,
  },
  buildOptions: {
    clean: true,
    out: 'dist',
    webModulesUrl: 'web',
    metaDir: 'static',
  },
  plugins: [
    [
      '@snowpack/plugin-run-script',
      {
        name: 'elm watch',
        cmd: 'elm-format src/ --yes',
        watch: 'npx node watch.js',
        output: 'stream',
      },
    ],
  ],
};
