/** @type {import("snowpack").SnowpackUserConfig } */

console.log('Build Environment: ', process.env.NODE_ENV )
const isProdution = process.env.NODE_ENV === "production"
process.env.SNOWPACK_PUBLIC_JS_FILE = isProdution ? "goban.min.js" : "goban.js"
process.env.SNOWPACK_PUBLIC_CABLE_URL = isProdution ? "wss://shoutan.herokuapp.com/cable/" : "ws://localhost:3000/cable/"

module.exports = {
  mount: {
    public: '/',
    src: '/js/',

  },
  plugins: [
    "snowpack-plugin-elm"
  ],
  install: [
    /* ... */
  ],
  installOptions: {
    /* ... */
  },
  devOptions: {
    hostname: "devd.io",
    port: 8000
  },
  buildOptions: {
    clean: true,
    out: process.env.NODE_ENV === "production" ? "dist" : "build-snow",
    webModulesUrl: 'web',
    metaDir: 'static'
  },
};
