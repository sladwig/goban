/** @type {import("snowpack").SnowpackUserConfig } */
module.exports = {
  mount: {
    build: '/',
    "src/js": '/_dist_/'
  },
  plugins: [
    "@snowpack/plugin-dotenv"
  ],
  install: [
    /* ... */
  ],
  installOptions: {
    /* ... */
  },
  devOptions: {
    /* ... */
  },
  buildOptions: {
    out: "build-snow"
    /* ... */
  },
  proxy: {
    /* ... */
  },
  alias: {
    /* ... */
  },
};
