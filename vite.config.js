import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";


const isProdution = process.env.NODE_ENV === 'production';

export default defineConfig({
  plugins: [
    elmPlugin(),
  ],
  define: {
    // SNOWPACK_PUBLIC_CABLE_URL: isProdution
    //   ? 'wss://shoutan.herokuapp.com/cable/'
    //   : 'ws://localhost:3000/cable/',

  },
  root: "src",
});

