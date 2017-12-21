import 'rollup-watch';
import purs from 'rollup-plugin-purs';

export default {
  input: "src/Api.purs",
  output: {
    file: "build/bundle.js",
    format: "cjs",
    sourcemap: true,
  },
  plugins: [
    purs({
      runMain: false,
      debug: true
    }),
  ],
  watch: {
    include: ['output/Api/*'],
    clearScreen: false
  },
  onwarn: (warning) => console.warn(warning.message)
};