import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';

export default {
  input: 'lib/index.js',
  output: [
    {
      file: 'dist/index.cjs.js',
      format: 'cjs',
      name: 'Verifier'
    },
    {
      file: 'dist/index.es.js',
      format: 'es',
      name: 'Verifier'
    }
  ],
  plugins: [
    resolve({
      browser: true,
      mainFields: ['module', 'import', 'main', 'exports'],
      preferBuiltins: true,
      extensions: ['.js', '.json']
    }),
    commonjs()
  ]
};
