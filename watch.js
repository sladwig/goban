var watch = require('watch');
var execshell = require('exec-sh').promise;

const ignoredFiles = new Set();

const ignore = (f) => ignoredFiles.add(f);
const isIgnored = (f) => ignoredFiles.has(f);
const unignore = (f) => ignoredFiles.delete(f);

const cmd = async (...args) => {
  let command = args.join(' ');
  try {
    await execshell(command);
  } catch (e) {
    console.error('Error executing:', command);
  }
};

watch.createMonitor(
  './src',
  {
    ignoreDotFiles: true,
    filter: (path) => path.endsWith('.elm'),
    interval: 0.05,
  },
  (monitor) => {
    console.log('watching Elm Files');

    monitor.on('changed', async function (f, curr, prev) {
      if (isIgnored(f)) return unignore(f);

      ignore(f);
      await cmd('elm-format', f, '--yes');
      await cmd('elm', 'make', '--output=src/elm.js', 'src/Main.elm');
      await cmd('./elm-module.sh');
    });
  },
);

// cmd('elm-test --watch');
