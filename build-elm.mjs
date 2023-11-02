require('zx/globals')

// clean dist

await $`rm -rf dist` // src/web_modules src/elm.js`


// make new dist
await $`mkdir -p dist`

// create elm module
await Promise.all(
  [
    await $`./elm-module.sh src/Main.elm`,  # prod
    await $`elm make src/Main.elm --output=dist/app.js`, # dev

  ]
)

var files = await glob('src/*.js')

for (const file of files) {
  const name = file.split(/(\\|\/)/g).pop()

  await $`bun terser ${file} --output dist/${name}`
  console.log(file, 'processed')
}

await $`bun snowpack build`
