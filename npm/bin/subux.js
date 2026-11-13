#!/usr/bin/env node
'use strict'

// Run the Subux executable built for this platform. Each directory under
// platforms/ is a release archive as published: the executable and the design
// tokens it reads when a site has none of its own.

const { spawnSync } = require('node:child_process')
const path = require('node:path')

const PLATFORMS = {
  'darwin/arm64': 'darwin-arm64',
  'darwin/x64': 'darwin-x86_64',
  'linux/arm64': 'linux-arm64',
  'linux/x64': 'linux-x86_64',
}

const host = `${process.platform}/${process.arch}`
const platform = PLATFORMS[host]
if (platform === undefined) {
  console.error(
    `subux: no executable for ${host}. Build one from source: https://github.com/Palbahngmiyine/subux`,
  )
  process.exit(1)
}

const executable = path.join(__dirname, '..', 'platforms', platform, 'subux')
const result = spawnSync(executable, process.argv.slice(2), { stdio: 'inherit' })
if (result.error) {
  console.error(`subux: cannot run ${executable}: ${result.error.message}`)
  process.exit(1)
}
// A null status means a signal ended the executable.
process.exit(result.status ?? 1)
