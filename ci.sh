#!/usr/bin/env bash
set -euo pipefail

echo "Running test suite..."
nim c -r -f --hints:off test
echo "Tests passed."

echo "Generating documentation..."
nim doc --hints:off --outdir:./docs signals.nim
echo "Docs generated in ./docs"

cat > ./docs/index.html <<'HTML'
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta http-equiv="refresh" content="0; url=signals.html" />
    <title>Redirecting…</title>
  </head>
  <body>
    <p>If you are not redirected automatically, follow this <a href="signals.html">link</a>.</p>
  </body>
</html>
HTML

echo "Added docs/index.html redirect."
