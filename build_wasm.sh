#!/bin/bash

# Do regular build. The final compilated can porbably be excluded from this.
./build.sh

# WASM build
emcc *.c --no-entry -s TOTAL_MEMORY=33685504 -o engine.mjs \
    -s EXPORTED_FUNCTIONS='["_WASM_init", "_WASM_generate_move", "_WASM_import_game_state", "_WASM_make_move"]' \
    -s EXPORTED_RUNTIME_METHODS='["ccall", "cwrap"]' \
    --pre-js wasm/locateFile.js  \
    -s ENVIRONMENT='web'  \
    -s EXPORT_NAME='createModule'  \
    -s USE_ES6_IMPORT_META=0  \
    -O3 \

sed -i 's/export\ default\ createModule//g' engine.mjs
cat wasm/template.js >> engine.mjs
