let makeMoveInEngine = null;
let generateEngineMove = null;

createModule().then((module) => {
  module.ccall("WASM_init", null, null, null);

  generateEngineMove = module.cwrap("WASM_generate_move", "string", []);

  makeMoveInEngine = module.cwrap("WASM_make_move", null, ["string"]);

  console.log("Engine loaded");
});

onmessage = (e) => {
  if (makeMoveInEngine === null || generateEngineMove === null) return;

  if (e.data.type !== "genMove") return;

  if (e.data.move) {
    makeMoveInEngine(e.data.move);
  }

  const engineMove = generateEngineMove();
  postMessage(engineMove);
};
;