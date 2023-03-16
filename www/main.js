import { WASI } from "@bjorn3/browser_wasi_shim/src";

const wasi = new WASI([], [], []);

const wasiImportObj = { wasi_snapshot_preview1: wasi.wasiImport };
const wasm = await WebAssembly.instantiateStreaming(fetch("experiment.wasm"), wasiImportObj);
wasi.inst = wasm.instance;
const exports = wasm.instance.exports;
const memory = exports.memory;
const encoder = new TextEncoder();
const decoder = new TextDecoder();


exports._initialize();
exports.hs_init(0, 0);

const canvas = document.getElementById("canvas");
console.log(canvas);

const width = 20;
const height = 20;

const ctx = canvas.getContext("2d");

const cellSize = 20;

canvas.width = width * cellSize;
canvas.height = height * cellSize;

async function update() {


    if (!window.direction) {
        window.direction = 's';
    }


    const inputLen = Buffer.byteLength(window.direction);
    const inputPtr = exports.malloc(inputLen);
    const inputArr = new Uint8Array(memory.buffer, inputPtr, inputLen);
    encoder.encodeInto(window.direction, inputArr);
    const outputPtrPtr = exports.mallocPtr();
    const outputLen = exports.updateGameState(inputPtr, inputLen, outputPtrPtr);
    const outputPtrArr = new Uint32Array(memory.buffer, outputPtrPtr, 1);
    const outputPtr = outputPtrArr[0];
    const outputArr = new Uint8Array(memory.buffer, outputPtr, outputLen);
    const output = decoder.decode(outputArr);



    exports.freePtr(inputPtr);
    exports.freePtr(outputPtr);
    exports.freePtr(outputPtrPtr);

    console.log(exports) 



    draw(output);
    // delay
    await new Promise(resolve => setTimeout(resolve, 150));


    if (window.isRunning) {
        window.requestAnimationFrame(update);
    }
}



function draw(gameStateStr) {
ctx.clearRect(0, 0, canvas.width, canvas.height);

const rows = gameStateStr.trim().split('\n');
console.log(rows)


for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
    const c = rows[y][x];
    if (c === '*') {
        ctx.fillStyle = 'black';
        ctx.fillRect(x * cellSize, y * cellSize, cellSize, cellSize);
    } else if (c === '@') {
        ctx.fillStyle = 'red';
        ctx.fillRect(x * cellSize, y * cellSize, cellSize, cellSize);
    }

    }
}
}

document.addEventListener("keydown", event => {
    window.direction = event.key;
    console.log(event.key);
});

window.start = () => {
    window.isRunning = true;
    console.log('start');
    console.log(window);
    update();
};

window.stop = () => {
    window.isRunning = false;
};

function init() {
    window.start();
}

init();

