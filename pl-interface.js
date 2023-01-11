'use strict';

let prologSession;
let prologInitialpuzzle; // since the frontend representation is slightly different, we retain our own copy

async function initBackend() {
    // see https://www.swi-prolog.org/pldoc/man?section=wasm-loading
    const Module = {
        arguments: ["-q"],
        locateFile: (file) => 'swipl/' + file
    };
    await SWIPL(Module);
    prologSession = Module.prolog;
    // Many servers refuse to dispense .pl directly!
    // so make sure this is allowed by some config
    await prologSession.consult("sudoku.pl");

    prologSession.query("set_square_block_mask").once();
}



async function makePuzzle(difficulty) {
    let result;

    // retry puzzle creation, as it can fail
    do {
        result = await prologSession.forEach("make_puzzle(Difficulty, Puzzle, FullSolution)", { Difficulty: difficulty });
    } while (result.length === 0);
    prologInitialpuzzle = result[0].Puzzle;

    // replace the empty position with null
    const nulledPuzzle = prologInitialpuzzle.grid.map(x => x == 'empty_position' ? null : x);
    return { initialPuzzleArray: nulledPuzzle, completedArray: result[0].FullSolution.grid };

}

function checkPlausible(x, y, value) {
    return prologSession.query("propose_value(Grid, X, Y, Val)", { Grid: prologInitialpuzzle, X: x, Y: y, Val: value })
        .once().success;

}

async function blockMaskBorder(x, y) {
    if (!prologSession) return [];
    return await prologSession.forEach("block_adjacent(X, Y, Direction)", { X: x, Y: y });
    
    
        

}