'use strict';

// Me: i don't need to spin up React, this will just be a small project
// 300 lines later ... well shit. lol


// an object containing the button in the puzzle grid that was clicked
let buttonClicked;

// an array with the current filled in values, incluing base values and user filled in
let userArray;

// the completed destination array with all values
let completedArray;

// the initial filled in values, .e.g initial puzzle state
let initialPuzzleArray;

// the start time when the last tick occured
let startTime;

// because we will allow pauses, we want to track total time seperately
let totalTime;

// the interval object used to tick the timer
let timerTickInterval;

// current difficulty
let difficulty;

function initGrid(paused) {
    const container = document.getElementById('container');
    // REMOVE current grid entirely
    container.innerText = '';

    // create blocks (3x3 super blocks)
    for (var by = 0; by < 3; by++) {
        for (var bx = 0; bx < 3; bx++) {
            const bdiv = document.createElement('div');
            bdiv.classList.add('block');
            //create individual cells (3x3) within each block
            for (var y = 0; y < 3; y++) {
                for (var x = 0; x < 3; x++) {
                    const idiv = document.createElement('div');
                    const gridy = by * 3 + y;
                    const gridx = bx * 3 + x;
                    // in the actual array, it is a linear 81 element array
                    // calculate the index into that array
                    const nid = gridy * 9 + gridx;

                    // don't show anything if paused
                    if (paused) {
                        idiv.innerText = '?';

                    } else {
                        switch (initialPuzzleArray[nid]) {
                            case undefined: // array is not loaded!
                                idiv.innerText = '?';
                                break;
                            case null:
                                // this is a user space - the user must fill in a value

                                // make button <button class="empty"></button>
                                const btn = document.createElement('button');
                                btn.dataset.x = gridx;
                                btn.dataset.y = gridy;
                                btn.dataset.id = nid;
                                if (userArray[nid]) { // has the user already filed in a value?
                                    btn.className = 'user';
                                    btn.innerText = userArray[nid];
                                } else {
                                    btn.className = 'empty';
                                }
                                btn.addEventListener('click', gridBtnClick);
                                idiv.append(btn);
                                break;
                            default:
                                // this is a fixed value provided by the puzzle and cannot be changed
                                idiv.innerText = initialPuzzleArray[nid];
                                break;

                        }
                    }
                    bdiv.append(idiv);
                }
            }
            container.append(bdiv);
        }
    }
}

function assignButtonValue(value) {
    buttonClicked.className = value ? 'user' : 'empty';
    buttonClicked.innerText = value ? value : '';
    userArray[buttonClicked.dataset.id] = Number(value);
    checkCompletion();

}

function checkCompletion() {
    const matches = userArray.length === completedArray.length && userArray.every((value, index) => value === completedArray[index]);
    if (matches) {
        // they have completed the puzzle succesfully!
        document.querySelectorAll('#container button').forEach(b => {
            b.disabled = true;
            b.className = 'success';


            setGameSuspended(true);
            document.getElementById('resumeButton').disabled = true;

        });
    }

}

function gridBtnClick(e) {

    buttonClicked = e.target;
    document.querySelectorAll('#chooseNumberDialog .block button').forEach(b => {
        // on "easy" mode, we oly allow plausible numbers from initial puzzle state
        // if we did from current puzzle state, we might prevent correct answers!
        if (difficulty === 'easy') {
            const proposed = Number(b.value);
            const x = Number(buttonClicked.dataset.x);
            const y = Number(buttonClicked.dataset.y);
            b.disabled = !checkPlausible(x, y, proposed);

        } else {
            b.disabled = false;
        }
        b.className = '';
    });

    if (buttonClicked.innerText) {
        document.getElementById(`chooseNumberButton-${buttonClicked.innerText}`)
            .className = 'user';
    }

    document.getElementById('chooseNumberDialog').showModal();
}

function chooserBtnClick(e) {
    switch (e.target.value) {
        case 'cancel':
            break; // do nothing, retain current 
        case 'clear':
            assignButtonValue(null);
            break;
        default:
            assignButtonValue(e.target.value);

    }
}

function timerTick() {
    // THIS SUCKS!!
    const ndt = new Date().getTime();
    let elapsedMilliseconds = ndt - startTime;
    startTime = ndt;
    totalTime += elapsedMilliseconds;

    let elapsedSeconds = totalTime / 1000;
    const hours = Math.floor(elapsedSeconds / (60 * 60));
    elapsedSeconds -= hours * 60 * 60;
    const minutes = Math.floor(elapsedSeconds / 60);
    elapsedSeconds -= minutes * 60;
    const seconds = Math.floor(elapsedSeconds);
    const phours = hours < 10 ? `0${hours}` : hours;
    const pminutes = minutes < 10 ? `0${minutes}` : minutes;
    const pseconds = seconds < 10 ? `0${seconds}` : seconds;

    const printed = `${phours}:${pminutes}:${pseconds}`;
    document.getElementById('timer').innerText = printed;
}

function setGameSuspended(suspended) {
    if (suspended) {
        if (timerTickInterval)
            clearInterval(timerTickInterval);
        const ndt = new Date().getTime();
        let elapsedMilliseconds = ndt - startTime;
        totalTime += elapsedMilliseconds;
    } else {
        startTime = new Date().getTime();
        timerTickInterval = setInterval(timerTick, 1000);
    }
    document.getElementById('pauseButton').disabled = suspended;
    document.getElementById('resumeButton').disabled = !suspended;
    if (difficulty === 'hard') {
        document.getElementById('hintButton').disabled = true;
    } else {
        document.getElementById('hintButton').disabled = suspended;
    }
}

function pauseGame() {

    initGrid(true);
    setGameSuspended(true);

}

function resumeGame() {

    initGrid();
    setGameSuspended(false);
}

function newGame() {
    document.getElementById('timer').innerText = '00:00:00';
    document.getElementById('gameDesc').innerText = '';

    setGameSuspended(true);
    document.getElementById('resumeButton').disabled = true;

    // this will just fill it with ?s
    completedArray = userArray = initialPuzzleArray = [];
    initGrid();

    document.getElementById('newGameDialog').showModal();
}

async function newGameSelected(e) {
    document.getElementById('newGameButton').disabled = true;
    document.getElementById('gameDesc').innerText = ` - INITIALIZING`;
    difficulty = e.target.value;
    if (!difficulty)
        difficulty = e.target.parentElement.value;
    // can you beleve this?  if you click on the h2 within the button e.target is set to the h2

    

    // Create new puzzle
    let diffblanks;
    switch (difficulty) {
        case 'easy':
            diffblanks = 35;
            break;
        case 'medium':
            diffblanks = 45;
            break;
        case 'hard':
            diffblanks = 60;
            break;
    }
    const result = await makePuzzle(diffblanks);
    initialPuzzleArray = result.initialPuzzleArray;
    userArray = [...initialPuzzleArray]; // clone array
    completedArray = result.completedArray;

    document.getElementById('gameDesc').innerText = ` - ${difficulty}`;
    document.getElementById('newGameButton').disabled = false;
    initGrid();

    startTime = new Date().getTime();
    totalTime = 0;

    setGameSuspended(false);

}

function hint() {
    // what is a hint?  Could be lots of things
    // if the user has made mistakes, mark them
    // if not, propose a new value

    // we'll focus on marking wrong but prepare to propose a new value
    // this list will serve to indicate all available blank spaces in the current user array
    let currentBlanks = []
    let markedWrong = false;
    for (let nid = 0; nid < 81; nid++) {
        // if they've put in a value, but its the wrong value
        if (userArray[nid] && completedArray[nid] !== userArray[nid]) {
            document.querySelector(`button[data-id='${nid}']`).className = 'hint-incorrect';
            markedWrong = true;
        } else if (!userArray[nid]) {
            currentBlanks.push(nid);
        }
    }
    if (markedWrong) return;

    const randomElement = currentBlanks[Math.floor(Math.random() * currentBlanks.length)];
    userArray[randomElement] = completedArray[randomElement];
    document.querySelector(`button[data-id='${randomElement}']`).className = 'hint-proposed';
    document.querySelector(`button[data-id='${randomElement}']`).innerText = completedArray[randomElement];

    checkCompletion(); // in case its the last loc

}

function initUI() {

    document.querySelectorAll('#chooseNumberDialog button').forEach(b => b.addEventListener('click', chooserBtnClick));

    // this allows the user to use the keyboard to enter a digit
    // NOTE: in easy, disabled buttons value can still be selected this way
    // to fix?  Maybe
    document.getElementById('chooseNumberDialog').addEventListener('keydown', e => {
        if (e.key >= '0' && e.key <= '9') {
            assignButtonValue(e.key);
            document.getElementById('chooseNumberDialog').close();
        }
    });

    // wire up new, pause, resume
    document.getElementById('pauseButton').addEventListener('click', pauseGame);
    document.getElementById('resumeButton').addEventListener('click', resumeGame);
    document.getElementById('newGameButton').addEventListener('click', newGame);
    document.getElementById('hintButton').addEventListener('click', hint);

    // within the new dialog, wire up the buttons
    document.querySelectorAll('#newGameDialog button.difficulty').forEach(b => b.addEventListener('click', newGameSelected));

    // this allows the user to click outside a dialog to cancel/close the dialog
    document.querySelectorAll('dialog').forEach(b => {
        b.addEventListener("click", (e) => {
            if (e.target === b) {
                b.close();
            }
        })
    });

    // this will just fill it with ?s
    completedArray = userArray = initialPuzzleArray = [];
    initGrid();


}

