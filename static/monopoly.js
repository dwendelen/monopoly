var playerName = new URLSearchParams(window.location.search).get('name');

var stateObj = null;

var interv = setInterval(async () => {
    try {
        await refresh();
    } catch (e) {
        clearInterval(interv);
    }
}, 200);

async function refresh() {
    var state = await getStateCall();
    stateObj = JSON.parse(state);
    console.log(stateObj);

    render(stateObj);
}

function render(state) {
    let playerElement = document.getElementById("player");
    playerElement.innerText = "Welcome " + playerName;

    let groundTable = document.getElementById("groundTable");
    groundTable.innerText = "";


    state.grounds.forEach((ground, index) => {
        var row = groundTable.insertRow(-1);
        var cell1 = row.insertCell(-1);
        var cell2 = row.insertCell(-1);
        var cell3 = row.insertCell(-1);
        var cell4 = row.insertCell(-1);

        cell1.innerHTML = getOwnerName(state, ground.owner);
        cell2.innerHTML = ground.name;
        cell3.innerHTML = ground.value || "";
        cell4.innerHTML = getPlayerOnGround(state, index);
    });

    cleanButton("startGameButton");
    cleanButton("rollButton");
    cleanButton("dontbuyButton");
    cleanButton("buyCashButton");
    cleanButton("buyBorrowButton");

    if(state.state.type == "AddPlayers") {
        enableButton("startGameButton");
    }
    if(state.state.type == "DiceRoll") {
        if(state.state.player == getMyIndex(state)) {
            enableButton("rollButton");
        }
    }
}

function cleanButton(id) {
    document.getElementById(id).classList.add("hidden");
}

function enableButton(id) {
    document.getElementById(id).classList.remove("hidden");
}

function startGame() {
    postAndRefresh("/game/startGame", {player: getMyIndex(stateObj), roll: roll});
}

function roll() {
    var dice1 = Math.floor(Math.random() * 6) + 1;
    var dice2 = Math.floor(Math.random() * 6) + 1;

    var roll = dice1 + dice2;

    postAndRefresh("/game/roll", {player: getMyIndex(stateObj), roll: roll});
}

function postAndRefresh(url, data) {
    var xhttp = new XMLHttpRequest();
    xhttp.open("POST", url, true);
    xhttp.onreadystatechange = function() {
        if (this.readyState == 4) {
            if (this.status == 200) {
                refresh()
            } else {
                alert("Some kind of REST call went wrong, reopen game");
            }
        }
    };

    if(data) {
        xhttp.send(JSON.stringify(data));
    } else {
        xhttp.send();
    }
}

function getOwnerName(state, idx) {
    if(idx == null) {
        return "";
    }

    return getPlayerName(state.players[idx]);
}


function getPlayerOnGround(state, idx) {
    var names = "";
    state.players.forEach(player => {
       if(player.position == idx) {
           names += getPlayerName(player) + "; ";
       }
    });

    return names;
}

function getPlayerName(player) {
    var name = player.name;
    if(name == playerName) {
        name += "***";
    }
    return name;
}

function getMyIndex(state) {
    var res = null;
    state.players.forEach((player, idx) => {
       if(player.name == playerName) {
           res = idx;
       }
    });
    return res;
}

function getStateCall() {
    return new Promise(function (acc, rej) {
        var xhttp = new XMLHttpRequest();
        xhttp.onreadystatechange = function() {
            if (this.readyState == 4) {
                if (this.status == 200) {
                    acc(xhttp.responseText)
                } else {
                    alert("Some kind of REST call went wrong, reopen game");
                    rej("stuff went wrong");
                }
            }
        };
        xhttp.open("GET", "/state", true);
        xhttp.send();
    });
}