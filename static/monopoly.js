var playerName = new URLSearchParams(window.location.search).get('name');

var stateObj = null;

function startLoop() {
    var interv = setInterval(async () => {
        try {
            await refresh();
        } catch (e) {
            clearInterval(interv);
        }
    }, 200);
}



async function refresh() {
    var state = await getStateCall();
    stateObj = JSON.parse(state);

    render(stateObj);
}

function render(state) {
    let playerElement = document.getElementById("player");
    playerElement.innerText = "Welcome " + playerName;

    document.getElementById("economy").innerText = state.economy;
    renderButtons(state);
    renderPlayers(state);
    renderBoard(state);
    renderLogs(state);
}

function renderButtons(state) {
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
    if(state.state.type == "BuyOrNot") {
        if(state.state.player == getMyIndex( state)) {
            enableButton("dontbuyButton");
            enableButton("buyCashButton");
            enableButton("buyBorrowButton");
        }
    }
}

function renderPlayers(state) {
    let playersTable = document.getElementById("playersTable");
    playersTable.innerText = "";

    var header = playersTable.createTHead();
    header = header.insertRow(0);
    header.insertCell(-1).innerText = "";
    header.insertCell(-1).innerText = "Money";
    header.insertCell(-1).innerText = "Assets";
    header.insertCell(-1).innerText = "Debt";
    header.insertCell(-1).innerText = "Start money";

    state.players.forEach((player, pIdx) => {
        var row = playersTable.insertRow(-1);
        row.insertCell(-1).innerHTML = getPlayerName(player, pIdx);
        row.insertCell(-1).innerText = player.money;
        row.insertCell(-1).innerText = player.assets;
        row.insertCell(-1).innerText = player.debt;
        row.insertCell(-1).innerText = player.startMoney;
    });
}

function renderBoard(state) {
    let groundTable = document.getElementById("groundTable");
    groundTable.innerText = "";

    state.grounds.forEach((ground, index) => {
        var row = groundTable.insertRow(-1);
        var cell1 = row.insertCell(-1);
        var cell2 = row.insertCell(-1);
        var cell3 = row.insertCell(-1);
        var cell4 = row.insertCell(-1);

        cell1.innerHTML = getOwnerName(state, ground.owner);
        cell1.classList.add("owner");
        cell2.innerHTML = ground.name;
        cell2.classList.add("ground-" + ground.color);
        cell3.innerHTML = ground.value || "";
        cell3.classList.add("amount");
        cell4.innerHTML = getPlayerOnGround(state, index);
    });
}

function renderLogs(state) {
    var logs = state.logs.join("<br>");
    state.players.forEach((player, idx) => {
        var re = new RegExp(player.name, "g");
        logs = logs.replace(re, "<span class=\"player" + idx + "\">" + player.name + "</span>")
    });
    document.getElementById("log").innerHTML = logs;
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

    postAndRefresh("/game/roll", {rollPlayer: getMyIndex(stateObj), roll: roll});
}

function dontBuy() {
    postAndRefresh("/game/dont-buy", {player: getMyIndex(stateObj)});
}
function buyCash() {
    postAndRefresh("/game/buy-cash", {player: getMyIndex(stateObj)});
}
function buyBorrow() {
    postAndRefresh("/game/buy-borrow", {player: getMyIndex(stateObj)});
}

function payBackDebt() {
    let asString = document.getElementById("payBackAmount").value;
    let asInt = parseInt(asString);

    if(isNaN(asInt)) {
        return;
    }

    document.getElementById("payBackAmount").value = "";

    postAndRefresh("/game/pay-back-debt", {payBackPlayer: getMyIndex(stateObj), payBackAmount: asInt})
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

    return getPlayerName(state.players[idx], idx);
}


function getPlayerOnGround(state, idx) {
    var names = "";
    state.players.forEach((player, pIdx) => {
       if(player.position == idx) {
           names += getPlayerName(player, pIdx) + " ";
       }
    });

    return names;
}

function getPlayerName(player, idx) {
    var name = player.name;
    return "<span class=\"player" + idx + "\">" + name + "</span>";
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