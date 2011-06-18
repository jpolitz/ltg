load("machine.js");
load("cards.js");

if (typeof printstderr == 'undefined') {
    printstderr = function() { };
}

// Harness for running Machine against the public interface
// per http://www.icfpcontest.org/2011/06/task-description-contest-starts-now.html

var MAX_SLOTS = 5;     // TODO: set to 256 for real play.
var MAX_VITALITY = 65535;
var INIT_VITALITY = 10; // TODO: set to 10000

var LEFT_APPLY = '1';
var RIGHT_APPLY = '2';

function TournamentMachine() {
    this.machine = new Machine(MAX_SLOTS, MAX_VITALITY, INIT_VITALITY);
}

TournamentMachine.prototype = {
    // Returns [slot, card, action]
    decide: function() {
        // Fancy-pants AI goes here.
        return ['0', 'I', LEFT_APPLY];
    },

    getCard: function(cardName) {
        if(cardName in CARDS) {
            return CARDS[cardName];
        }
        throw {'tag': 'Error', 'message': 'Bad card name', 'name': cardName};
    },

    atoi: function(N) {
        var n = parseInt(N);
        if (n == n) {
            return n;
        } else {
            throw "Bad int " + N;
        }
    },

    playMyTurn: function() {
        var move = this.decide();
        var slot = this.atoi(move[0]);
        var card = move[1];
        var action = move[2];

        print(action);
        print(card);
        print(slot);

        this.machine.move(slot, this.getCard(card), action == LEFT_APPLY);
        this.machine.endTurn();
    },

    playOpponentTurn: function() {
        var opponentAction = readline();
        var opponentCard = readline();
        var opponentSlot = this.atoi(readline());

        this.machine.move(opponentSlot, this.getCard(opponentCard), opponentAction == LEFT_APPLY);
        this.machine.endTurn();

        // Debuggery.  TODO: Remove.
        this.machine.print(printstderr);
    },

    run: function(playerNumber) {
        if (playerNumber != 0) {
            this.playOpponentTurn();
        }

        while (true) {
            this.playMyTurn();
            this.playOpponentTurn();
        }
    }
};

(new TournamentMachine()).run(0);
