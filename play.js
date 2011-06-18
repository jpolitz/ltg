function turn(machine) {
    print("---------------------------------------------");
    print("It is player" + machine.proponent + "'s turn.");
    function getSlot() {
        print("Give a slot #");
        var slot = Number(this.readline());
        if(!machine.validSlot(slot)) {
            print("Bad slot, try again");
            getSlot();
        }
        return slot;
    }

    function getCard() {
        print("Give a card name: ");
        var cardName = readline();
        if(cardName in CARDS) {
            return CARDS[cardName];
        }
        print("Bad card name: " + cardName);
        getCard();
    }

    print("Type L for left apply, something else for right apply (or quit)");
    var leftStr = this.readline();
    if(leftStr === "quit") { quit(); }
    var left = leftStr === "L";
    var slot = getSlot();
    var card = getCard();
    
    machine.move(slot, card, left);
}

function play(m) {
    while(true) { 
        m.print();
        turn(m); 
    }
}
