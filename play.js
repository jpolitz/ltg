

function turn(machine) {
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

    print("Type L for left apply, something else for right apply");
    var left = this.readline() === "L";
    print(left);
    var slot = getSlot();
    var card = getCard();
}

function play(m) {
    while(true) { 
        m.print();
        turn(m); 
    }
}
