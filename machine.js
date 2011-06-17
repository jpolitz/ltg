var id = function(x) { return x; }

var Machine = function(maxSlots, maxVitality, initVitality) {
    var initSlots = function(slots) { 
        for(var i = 0; i < maxSlots; i++) {
            slots[i] = {field : id,
                        vitality : initVitality};
        }
    }
    this.maxSlots = maxSlots;
    if(maxVitality < initVitality) {
        throw "maxVitality should not be smaller than the init";
    }
    this.maxVitality = maxVitality;
    this.initVitality = initVitality;
    this.player0Slots = [];
    this.player1Slots = [];
    initSlots(this.player0Slots);
    initSlots(this.player1Slots);
    this.slots = [this.player0Slots, this.player1Slots];
    this.proponent = 0;
    this.zombie = false;
}

Machine.prototype.validSlot = function(num) {
    if(typeof num !== "number") { return false; }
    return num >= 0 && num <= this.maxSlots;
}

Machine.prototype.deadField = function(num) {
    if(typeof num !== "number") { return false; }
    return num <= 0;
}

Machine.prototype.deadSlot = function(slot) {
    if(slot && slot.field) {
        return this.deadField(slot.field);
    }
    return false;
}

Machine.prototype.getProponentSlot = function(i) {
    if(!this.validSlot(i)) {
        throw "Bad slot given to getProponentSlot";
    }
    return this.slots[this.proponent][i];
}

Machine.prototype.getProponentSlotOpposite = function(i) {
    if(!this.validSlot(i)) {
        throw "Bad slot given to getProponentSlotOpposite";
    }
    return this.getProponentSlot(machine.maxSlots - i);
}

Machine.prototype.getOpponentSlot = function(i) {
    if(!this.validSlot(i)) {
        throw "Bad slot given to getOpponentSlot";
    }
    return this.slots[1 - this.proponent][i];
}

Machine.prototype.getOpponentSlotOpposite = function(i) {
    if(!this.validSlot(i)) {
        throw "Bad slot given to getOpponentSlotOpposite";
    }
    return this.getOpponentSlot(machine.maxSlots - i);
}

Machine.prototype.endTurn = function() { 
    this.proponent = 1 - this.proponent;
}

Machine.prototype.printInfo = function() {
    print("Slots: " + this.maxSlots);
    print("Starting vita: " + this.initVitality);
    print("Max vita: " + this.maxVitality);
}

Machine.prototype.printSlots = function(slots) {
    var s = "";
    function fld(fld) { 
        if (typeof fld === "number") {
            return String(fld);
        }
        if (fld === id) {
            return "id";
        }
        if (!fld.card) { throw "not a card!"; }
        return fld.card;
    }
            
    for(var i = 0; i < slots.length; i++) {
        s += "[(" + i + ") " + fld(slots[i].field) + "; " + slots[i].vitality + "]"
    }
    print(s);
}

Machine.prototype.print = function() {
    this.printInfo();
    this.printSlots(this.player0Slots);
    this.printSlots(this.player1Slots);
}

Machine.prototype.leftApply = function(slot, card) {
    try {
        var slot = this.getProponentSlot(slot);
        var result = card.make(this)(slot.field);
        slot.field = result;
    }
    catch(e) {
        if(e.tag === "interp") {
            
        }
        else {
            print("Non-interp error");
            print(e);
        }
    }
}

Machine.prototype.rightApply = function(slot, card) {
    try {
        var slot = this.getProponentSlot(slot);
        var result = slot.field(card.make(this));
        slot.field = result;
    }
    catch(e) {
        if(e.tag === "interp") {
            
        }
        else {
            print("Non-interp error");
            print(e);
        }
    }
}

Machine.prototype.move = function(slot, card, left) {
    if(!this.validSlot(slot)) {
        throw "Bad slot";
    }
    if(left) {
        this.leftApply(slot, card);
    }
    else {
        this.rightApply(slot, card);
    }
    this.endTurn();
}
