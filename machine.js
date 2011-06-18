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
    return this.getProponentSlot(this.maxSlots - i - 1);
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
    return this.getOpponentSlot(this.maxSlots - i - 1);
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
        if (typeof fld === 'undefined') {
            throw "fld was undefined";
        }
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
    var slot = this.getProponentSlot(slot);
    try {
        var fn = card.make(this);
        if(typeof fn !== 'function') {
            slot.field = id;
            return;
        }
        var result = card.make(this)(slot.field);
        slot.field = result;
    }
    catch(e) {
        if(e.tag === "interp") {
            print("Interp error: " + e.extra);
            slot.field = id;
        }
        else {
            print("Non-interp error " + e);
        }
    }
}

Machine.prototype.rightApply = function(slot, card) {
    var slot = this.getProponentSlot(slot);
    try {
        var fn = slot.field;
        if(typeof fn !== 'function') {
            slot.field = id;
            return;
        }
        var result = slot.field(card.make(this));
        slot.field = result;
    }
    catch(e) {
        if(e.tag === "interp") {
            print("Interp error: " + e.extra);
            slot.field = id;
        }
        else {
            print("Non-interp error: " + e);
        }
    }
}

Machine.prototype.zombieMoves = function() {
    this.zombie = true;
    for(var i = 0; i < this.slots[this.proponent].length; i++) {
        var slot = this.slots[this.proponent][i];
        if(slot.vitality === -1) {
            slot.field(id);
            slot.field = id;
        }
    }
    this.zombie = false;
}

Machine.prototype.move = function(slot, card, left) {
    if(!this.validSlot(slot)) {
        throw "Bad slot: " + slot;
    }
    this.zombieMoves();
    if(left) {
        this.leftApply(slot, card);
    }
    else {
        this.rightApply(slot, card);
    }
}

Machine.prototype.turn = function(slot, card, left) {
    this.move(slot, card, turn);
    this.endTurn();
}
