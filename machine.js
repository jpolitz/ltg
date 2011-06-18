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
    this.calls = 0;
    this.callLimit = 1000;
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

Machine.prototype.call = function(f, arg) {
    if(this.calls > this.callLimit) {
        throw {tag: "interp", extra: "Call limit reached"};
    }
    this.calls += 1;
    return f(arg);
}

Machine.prototype.printInfo = function(printFn) {
    printFn("Slots: " + this.maxSlots);
    printFn("Starting vita: " + this.initVitality);
    printFn("Max vita: " + this.maxVitality);
}

Machine.prototype.printSlots = function(printFn, slots) {
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
        if (!fld.card) { 
            print(fld);
            print(fld.card);
            throw "not a card!"; 
        }
        return fld.card;
    }
            
    for(var i = 0; i < slots.length; i++) {
        s += "[(" + i + ") " + fld(slots[i].field) + "; " + slots[i].vitality + "]";
    }
    printFn(s);
}

Machine.prototype.print = function(printFn) {
    printFn = printFn || print;
    this.printInfo(printFn);
    this.printSlots(printFn, this.player0Slots);
    this.printSlots(printFn, this.player1Slots);
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
    this.calls = 0;
}

Machine.prototype.turn = function(slot, card, left) {
    this.move(slot, card, turn);
    this.endTurn();
}
