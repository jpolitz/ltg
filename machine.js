var id = function(x) { return x; }

var Machine = function(maxSlots, maxVitality, initVitality) {
    var initSlots = function(slots) { 
        for(var i = 0; i < maxSlots; i++) {
            slots[i] = {field : id,
                        vitality : initVitality};
        }
    }
    this.maxSlots = maxSlots;
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
