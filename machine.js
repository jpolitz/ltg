var Machine = function(maxSlots, maxVitality, initVitality) {
    this.maxSlots = maxSlots;
    this.maxVitality = maxVitality;
    this.initVitality = initVitality;
    this.player1Slots = [];
    this.player2Slots = [];
}

Machine.prototype.validSlot(num) {
    return num >= 0 && num < this.maxSlots;
}

Machine.prototype.deadField(num) {
    return num <= 0;
}

